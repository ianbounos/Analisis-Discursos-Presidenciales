
library(tm)
library(NMF)
library(Matrix)
library(data.table)
library(tidyr)
library(fastmatch)
library(gutenbergr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(RcppML)
library(SnowballC)
library(plot.matrix)
library(RColorBrewer)

tidy_text <- readRDS("C:/Users/ian bounos/OneDrive/Escritorio/DISCURSOS CRISTINA/datamining/tidy_autores2.RDS")
tidy_text  = tidy_text%>% filter(!term %in% c("mauricio", "cristina","cfk", "alberto","fernández","kirchner","macri"))%>% filter(año>2007)
### numero es el indicador del discurso.

##### Analisis exploratorio



View(tidy_text %>% group_by(autor,año,numero) %>%summarise(indic=1)%>% group_by(autor)%>%summarise(sum(indic)))

View(tidy_text %>% group_by(autor,año,numero) %>%summarise(indic=1)%>% group_by(autor,año)%>%summarise(sum(indic)))



# Gráfico 1

plot1 <- tidy_text %>% filter(autor=="cristina")%>%
  count(term_lematizado, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(term_lematizado, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
plot1



frequency <- tidy_text%>% 
  # mutate(word = str_extract(word, "[a-z']+")) %>%
  count(autor, term_lematizado) %>%
  group_by(autor) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = autor, values_from = proportion)



ggplot(frequency, aes(x = alberto, y = cristina)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_point(alpha = 0.1, size = 2.5) +
  geom_text(aes(label =term_lematizado), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position="none")

ggplot(frequency, aes(x = alberto, y = cristina)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_point(alpha = 0.1, size = 2.5) +
  geom_text(aes(label = term_lematizado, color = term_lematizado), 
            check_overlap = TRUE, vjust = 1.5) +  # Añadido color en geom_text
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_manual(values = c("vacuna" = "red",  # Asignar colores a las palabras
                                "aplausos" = "red",
                                "virus" = "red",
                                "enorme" = "red",
                                "contagios" = "red"
                                )) +
  theme(legend.position = "none")



frequency <- tidy_text%>% filter(año>2019)%>%
  # mutate(word = str_extract(word, "[a-z']+")) %>%
  count(autor, term_lematizado) %>%
  group_by(autor) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = autor, values_from = proportion)



ggplot(frequency, aes(x = alberto, y = cristina)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_point(alpha = 0.1, size = 2.5) +
  geom_text(aes(label =term_lematizado), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position="none")




#### Primero  haremos uno para el año y otro para cada texto



tidy_text_año_groupped = tidy_text%>% filter(año>2006)%>%
  group_by(año, term_lematizado) %>% 
  summarize(count = sum(count))



tidy_text_numero_groupped = tidy_text%>%filter(año>2006)%>%
  arrange(numero)%>% 
  group_by(numero, term_lematizado) %>% 
  summarize(count = sum(count))

# Crear tabla con información de año y autor
info_autor_anio <- tidy_text %>%
  distinct(numero, .keep_all = TRUE) %>%  # Seleccionar una única fila por número
  select(numero, autor, año)  %>%arrange(numero)            # Seleccionar columnas necesarias


### Defino funcion  # Utilizar la función para eliminar columnas con más del 95% de no ceros
### Esto sirve sobre todo si no se usa matriz TF-IDF


eliminar_columnas <- function(matriz, umbral){
  columna_ceros <- apply(matriz, 2, function(x) sum(x == 0)/length(x))
  columna_eliminar <- columna_ceros < umbral
  matriz[, !columna_eliminar]
}

principales_palabras <- function(n_pal,H){
  palabras= colnames(H)
  top10_cols <- matrix(NA, nrow = nrow(H), ncol = n_pal)
  rango = nrow(H)
  # Iterar sobre todas las filas de la matriz H
  for (i in 1:nrow(H)) {
    # Obtener la fila i de la matriz H
    hi <- H[i, ]
    # Ordenar los valores absolutos de la fila i de mayor a menor
    hi_abs <- sort(abs(hi), decreasing = TRUE)
    # Seleccionar los 10 valores absolutos más grandes
    top10 <- hi_abs[1:n_pal]
    # Obtener los índices de las columnas correspondientes a los 10 valores más grandes
    top10_idx <- order(abs(hi), decreasing = TRUE)[1:n_pal]
    # Almacenar los índices de las columnas correspondientes a los 10 valores más grandes
    top10_cols[i, ] <- top10_idx
  } 
  
  # Imprimir los resultados
  top10_cols
  
  palabras_por_topico = data.frame("V1" = 1:n_pal)
  for(k in 1:rango){
    palabras_por_topico[,k]= palabras[top10_cols[k,]]
  }
  return(palabras_por_topico)
  
}


NMF_Implementacion  = function( A ,rango,porc = 0 , l1=c(0,0)  ){ 
  
  ### observemos que normaliza filas
  #porc = cantidad palabras que quedan afuera 

  palabras= A$dimnames
  palabras = palabras$Terms
  A <-(as.matrix(A))
  
  A <-  apply(A, 1, function(x) x/sum(x))
  A=t(A)
  rownames(A)
  A = A[order(as.numeric(rownames(A)),decreasing=FALSE),    ]
  A <- eliminar_columnas(A,porc) 
  
  model = RcppML::nmf(A,rango, verbose = F,L1 = l1)
  W <- model$w
  rownames(W)=rownames(A)
  D <- model$d
  H <- model$h
  colnames(H) = colnames(A)
  
  nmf_res = list()
  nmf_res$W = W 
  nmf_res$H = H
  nmf_res$palabras = palabras
  nmf_res$matriznormalizada = A
return(nmf_res)
 }



A1 <- tidy_text_año_groupped%>%
  cast_dtm(año, term_lematizado, count)

### para hacer la versión tf Idf
#A = weightTfIdf(A1)
A = A1

ncol(A)
nrow(A)


### Ver particularmente caso rango = 5 y ver como se rompe todo con porc distinto de 0

set.seed(087)
k = 3
nmf_res = NMF_Implementacion((A),rango=k,porc = 0.0) 
W = nmf_res$W
H = nmf_res$H

print(principales_palabras(20,H))
par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(W,col=brewer.pal(9,"Blues"),main="",ylab="",xlab="")
plot(log(W),col=brewer.pal(9,"Blues"),main="",ylab="",xlab="")




for(k in 2:10){ 
nmf_res = NMF_Implementacion((A),rango=k,porc = 0.0) 
W = nmf_res$W
H = nmf_res$H

print(principales_palabras(20,H))
par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(W,col=brewer.pal(9,"Blues"),main="",ylab="",xlab="")
plot(log(W),col=brewer.pal(9,"Blues"),main=paste("log",k),ylab="")
}




x = c()
y= c()
### Graficamos para cada año que porcentaje "explica" el primer tópico
for(rango in 2:10){ 
  model = RcppML::nmf(A,rango, verbose = F,L1 =c(0,0))
  
  W <- model$w
  rownames(W)=rownames(A)
  D <- model$d
  H <- model$h
  colnames(H) = colnames(A)

  print(  principales_palabras(10,H))
  max_col <- apply(W, 1, which.max)
  print(max_col)
  diff_perc <- apply(W, 1, function(x) {
    max_val <- max(x)
    sum_val <- sum(x)
    (max_val) / sum_val
  })
  años = sort(as.numeric(rownames(A)),decreasing=FALSE)
  plot(años,diff_perc[order(años,decreasing = FALSE)],type="l",main=paste("cant topicos ",rango),xlab=sum(diff_perc)/length(diff_perc)
  )
  
  x= c(x,rango)
  y = c( y, median(diff_perc))
}

#### para cada rango graficamos la mediana
plot(x,y,type="l")





###### Mapa de correlaciones matriz W. para un rango dado
rango = 3
par(mar = c(2,2,2,2))
par(mfrow=c(1,1))
for(k in 1:length(as.numeric(rownames(A)) )){ 
  
  nmf_res = NMF_Implementacion(A,rango=rango,porc = 0.2) 
  W = nmf_res$W
  H = nmf_res$H
  
  plot(as.numeric(rownames(A)) ,cor(t( W ))[,k],main=rownames(A)[k],type = "l",xlab="",ylab="")
  abline( v=as.numeric(rownames(A))[k] )
}

plot(as.numeric(rownames(W)),max.col(W,"first"),type="h",lwd=5,xlab="",ylab="cluster",lend=1)

par(mfrow=c(1,1))
################## 














######## Ahora vamos a hacer un análisis documento a documento


A2 <- tidy_text_numero_groupped%>%
  cast_dtm(numero, term_lematizado, count)
A = A2 
A=weightTfIdf(A)

nrow(A)
ncol(A)


##### corremos el proceso
tasa = c()
for(m in 1:8){ 

set.seed(087)
rango = m

nmf_res = NMF_Implementacion(A,rango=rango) 
W = nmf_res$W
H = nmf_res$H


#### Que topico es mayor por cada autor


tabla_autores = table(info_autor_anio$autor,max.col(W, "first"))
par(mar = c(4,4,4,4))
colores_autores <- c(2,3,4)
nombres_autores <- rownames(tabla_autores)
colores <- setNames(colores_autores[1:length(nombres_autores)], nombres_autores)
barplot(tabla_autores, col = colores, beside = TRUE,
        ylab = "Cantidad", xlab = "Tópico")
legend("topright", legend = nombres_autores, fill = colores, cex = 0.8)
      

principales_palabras(10,H)



########### Veamos como clasifica esto
#### Probarlo con rango 10 y mostrar que clasifica bien 


library(nnet)
### Hacemos cross validation con  5 folds y vemos que clasifica bien

 
datos = data.frame( Y= info_autor_anio$autor,W,año= info_autor_anio$año )


ind <- sample(2, nrow(datos), replace = TRUE, prob = c(0.8, 0.2))

ind <- sample(1:nrow(datos),nrow(datos),replace = FALSE)
indices = list()
indices[[1]]=ind[1:trunc(nrow(datos)/5)]
indices[[2]]=ind[(trunc(nrow(datos)/5)+1):(trunc(2*nrow(datos)/5))]
indices[[3]]=ind[(trunc(2*nrow(datos)/5)+1):(trunc(3*nrow(datos)/5))]
indices[[4]]=ind[(trunc(3*nrow(datos)/5)+1):(trunc(4*nrow(datos)/5))]
indices[[5]]=ind[(trunc(4*nrow(datos)/5)+1):nrow(datos)]

confusion=0
confusion_mas_2015 = 0
for(j in 1:5){
train <- datos[-indices[[j]], ]
test <- datos[indices[[j]], ]

modelo <- multinom(Y ~ .-año, data = train)
predicciones <- predict(modelo, newdata = test, type = "class")
precision <- mean(test$Y == predicciones)
precision_mas_2015 <- mean((test$Y == predicciones)[test$año>2015])

confusion <- confusion + table(test$Y, predicciones)
confusion_mas_2015 <- confusion_mas_2015 + table(test$Y[test$año>2015], predicciones[test$año>2015])
} 

confusion
tasa_aciertos = (confusion[1,1]+confusion[2,2]+confusion[3,3])/sum(confusion)
print(tasa_aciertos)
tasa = c(tasa,tasa_aciertos)
}

grafico_tasa_aciertos = data.frame(Topicos = 1:8,Tasa= tasa)
ggplot(data = grafico_tasa_aciertos)+
  geom_line(aes(x = Topicos,y=Tasa)  ) +
  geom_point( x=5,y=grafico_tasa_aciertos$Tasa[5] ,color="blue")

####################################################
################### PCA ##############################
# con 3 funciona horrible. despues con muchas componentes funciona perfecto
######################################################

# Hacemos PCA y guardamos el resultado porque tarda mucho en correr
Ascaled = scale(A)
# 
pca <- prcomp(Ascaled)
## Tarda en correr. Si uno quiere guardarlo:
#saveRDS(pca, "resultadosPCA.RDS")



# Explorar resultados

summary(pca)
plot(pca)

k=2 ### k es que componente graficamos en el eje x y j cual en el y
j= 3
df = data.frame(X1 = pca$x[,k],X2 = pca$x[,j],autor = info_autor_anio$autor)

# Agregar puntos al gráfico
# Crear objeto ggplot con transparencia
ggplot(df, aes(x = X1, y = X2, color = autor)) +
  geom_point(alpha = 0.5)+
 labs(
         x = "PC2", y = "PC3") + 
  theme_minimal()




tasa = c()
lista_confusion = list()
for(m in 1:15){ 
  set.seed(087)
  datos = data.frame( Y= info_autor_anio$autor,pca$x[,1:m])
  ind <- sample(2, nrow(datos), replace = TRUE, prob = c(0.8, 0.2))
  
  ind <- sample(1:nrow(datos),nrow(datos),replace = FALSE)
  indices = list()
  indices[[1]]=ind[1:trunc(nrow(datos)/5)]
  indices[[2]]=ind[(trunc(nrow(datos)/5)+1):(trunc(2*nrow(datos)/5))]
  indices[[3]]=ind[(trunc(2*nrow(datos)/5)+1):(trunc(3*nrow(datos)/5))]
  indices[[4]]=ind[(trunc(3*nrow(datos)/5)+1):(trunc(4*nrow(datos)/5))]
  indices[[5]]=ind[(trunc(4*nrow(datos)/5)+1):nrow(datos)]
  
  confusion=0
  confusion_mas_2015 = 0
  for(j in 1:5){
    train <- datos[-indices[[j]], ]
    test <- datos[indices[[j]], ]
    
    modelo <- multinom(Y ~ ., data = train)
    predicciones <- predict(modelo, newdata = test, type = "class")
    precision <- mean(test$Y == predicciones)
    precision_mas_2015 <- mean((test$Y == predicciones)[test$año>2015])
    
    confusion <- confusion + table(test$Y, predicciones)
    confusion_mas_2015 <- confusion_mas_2015 + table(test$Y[test$año>2015], predicciones[test$año>2015])
  
 
    
    } 
  
  confusion
  tasa_aciertos = (confusion[1,1]+confusion[2,2]+confusion[3,3])/sum(confusion)
  print(tasa_aciertos)
  tasa = c(tasa,tasa_aciertos)
  lista_confusion[[m]]= confusion
}

t(lista_confusion[[5]])

grafico_tasa_aciertos = data.frame(Componentes = 1:15,Tasa= tasa)
ggplot(data = grafico_tasa_aciertos)+
  geom_line(aes(x = Componentes,y=Tasa)  ) 


