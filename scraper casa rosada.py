# -*- coding: utf-8 -*-
"""
Created on Tue Feb 28 12:33:11 2023

@author: ian bounos
"""

import requests
from bs4 import BeautifulSoup
import re
import time
import pandas as pd


def limpiar(html):
  expresion = re.compile("<.*?>")
  texto = re.sub(expresion,"",html)
  return(texto)

listalinks = set()
j = 0
for k in range(12,40):
  print(str(40*k))
  pagefuente = requests.get("https://www.casarosada.gob.ar/informacion/discursos?start="+str(40*k))
  soupfuente = BeautifulSoup(pagefuente.text,"html.parser")


  for link in soupfuente.find_all("a"):
      if "/informacion/discursos/" in link.get('href'):
        if "mauricio-macri" in link.get('href'):
          listalinks.add("https://www.casarosada.gob.ar/"+link.get('href'))
          print("la lista va por"+str(j))
          j = j+1



lista_fechas =[]
lista_discursos = []
j = 0
for pagina in listalinks:
  print(j)
  page = requests.get(pagina)
  soup = BeautifulSoup(page.text,"html.parser")
  souptext = soup.find_all('p')
  fecha = limpiar(str(soup.find("time",class_="pull-right")))
  anio_regex = r"\d{4}"
  fecha = re.findall(anio_regex, fecha)[0]  
  discurso = limpiar(str(souptext))
  discurso = discurso.replace("[\xa0,","")
  discurso = discurso.replace("\xa0, \n]","")
  ocurrencias_and = discurso.count("and")
  ocurrencias_the = discurso.count("the")
  total_ocurrencias = ocurrencias_and + ocurrencias_the
  if total_ocurrencias > 10:
    titulo = "macri-"+str(fecha)+"-m"+str(j)+".txt"
    titulo = titulo.replace(" ","")
    try:
        archivo = open(titulo, "w")
        archivo.write(discurso)
        archivo.close()
    except UnicodeEncodeError:
        pass
  j+=1