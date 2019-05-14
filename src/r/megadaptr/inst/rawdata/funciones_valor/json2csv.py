from os.path import join
from os import listdir
import json
import csv

folder = "/Users/fidel/Desktop/funciones_valor"

for capa in listdir(folder):
  if capa.endswith(".json"):
    capa_csv = capa[:-4]+"csv"
    csv_path = join(folder, capa_csv)
    data = json.load(open(join(folder, capa)))
    renglones = []
    for key, value in data.iteritems():
        temp = [key,value]
        print temp
        renglones.append(temp)


    with open(csv_path, "wb") as f:
        writer = csv.writer(f)
        writer.writerows(renglones)
