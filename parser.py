import urllib.request  # the lib that handles the url stuff
from bs4 import BeautifulSoup
import re
import time
import random
import csv

folder = "./textsNew/"
csvFile = open('textiNew.csv', 'w+', newline='')
#fieldnames = ['url', 'imeFajla', 'imeDela', 'imeAvtorja']
textiCsv = csv.writer(csvFile, delimiter=',', dialect="excel") #, fieldnames=fieldnames)
bookshelf = "Philosophy"
#textiCsv.writeheader()

def shraniText(url, imeTexta, imeAvtorja):
    print("        ", imeTexta, imeAvtorja, url)
    data = urllib.request.urlopen(url).read() # it's a file like object and works just like a file
    text = data.decode('utf-8')
    inicialke = ""
    for i in imeAvtorja.split(" "):
        inicialke += i[0].upper()
    if len(inicialke)<1:
        inicialke = "NN"
    textName = ""
    for j in imeTexta.split():
        textName += j[0].upper()
        if len(j)>1:
            textName += j[1:]
    outFileName = inicialke +  "_" + textName + ".txt"
    outFilePath = folder + outFileName

    textiCsv.writerow([url, outFileName, imeTexta, imeAvtorja, bookshelf])
    csvFile.flush()

    with open(outFilePath, 'w+', encoding="utf8") as outFile:
        outFile.write(text)



html_page = urllib.request.urlopen("https://www.gutenberg.org/wiki/Philosophy_(Bookshelf)")
soup = BeautifulSoup(html_page, "html.parser")
for link in soup.findAll('a', attrs={'href': re.compile(".+ebooks[/].+")}):
    opis_url = "https:" + link.get('href')
    print(opis_url)
    page_opis = urllib.request.urlopen(opis_url)
    soupOpis = BeautifulSoup(page_opis, "html.parser")
    nam = soupOpis.find("meta", attrs={"name":"title"})
    title = nam["content"].rsplit(" by ", 1)
    imeDela = title[0]
    if len(title)>1:
        imeAvtorja = title[1]
    else:
        imeAvtorja = "NN"

    for link in soupOpis.findAll('a', attrs={'href': re.compile(".+[.]txt.+")}):
        l = link.get('href')
        if len(l)> 5:
             l = "https:" + l
             shraniText(l, imeDela, imeAvtorja)
        time.sleep(15+random.randrange(15))

csvFile.close()