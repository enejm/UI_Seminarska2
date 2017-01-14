import urllib.request  # the lib that handles the url stuff
from bs4 import BeautifulSoup
import re
import time
import random
import csv
import sys


parseDict = {"Fantasy":'http://www.gutenberg.org/wiki/Fantasy_(Bookshelf)',
             "Adventure" : "http://www.gutenberg.org/wiki/Adventure_(Bookshelf)"} #"Science Fiction" : "http://www.gutenberg.org/wiki/Science_Fiction_(Bookshelf)",
vrstniRed = ["Adventure","Fantasy","Science Fiction"]
#folder = "./textsNew/"
csvFile = open('textiFiction.csv', 'a+', newline='')
#fieldnames = ['url', 'imeFajla', 'imeDela', 'imeAvtorja']
textiCsv = csv.writer(csvFile, delimiter=',', dialect="excel") #, fieldnames=fieldnames)
bookshelf = "Philosophy"
#textiCsv.writeheader()

mainFolder = "./textsOriginalNew/"
inMapa =  ""
outMapa = "./textsModifiedNew/"
TOTAL_WORDS = 400
STEVILO_ODLOMKOV = 20
def fileModifier(fn):
    inPath = inMapa + fn + ".txt"
    outPath = outMapa + fn + "_M.txt"
    #TODO odstrani zacetek in konec
    zacIdx = 100
    konIdx = 0
    with open(inPath, 'r', encoding="utf8") as inputFile:
        i = 0
        for line in inputFile.readlines():
            if bool(re.search(".*end of the project gutenberg.*", line.lower())):
                print("                    END OF PR ", konIdx )
                konIdx=i
                break
            i+=1


    #Dobimo število vseh vrstic, da vemo na katerih mestih kasneje vzamemo odlomke
   # with open(inPath, 'r', encoding="utf8") as inputFile:
    #    korakZaNaslednjiOdlomek= len(inputFile.readlines())//STEVILO_ODLOMKOV;
    korakZaNaslednjiOdlomek= (konIdx-zacIdx)//STEVILO_ODLOMKOV;
    with open(inPath, 'r', encoding="utf8") as inputFile, open(outPath, 'w+', encoding="utf8") as outFile:
        lines = inputFile.readlines()
        i = 0
        while i < STEVILO_ODLOMKOV:
            l = zacIdx+i*korakZaNaslednjiOdlomek
            wc = 0
            while wc < TOTAL_WORDS:
                line = lines[l]
                wc += len(line.split(" "))
                outFile.write(line)
                l += 1
            print("                "+ str(i) + ", " + str(wc))
            outFile.write("\n")
            i+=1

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
    outFileName = inicialke +  "_" + textName
    outFilePath = inMapa + outFileName + ".txt"

    textiCsv.writerow([url, outFileName, imeTexta, imeAvtorja, bookshelf])
    csvFile.flush()

    with open(outFilePath, 'w+', encoding="utf8") as outFile:
        outFile.write(text)

    fileModifier(outFileName)




#for folder, urlBookshelf in parseDict.items():
for folder in vrstniRed:
    bookshelf = folder
    inMapa = mainFolder + folder + '/'
    urlBookshelf = parseDict[folder]
    print(folder.upper())
    try:
        html_page = urllib.request.urlopen(urlBookshelf)
        soup = BeautifulSoup(html_page, "html.parser")
        nadaljuj = False
        for link in soup.findAll('a', attrs={'href': re.compile(".+ebooks[/].+")}):
            opis_url = "https:" + link.get('href')
            print("Knjiga:", opis_url)
            if nadaljuj:
                #time.sleep(5+random.randrange(15))
                try:
                    print(" W")
                    page_opis = urllib.request.urlopen(opis_url)
                    soupOpis = BeautifulSoup(page_opis, "html.parser")
                    nam = soupOpis.find("meta", attrs={"name":"title"})
                    title = nam["content"].rsplit(" by ", 1)
                    imeDela = title[0]
                    if len(title)>1:
                        imeAvtorja = title[1]
                    else:
                        imeAvtorja = "NN"

                    for link in soupOpis.findAll('a', attrs={'href': re.compile(".+[.]txt.*")}):
                        l = link.get('href')
                        if len(l)> 5: # and imeAvtorja.split(" ")[-1][0]!="B":
                             l = "https:" + l
                             print("   ST")
                             shraniText(l, imeDela, imeAvtorja)
                        #time.sleep(10+random.randrange(15))
                except Exception as e:
                    print("ERROR IN")
                    print(e)
                    csvFile.close()
                    sys.exit()
            if opis_url.split("/")[-1]=='421':
                print("True")
                nadaljuj = True
    except Exception as e:
        print("ERROR 1")
        print(e)
        #time.sleep(2)






csvFile.close()