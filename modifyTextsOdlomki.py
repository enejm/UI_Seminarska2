import csv
import re

inMapa = "./textsOriginal/"
outMapa = "./textsModiedOdlomki/"
TOTAL_WORDS = 400
STEVILO_ODLOMKOV = 20
vrstniRed = ["Adventure","Fantasy","Science Fiction"]

global zapStevilka
zapStevilka = 1

def fileModifier(fn, shelf):
    global zapStevilka
    inPath = inMapa + shelf + "/" + fn  + ".txt"
    #outPath = outMapa + fn  + "_M.txt"
    outPath = outMapa + str(zapStevilka).zfill(3) + ".txt"
    zapStevilka +=1


    #TODO odstrani zacetek in konec
    zacIdx = 100
    konIdx = 0
    imamoKonec = False
    with open(inPath, 'r', encoding="utf8") as inputFile:
        i = 0
        for line in inputFile.readlines():
            if (bool(re.search(".*start of .*project gutenberg.*", line.lower()))):
                zacIdx=i
                #print("                    Start OF PR ", zacIdx )
            if (not imamoKonec and bool(re.search(".*end of .*project gutenberg.*", line.lower()))):
                konIdx=i
                imamoKonec = True
                #print("                    END OF PR ", konIdx )
                #break
            i+=1
        zacIdx += 100
        if(zacIdx>500):
            print("PREVERI!!!!!!!!!")
            zacIdx = 500;
        if(not imamoKonec):
            konIdx = i
        #print("     Zac idx:", zacIdx, "Kon idx", konIdx, 'Dolzina celega texta', i)
    korakZaNaslednjiOdlomek= (konIdx-zacIdx)//STEVILO_ODLOMKOV;
    #print("       Korak", korakZaNaslednjiOdlomek)
    if (korakZaNaslednjiOdlomek < 23):
        print("PREMAJHEN KORAK ", fn, korakZaNaslednjiOdlomek, zacIdx, konIdx, "!!!!!!!!!")

    #Dobimo število vseh vrstic, da vemo na katerih mestih kasneje vzamemo odlomke
    # with open(inPath, 'r', encoding="utf8") as inputFile:
    #     korakZaNaslednjiOdlomek= len(inputFile.readlines())//STEVILO_ODLOMKOV;
    with open(inPath, 'r', encoding="utf8") as inputFile, open(outPath, 'w+', encoding="utf8") as outFile:
        lines = inputFile.readlines()
        i = 0
        while i < STEVILO_ODLOMKOV:
            odlomek = ""
            l = zacIdx + i*korakZaNaslednjiOdlomek
            wc = 0
            #print(i)
            while wc < TOTAL_WORDS:
                line = lines[l]
                wc += len(line.split(" "))
                line = line.strip()
                odlomek += line + " "
                #print("     "+ str(wc))
                l += 1
            outFile.write(odlomek+"\n")
            i+=1


with open('textiFiction.csv', newline='') as csvfile:
    texts = csv.reader(csvfile, delimiter=',', dialect="excel")
    glava = next(texts)
    for book in texts:
        ime = book[1]
        shelf = book[4]
        #print(ime, shelf)
        fileModifier(ime, shelf)


