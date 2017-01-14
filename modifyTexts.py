import csv

inMapa = "./textsOriginal/"
outMapa = "./textsModified/"
TOTAL_WORDS = 400
STEVILO_ODLOMKOV = 20
def fileModifier(fn):
    inPath = inMapa + fn  + ".txt"
    outPath = outMapa + fn  + "_M.txt"
    #TODO odstrani zacetek in konec
    with open(inPath, 'r', encoding="utf8") as inputFile:
        for line in inputFile.readline():
            if line=="":
                return 0
                #TODO


    #Dobimo število vseh vrstic, da vemo na katerih mestih kasneje vzamemo odlomke
    with open(inPath, 'r', encoding="utf8") as inputFile:
        korakZaNaslednjiOdlomek= len(inputFile.readlines())//STEVILO_ODLOMKOV;
    with open(inPath, 'r', encoding="utf8") as inputFile, open(outPath, 'w+', encoding="utf8") as outFile:
        lines = inputFile.readlines()
        i = 0
        while i < STEVILO_ODLOMKOV:
            l = i*korakZaNaslednjiOdlomek
            wc = 0
            print(i)
            while wc < TOTAL_WORDS:
                line = lines[l]
                wc += len(line.split(" "))
                outFile.write(line)
                print("     "+ str(wc))
                l += 1
            outFile.write("\n")
            i+=1


with open('texti.csv', newline='') as csvfile:
    texts = csv.reader(csvfile, delimiter=',', dialect="excel")
    glava = next(texts)
    for book in texts:
        ime = book[0]
        fileModifier(ime)


