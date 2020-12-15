import os,re

filecontent = ''
with open('/data/user/bio18/wch/big/gdownload/gdownload.sh') as fList:
    List = fList.readlines()
    for line in List:
        line = line.strip()
        if line[0] == 'c':
            linesplit0 = line.split(' ')
            linesplit1 = line.split('/')
            print(linesplit1)
            line = linesplit0[2].rstrip(linesplit1[-1]).rstrip('/')
            filecontent = filecontent + line + '\n'

with open('ftplist.txt','w') as fw:
    fw.write(filecontent)