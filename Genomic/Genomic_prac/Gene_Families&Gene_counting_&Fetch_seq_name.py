#gene_families&gene_count&fetch_seq_name
import re
import os
import sys
### cat dump.data.mci.I14 | wc -l

def Output_Each_Line(SN,Number_of_Gene):
    output = ''
    SN = str(SN)
    Number_of_Gene = str(Number_of_Gene)
    output = SN + '\t' + Number_of_Gene + '\n'
    return output

def File_Name(Address):
    filename = ''
    outputname = re.findall(r'dump.*', Address)
    for temp in outputname:
        filename = temp
    filename = filename + '_count'
    return filename

if __name__ == '__main__':
    FileList = ['/data/user/bio18/wch/big/gdownload_name_edited/dump.data.mci.I14','/data/user/bio18/wch/big/gdownload_name_edited/dump.data.mci.I20','/data/user/bio18/wch/big/gdownload_name_edited/dump.data.mci.I40']
    for FileAddress in FileList:
        filecontent = ''
        Single_Copy_filecontent = ''
        Row_Serial = 1

        with open(FileAddress) as f0:
            for line in f0.readlines():
                line = line.strip()
                line1 = re.sub(r'WP_\d+.\d:','',line)
                line1s = re.sub(r'	',' ',line1)
                line2 = re.sub(r'	',' ',line)
                linesplit = line1s.split(' ')
                length = len(linesplit)
                if length == len(set(linesplit)) and length == 80:
                    Single_Copy_filecontent = Single_Copy_filecontent + line2 + '\n'
                    
                Line_output = Output_Each_Line(Row_Serial,length)
                filecontent = filecontent + Line_output
                Row_Serial += 1

        ##-------- Output Gene Families & Genes Count ------------
        NewFileName = File_Name(FileAddress)
        print('Count Gene Families of %s: \t%d' %(NewFileName,Row_Serial-1))
        NewFile = r'/data/user/bio18/wch/big/gdownload_name_edited/%s' % (NewFileName)
        with open(NewFile,'w') as f1:
            f1.write(filecontent)
        
        ##-------- Output Single Copy Genes ----------------------
        SingleCopyFileName = File_Name(FileAddress)+'_Single_Copy_List.txt'
        SingleCopyFile = r'/data/user/bio18/wch/big/gdownload_name_edited/%s' % (SingleCopyFileName)
        with open(SingleCopyFile,'w') as f2:
            f2.write(Single_Copy_filecontent)
