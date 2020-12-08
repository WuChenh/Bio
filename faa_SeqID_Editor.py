import re
import os

def file_list(file_dir):
    F_List=[]
    for root, dirs, files in os.walk(file_dir):
        for file in files:
            if os.path.splitext(file)[1] == '.faa':
                F_List.append(os.path.join(root, file))
    return F_List

def GCF_code(Gfilename):
    AddPrefix = ''
    AddPrefix0 = re.findall(r'GCF_\d+.\d',Gfilename)
    for temp1 in AddPrefix0:
        AddPrefix = temp1
    return AddPrefix

def File_Name(Address):
    filename = ''
    outputname = re.findall(r'GCF_.*.faa', Address)
    for temp2 in outputname:
        filename = temp2
    return filename

def Replace_Full_Name(SeqPrior,GCF):
    seq_prior = ''
    seq_prior0 = re.findall(r'>\w+_\d+.\d',SeqPrior)
    for temp3 in seq_prior0:
        seq_prior = temp3
    Replace = r'>\w+_\d+.\d'
    Full_Name0 = seq_prior + ':' + GCF
    Full_Name = re.sub(Replace, Full_Name0, SeqPrior)
    return Full_Name

if __name__ == '__main__':
    dir = '/data/user/bio18/wch/big/gdownload'
    FileList = file_list(dir)
    for FileAddress in FileList:
        GCF = GCF_code(FileAddress)
        Output_Name = File_Name(FileAddress)
        filecontent = ''

        with open(FileAddress) as f0:
            #s = f0.read().strip()
            line = f0.readline()
            while line:
                FullLine = Replace_Full_Name(line, GCF)
                filecontent = filecontent + FullLine
                line = f0.readline()
        f0.close()
        #SingleLineInFaa = re.findall(r'>\w+_\d+.\d',s)

        NewFileName = Output_Name + 'new'
        NewFile = r'/data/user/bio18/wch/big/gdownload_name_edited/%s' % (NewFileName)
        with open(NewFile,'w') as f1:
            f1.write(filecontent)
        f1.close()
