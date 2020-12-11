#Merge Seq
import os,re,sys


def File_list(File_dir):
    F_List=[]
    for root, dirs, files in os.walk(File_dir):
        for file in files:
            if os.path.splitext(file)[1] == '.fas':
                F_List.append(os.path.join(root, file))
    return F_List

def New_Description(GCF_ID,Address_lines):
    lines = Address_lines
    for line in lines:
        if line[0] == '>':
            line_GCF = re.split(r'[ :]', line.strip()[1:])[1]
            if GCF_ID == line_GCF:
                ORGANISM = ''
                ORGANISMl = re.findall(r'[[].*[]]', line)
                for og in ORGANISMl:
                    ORGANISM = og
                Output_Description = line_GCF + ' ' + ORGANISM
                return Output_Description

def GCF_Code(ID_line):
    ID_line_split = re.split(r'[ :]', ID_line.strip()[1:])[1]
    return ID_line_split

def Fetch_Seq_from_Files(GCF,FileList):
    seq_content = ''
    for FileAddress in FileList:
        ft = open(FileAddress)
        ftLines = ft.readlines()
        ft.close()
        seq_Y_or_N = 1
        for ftLine in ftLines:
            if ftLine[0] == '>':
                ftLine_GCF = re.split(r'[ :]', ftLine.strip()[1:])[1]
                if GCF == ftLine_GCF:
                    seq_Y_or_N = 0
                else:
                    seq_Y_or_N = 1
            else:
                if seq_Y_or_N == 0:
                    seq_Y_or_N = 0
                else:
                    seq_Y_or_N = 1
            if seq_Y_or_N == 0 and ftLine[0] != '>':
                seq_content = seq_content + ftLine
    return seq_content


if __name__ == '__main__':
    dir = '/data/user/bio18/wch/big/gdownload_name_edited/Gene_Families/GF_with_hypothetical'
    FileList = File_list(dir)
    Seq_content = ''
    GCF_Queue = []
    File_Address = FileList[0] #=0-72 /total 73fas
    fr = open(File_Address)
    frLines = fr.readlines()
    fr.close()
    for frLine in frLines:
        if frLine[0] == '>':
            GCF = GCF_Code(frLine)
            GCF_Queue.append(GCF)
    for GCF_goal in GCF_Queue:
        Description_and_Seq = '>' + New_Description(GCF_goal,frLines) + '\n' + Fetch_Seq_from_Files(GCF_goal,FileList) + '\n'
        Seq_content = Seq_content + Description_and_Seq
        print(New_Description(GCF_goal,frLines))
        
    fw = open('/data/user/bio18/wch/big/gdownload_name_edited/Gene_Families/psudo_molecular_sequences_alignment.fas','w')
    fw.write(Seq_content)
    fw.close()