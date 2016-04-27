#!/usr/bin/python
import re
import random

# Get and parse:
fin = open("Merged_data.csv","r")
data = []
fin.readline()
for line in fin:
    c = line.split(",")
    data.append((c[1],c[2],int(c[-4])))

        
# approach: Make a string and mine it with RE
strd = ""
for i in data:
    strd += str(i[1])



# First pattern: stretch of ones. 

pat1 = re.compile("1+")

# Randomization (permutation) of string and research of the largest pattern:
rs1=[]
for i in xrange(10000):   
    random.shuffle(l)
    tmp  = ''.join(l)
    rs1.append(max(map(len,re.findall(pat1,tmp))))

# rs1 now contains the length of the longest pattern in each permutation
# we sort and search the first value that is contained in the 5% of the distribution:
rs1.sort()
threshold1 = 0
for i in range(rs1[0],rs1[-1]):
    rank = rs1.index(i)/float(len(rs1))
    if rank > 0.95:
        threshold1 = i
        break

# print all pattern in observed data with a length > threshold
for i in re.finditer(pat1,strd):
    if len(i.group(0))>threshold1:
        print i.start(),i.end(),i.group(0)

###################################################################
###################################################################


# Second pattern: Stretch of ones with one interruption
pat2 = re.compile("1+[1234]1+")

# Randomization (permutation) of string and research of the largest pattern:
rs2=[]
for i in xrange(10000):   
    random.shuffle(l)
    tmp  = ''.join(l)
    rs2.append(max(map(len,re.findall(pat2,tmp))))

# rs2 now contains the length of the longest pattern in each permutation
# we sort and search the firs2t value that is contained in the 5% of the distribution:
rs2.sort()
threshold2 = 0
for i in range(rs2[0],rs2[-1]):
    rank = rs2.index(i)/float(len(rs2))
    if rank > 0.95:
        threshold2 = i
        break

fout = open("cluster_abs_pos.txt",'w')

# print all pattern in observed data with a length > threshold
for i in re.finditer(pat2,strd):
    if len(i.group(0))>threshold1:
        fout.write(str(i.start())+" "+str(i.end())+ "\n")

fout.cose()



