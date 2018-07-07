
#Extract similarities using Word2Vec and DepEmbeddings

import os
import gensim
import pandas as pd
import math

def cosine_similarity(v1,v2):
       sumxx, sumxy, sumyy = 0,0,0
       for i in range(len(v1)):
              x = v1[i]; y = v2[i]
              sumxx += x*x
              sumyy += y*y
              sumxy += x*y
       return sumxy/math.sqrt(sumxx*sumyy)

#Word2vec
model = gensim.models.KeyedVectors.load_word2vec_format('GoogleNews-vectors-negative300.bin', binary=True)  

#Dependency-based Word Embeddings
contexts = open('deps.contexts','r').readlines()
words = open('deps.words','r').readlines()



data = open('Datasheet.txt','r').readlines()
data = data[0].split('\r')[1:-1]
data2 = open('Datasheet2.txt','w')
data2.write('sent\tverb\tobj\tType\tPatienthood\tBigramF\tW_Freq\tlogWFreq\tDepEmbedding\tWord2Vec\n')


verbset = dict()
objset = dict()

                    

for i in data:
       verb = i.split('\t')[1]
       verbset[verb]=None
       obj= i.split('\t')[2]
       objset[obj] = None


for i in words:
       if i.split(' ')[0] in verbset.keys():
              i = i.rstrip()
              verbset[i.split(' ')[0]]= [float(k) for k in i.split(' ')[1:]]


for i in contexts:
       if i.split(' ')[0].replace('dobj_','') in objset.keys():
              i = i.rstrip()
              objset[i.split(' ')[0].replace('dobj_','')]= [float(k) for k in i.split(' ')[1:]]



for i in data:
       verb = i.split('\t')[1]
       obj = i.split('\t')[2]
       if verbset[verb] != None and objset[obj] !=None:
              i += '\t'+str(cosine_similarity(verbset[verb],objset[obj]))
       else:
              i+= '\t'+"None"
       i+='\t'+str(model.similarity(verb,obj))
       data2.write(i+'\n')

data2.close()


