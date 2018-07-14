from nltk.stem.wordnet import WordNetLemmatizer
from collections import Counter
import sys
reload(sys)
sys.setdefatultencoding('utf-8')

dictionary=dict()
##=========target verbs that McRae'd data include.
targetset = ['execute', 'shoot', 'consider', 'sentence', 'scratch',
             'govern', 'pardon', 'identify', 'corner', 'instruct', 'rob',
             'find', 'capture', 'help', 'revive', 'grab', 'devour', 'parade',
             'show', 'send', 'arrest', 'praise', 'bribe', 'choose', 'overthrow',
             'enslave', 'kick', 'do', 'hit', 'rescue', 'torture', 'evaluate',
             'beat', 'propose', 'watch', 'crush', 'break', 'conquer', 'accuse',
             'convict', 'invite', 'interrogate', 'kill', 'hire', 'execute',
             'terrorize', 'console', 'warn', 'hunt', 'arrest', 'investigate',
             'exile', 'release', 'flog', 'steal', 'injure', 'sketch', 'marry',
             'challenge', 'see', 'teach', 'expect', 'sketch', 'transport',
             'assist', 'slap', 'worship', 'thrill', 'startle', 'lecture',
             'picture', 'calm', 'kick', 'crucify', 'slaughter', 'devour',
             'betray', 'teach', 'throw', 'search', 'cure', 'involve', 'study',
             'kiss', 'interrogate', 'entertain', 'time', 'approve', 'terrorize',
             'love', 'grade', 'visit', 'disobey', 'instruct', 'carry', 'blame',
             'frighten', 'select', 'caress', 'cheer', 'question', 'tow', 'fight'
             , 'paint', 'attack', 'worship', 'punish', 'recognize', 'question',
             'separate', 'invite', 'trap', 'fire', 'punish', 'dismiss', 'serve',
             'interview', 'release', 'lift', 'inject', 'convince', 'eat', 'present',
             'shoot', 'hire', 'abandon', 'transfer', 'seat', 'torture', 'admire',
             'record', 'examine', 'stalk', 'inspire', 'disown', 'indicate', 'tickle',
             'frighten', 'describe', 'accept', 'examine', 'cure', 'grasp', 'want',
             'chase', 'capture', 'excuse', 'pay', 'whip', 'write', 'read', 'take',
             'evaluate', 'interview', 'flatter', 'draw', 'investigate', 'scrub',
             'applaud', 'grade', 'bother', 'hypnotize', 'VERB', 'scorn', 'lecture',
             'bury', 'grow', 'audit', 'hypnotize', 'rescue', 'adopt', 'request',
             'walk', 'desert', 'cheer', 'entertain', 'search', 'haunt']





depCount = open('Depcount2.txt','r').readlines()
sample = [i.split() for i in depCount]
for quad in sample:
       verb = quad[0]
       baseverb = str(WordNetLemmatizer().lemmatize(verb,'v'))
       i =1
       if baseverb in targetset:
              verb = verb.encode('utf-8')
              while i <len(quad)-1:
                     if quad[i].startswith(verb) and baseverb in targetset and quad[i].split('/')[1].startswith('VB'):
                            loc=i
                            j=i+1
                            while j < len(quad)-1:
                                   if quad[j].endswith(str(loc)):

                                          #save Pobj
                                          if(quad[j].split('/')[2]=='prep'):
                                                 loc2 = j
                                                 k = j+1
                                                 while k<len(quad)-1:
                                                        if quad[k].endswith(str(loc2)):
                                                               if baseverb in dictionary.keys():
                                                                      dictionary[baseverb][quad[j][:-2]+' '+quad[k][:-2]]+=int(quad[-1])
                                                               else:
                                                                      dictionary[baseverb] = Counter({quad[j][:-2]+' '+quad[k][:-2]:int(quad[-1])})
                                                               k=k+1
                                          #save Nobj
                                          if(quad[j].split('/')[1]=='NN'):
                                                 if baseverb in dictionary.keys():
                                                        dictionary[baseverb][quad[j][:-2]]+=int(quad[-1])
                                                 else:
                                                       dictionary[baseverb] =Counter({quad[j][:-2]:int(quad[-1])})
                                   j=j+1
                     i=i+1


for i in targetset:
      if i in dictionary.keys():
            #print(i)
            wrfile = open(i+'.csv','w')
            constset = dictionary[i].most_common()
            for const in constset:
                  wrfile.write(const[0]+'\t'+str(const[1])+'\n')
            wrfile.close()







