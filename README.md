# SprExp

Reg1: HH = HL, LH, LL
Reg2: HH = HL, LH, LL
Reg3: HH = LH, LL, but != HL    
Reg4: HH = HL, LH, LL
Reg5: HH = HL, LH, LL
Reg6: HH = LH, LL, but != HL
Reg5: HH = HL, LH, LL


Important things to note about the R file:

1. Each participant has his/her peculiar reading time as a function of caracter length, and so
   the regressions allow us to measure the effect to character length. As it stands, this is
   done ennumeratively, one regression per person. This code is fail-proof, but it would be 
   nice to one day have a "for" loop here, to avoid so many repeated lines of code.
   The same goes for the code for each of the regions.

2. To get a good discussion of how the analysis proceeds see: 
    https://www.hlp.rochester.edu/resources/BCS152-Tutorial/SPRAnalysisResults.html
    
