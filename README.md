# SprExp

Findings: no difference across all conditions in regions 1, 2, 4, 5 and 7. Condition HL was read *faster* than all other conditions in region 3 (non-critical) and *slower* than all other conditions in region 6 (critical).


Important things to note about the R file:

1. Each participant has his/her peculiar reading time as a function of caracter length, and so
   the regressions allow us to measure the effect to character length. As it stands, this is
   done ennumeratively, one regression per person. This code is fail-proof, but it would be 
   nice to one day have a "for" loop here, to avoid so many repeated lines of code.
   The same goes for the code for each of the regions.

2. To get a good discussion of how the analysis proceeds see: 
    https://www.hlp.rochester.edu/resources/BCS152-Tutorial/SPRAnalysisResults.html
    
