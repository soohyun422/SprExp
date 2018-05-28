# SprExp

Important things to note about the R file:

1. R does not really have a way to comment out blocks of code, so I'm using 
   "if (FALSE) { ...}" to achieve that effec.
   When we run the actual data, these commented blocks need to be allowed to run.
   
2. Each participant has his/her peculiar reading time as a function of caracter length, and so
   the regressions allow us to measure the effect to character length. As it stands, this is
   done ennumeratively, one regression per person. This code is fail-proof, but it would be 
   nice to one day have a "for" loop here, to avoid so many repeated lines of code.
   
3. The same goes for the code for each of the regions.

4. To get a good discussion of how the analysis proceeds see: 
    https://www.hlp.rochester.edu/resources/BCS152-Tutorial/SPRAnalysisResults.html
    
5. All is done in the R code except:
        5.1 Because we have 4 conditions, we need multiple LMERs, not just 1.
        5.2 The code automatically creates a LaTeX file with the graph. But currently one data
            point is shown. All else is made up. This needs to be fixed.
        5.3 Because there are very few participants, these data sets yield some errors and because
            some reading times are ridiculously fast (you just pressed on the key as quick as possible),
            the graph is odd. But with real data both issues should vanish.
