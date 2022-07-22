## R CMD check results

+ I have switched imports to depends, this solves the 'excessive importing' 
  note

* This is a new release.
* The package was checked on rhub, r-devel, and github actions (windows, mac, 
  ubuntu)
  
Two notes were found: 
"   Depends: includes the non-default packages:
     'ggplot2', 'latex2exp', 'dreamerr', 'fabricatr', 'MASS', 'collapse'
   Adding so many packages to the search path is excessive and importing
   selectively is preferable.
"

I personally don't think that adding these packages is excessive, but am 
happy to be convinced otherwise =)  

Second 
"Possibly misspelled words in DESCRIPTION:
  Stata (6:105)
"
This is spelled correctly.

0 errors | 0 warnings | 2 note
