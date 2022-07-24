## R CMD check results


+ Submission 4: 

fixed broken link; ignore misspelled Stata (it is not misspelled)

"
Possibly misspelled words in DESCRIPTION:
  Stata (11:98)

Found the following (possibly) invalid URLs:
  URL: https://github.com/%3Corg%3E/%3Crepo%3E/actions?query=workflow%3Apkgcheck
    From: README.md
"

+ Submission 3: 

* filled in an empty URL in the readme file
* checked on rhub, win_devel and github actions (Mac, Ubuntu, windows), 
  no errors, messages received, but the following notes 
  
  "Possibly misspelled words in DESCRIPTION:
  Stata (11:98)

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  "
  
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
