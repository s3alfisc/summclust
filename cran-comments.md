## R CMD check results

+ sumbission 7: 

A while back, I received the following comments by the CRAN team, summarized in this [github issue](https://github.com/s3alfisc/summclust/issues/9). 

All comments have been addressed. 

The package has further been tested via github actions (multiple OS), 
`pkgcheck::pkgcheck()`, `rhub::check_for_cran()` and `devtools::check_win_devel()`. No errors, no warnings. 


+ Submission 6: 

This is a re-submission of submission 5, which happened during your (the CRAN team's) well deserved summer break. 
I have checked the package locally, on github actions (no comments), rhub (2 notes for linux: detritus in them and lastMiKTexTexException), and wind-develop.

+ Submission 5: 

I have changed Stata to 'STATA' in description; add 'codemeta.json' to .Rbuildignore 
after receiving the following feedback: 

New submission

Possibly misspelled words in DESCRIPTION:
  Stata (11:98)
* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  'codemeta.json'

No notes on r-hub, r-devel.

+ Submission 4: 

deleted broken link; ignore misspelled Stata (it is not misspelled); checked again on 
rhub; github actions 

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
