
# Libraries ---------------------------------------------------------------

library(readstata13) #use to open .dta files (read.dta13)


# Individuals & PID (P_Path_l) --------------------------------------------
#load ppathl dataset 

#This is individual-level data like birth year, country of origin...
ppath <- read.dta13(file="ppath.dta", convert.factors=F)



# Spell Data --------------------------------------------------------------

#load refugee spell data
refugspell <- read.dta13(file = "refugspell.dta", convert.dates = TRUE, convert.factors = FALSE)
#load migrant spell data
migspell <- read.dta13(file = "migspell.dta", convert.dates = TRUE, convert.factors = FALSE)






