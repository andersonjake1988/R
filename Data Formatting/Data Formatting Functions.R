##############################################################
# Package Loading
##############################################################
install.packages("Formula")
library(Formula)
library(mosaic)
library(tidyverse)

##############################################################
# Completed Formatting Functions
##############################################################

# Changing variables with less than 10 different values to Factors
mosaic::factorize() "works pretty well, but below will customize the cutoff for unique values"
for(i in names(test)){
  if(nlevels(as.factor(test[[i]])) < 10){
    test[[i]] <- as.factor(test[[i]])
  }
}

# Pull Numeric Vector Names from dataframe
PNVN <- function(data){
  number_cols <- list()
  for(i in names(data)){
    if(is.numeric(data[[i]])){
      number_cols[i] <- class(data[[i]])
    }
  }
  return(names(number_cols))
}

# Pull Numeric Columns from dataframe
PNV <- function(data){
  number_cols <- list()
  for(i in names(data)){
    if(is.numeric(data[[i]])){
      number_cols[i] <- class(data[[i]])
    }
  }
  return(data[,names(number_cols)])
}

# Extract only numbers from a mixed number/letter variable
numextract <- function(string){
  as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
}

# Full dataset number extract
numextract_tot <- function(data){
  for(i in names(data)){
    data[[i]] <- as.numeric(numextract(data[[i]]))
  }
  return(data)
}
