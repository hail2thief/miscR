# Helper functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Rescaling variables
rescale <- function(x,new_max,new_min){
 xResc <- (new_max - new_min) / (max(x,na.rm=T) - min(x,na.rm=T))*(x - min(x,na.rm=T)) + new_min
 xResc }

# turn variables into numeric
char = function(x){as.character(x)}
num <- function(x){ as.numeric(char(x)) }

# Convert to cname
library(countrycode)
cname <- function(x){
	require(countrycode); x <- as.character(x)
	y <- countrycode(x, 'country.name', 'country.name') }
