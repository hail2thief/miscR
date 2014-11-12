# Build dyad dataset from monadic data
# This only works if the units listed in countrylist exactly
# match the units listed in the monad dataset
# Monad data must identify country by a variable called 
# ccode and the time aspect by a variable called by year
# time is a simple vector of years
# countryList is a list containing ccodes for each year
dyadBuildMonad <- function(variable, oper,
	monadData, time, countryList){
	monadData <- monadData[,c('ccode','year',variable)]
	monadData <- data.matrix(monadData)
	rownames(monadData) <- monadData[,'ccode']

	undirectMats <- list()

	for(ii in 1:length(time)){
		countries <- countryList[[ii]]
		yearMatrix <- matrix(0, nrow=length(countries), ncol=length(countries))
		rownames(yearMatrix) <- colnames(yearMatrix) <- countries

		data <- monadData[which(monadData[,'year'] %in% time[ii]), ]

		for(jj in 1:nrow(yearMatrix)){
			cntryRating <- data[as.character(countries[jj]),variable]
			others <- data[as.character(countries),variable]
			if(oper=='absdiff'){relates <- abs(cntryRating-others)}
			if(oper=='same'){relates <- as.numeric(cntryRating==others)}
			yearMatrix[jj,] <- relates
		}

		diag(yearMatrix) <- 0
		undirectMats[[ii]] <- yearMatrix
		print(time[ii]) 
	}
		names(undirectMats) <- time
		undirectMats
}