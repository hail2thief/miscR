# Function to create APSR tables for duration object
# modelResults= list object of models
durTable = function(modResults, varDef, digs=3){
	modSumm=lapply(modResults, 
		function(x) FUN=summary(x)$coefficients[,c('coef','se(coef)','Pr(>|z|)')])
	noModels=length(modSumm)

	varsTable=varDef[,1]
	tableResults = matrix('', nrow=2*length(varsTable), ncol=1+noModels)
	tableResults[,1]=rep(varsTable,2)
	colnames(tableResults) = c('Variable',paste('Model',1:noModels))

	for(ii in 2:ncol(tableResults)){
		temp = modSumm[[ii-1]]
		n = modResults[[ii-1]]$n-length(modResults[[ii-1]]$coefficients)
		temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
		estims = temp[1:length(varsTable),'coef']
		estims = round(as.numeric(as.character(estims)),digs)
		pvals = abs(temp[1:length(varsTable),'Pr(>|z|)'])
		pvals = round(as.numeric(as.character(pvals)),digs)
		estims = ifelse(pvals<=0.10 & !is.na(pvals) & pvals>0.05, 
			paste('$', estims,'^{\\ast}$',sep=''), estims)
		estims = ifelse(pvals<0.10 & !is.na(pvals) & pvals<=0.05, 
			paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
		estims = ifelse(is.na(estims),'',estims)
		tableResults[1:length(varsTable),ii] = estims
		serrors = temp[(length(varsTable)+1):nrow(tableResults),'se(coef)']
		serrors = round(as.numeric(as.character(serrors)),digs)
		serrors = paste('(',serrors,')',sep='')
		serrors = ifelse(serrors=='(NA)','',serrors)
		tableResults[(length(varsTable)+1):nrow(tableResults),ii] = serrors
	}

	# Reorganizing rows and variable labels
	tableFinal = NULL
	for(ii in 1:length(varsTable)){
	temp = cbind('', t(tableResults[ii+length(varsTable),2:ncol(tableResults)]))
	tableFinal = rbind(tableFinal, tableResults[ii,], temp) }

	# Adding other info
	sSize = cbind('n', t(as.vector(mapply(x=modResults, 
		function(x) FUN=x$n))))
	events = cbind('Events', t(as.vector(mapply(x=modResults, 
		function(x) FUN=x$nevent))))
	logtest = cbind('Likelihood ratio test', 
		t(as.vector( 
			mapply(x=modResults, function(x) 
				FUN=paste(
					round(summary(x)$logtest[1], digs-1),
					paste0('(',round(summary(x)$logtest[3], digs-1),')')
				 ) 
				)
			) ) )

	tableFinal = rbind(tableFinal, sSize, events, logtest)

	temp=varDef[match(tableFinal[,'Variable'], varDef[,1]),2]
	temp[which(is.na(temp))]=tableFinal[,'Variable'][which(is.na(temp))]
	tableFinal[,'Variable']=temp
	tableFinal
}