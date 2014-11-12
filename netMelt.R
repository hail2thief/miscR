netMelt <- function(meltData, meltID, meltYr, netList, rst=TRUE, netStat=function(x){mean(x)}){
	ndata <- NULL
	for(ii in 1:nrow(meltData)){
		slice <- meltData[ii,]
		sen <- as.character(t(slice[,3:7]))
		sen <- sen[!is.na(sen)]
		tar <- as.character(slice[,meltID])

		if(length(intersect(names(netList),slice[,meltYr]))!=0){
				ddata <- netList[[as.character(slice[,meltYr])]]
		
				if(rst==TRUE){
						# Row standardize
						matDenom <- apply(ddata, 1, sum); matDenom[matDenom==0] <- 1
						ddata <- ddata/matDenom}
		
				if(!is.na(sum(match(c(tar,sen),rownames(ddata))))){
					# Row/Col Rel.	
					ddata <- ddata[tar, sen]
					# Network measure
					ddata <- netStat(ddata) } else {
						ddata <- NA
					}
				} else {
					ddata <- NA
				}

		# Combine
		ndata <- rbind(ndata, ddata)

		# Progress
		if(ii==1 | ii%%1000==0 | ii==nrow(meltData)){
			cat(paste(round(100*ii/nrow(meltData),0),'% ',sep=''))}
	}
	print(' Completed '); ndata
}