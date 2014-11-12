# Function to set up scenarios for prediction
scenBuild=function(vi, vRange, vars, ostat, simData){
	if(is.null(vRange)){ vRange=quantile(simData[,vi], probs=c(0.05,0.95), na.rm=T) }	
	scenCol = length(vars); scenRow = length(vRange)
	scenario = matrix(NA, nrow=scenRow, ncol=scenCol)
	colnames(scenario) = c(vars)
	scenario[,vi] = vRange

	viPos = which(vi==vars)
	ovals = apply(simData[,vars[-viPos]], 2, ostat, na.rm=T)
	scenario[,vars[-viPos]] = matrix(rep(ovals,scenRow),nrow=scenRow,byrow=TRUE)
	data.frame(scenario)
}