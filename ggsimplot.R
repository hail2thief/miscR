#######################################################################
# gg simulation plot
library(ggplot2)
theme_set(theme_bw())

ggsimplot = function(
  modelResults, sims=10000, simData, vars, vi, ostat=median, 
  sigma=FALSE, intercept=TRUE, ylabel, xlabel,
  specX=FALSE, ggxlims=NULL, ggxbreaks=NULL,
  specY=FALSE, ggylims=NULL, ggybreaks=NULL, plotType='errorBar'){

  # Prep data
  simData=na.omit(simData[,vars])

  # Pull out model results
  vcov=vcov(modelResults)
  betas=modelResults@beta; names(betas)=rownames(vcov)
  RSS=sum(resid(modelResults)^2)
  dfResid=nrow(simData)-length(modelResults@beta) - length(modelResults@u) + 1
  if(sigma){sigma = sqrt(RSS/dfResid)} else {sigma = 0}

  # Set up scenario
  # vRange=seq(min(simData[,vi]), max(simData[,vi]), .1)
  vRange=sort(unique(simData[,vi]))
  scenCol = length(vars); scenRow = length(vRange)
  scenario = matrix(NA, nrow=scenRow, ncol=scenCol)
  colnames(scenario) = c(vars)
  scenario[,vi] = vRange
  
  viPos = which(vi==vars)
  ovals = apply(simData[,vars[-viPos]], 2, ostat)
  scenario[,vars[-viPos]] = matrix(rep(ovals,scenRow),nrow=scenRow,byrow=TRUE)
  if(intercept){scenario = cbind('(Intercept)'=1, scenario)}
  vars2 = colnames(scenario)
  
  draws = mvrnorm(n = sims, betas[vars2], vcov[vars2,vars2])
  modelPreds = draws %*% t(scenario)
  modelExp = apply(modelPreds, 2, function(x) FUN=rnorm(sims, x, sigma))
  
  if(plotType=='errorBar' | plotType=='ribbon'){
    ggData = t(apply(modelExp, 2, 
       function(x){ mean = mean(x) ;
         qlo95 = quantile(x, 0.025) ; qhi95 = quantile(x, 0.975) ;
         qlo90 = quantile(x, 0.05) ; qhi90 = quantile(x, 0.95) ;
         rbind(mean, qlo95, qhi95, qlo90, qhi90) } ))
    ggData = data.frame(ggData)
    colnames(ggData) = c('Fit', 'Lo95', 'Hi95', 'Lo90', 'Hi90')
    
    if(plotType=='errorBar'){
        ggData$x = as.factor(vRange)
        temp = ggplot(ggData)
        temp = temp + geom_errorbar(aes(x=x, ymax=Hi90, ymin=Lo90), width=0.2) 
        temp = temp + geom_errorbar(aes(x=x, ymax=Hi95, ymin=Lo95), width=.5) 
        temp = temp + geom_point(aes(x=x, y=Fit))
        temp = temp + geom_rug(sides='b', position='jitter')
        if(specX){temp = temp + scale_x_discrete(limits=ggxlims, breaks=ggxbreaks)}
        }
    
    if(plotType=='ribbon'){
      ggData$x=vRange
      temp <- ggplot(ggData, aes(x=x, y=Fit, ymin=Lo95, ymax=Hi95))
      temp <- temp + geom_line() + geom_ribbon(alpha=0.3)
      temp <- temp + geom_ribbon(aes(ymin=Lo90, ymax=Hi90), alpha=0.5)
      temp = temp + geom_rug(sides='b', position='jitter')
      if(specX){temp = temp + scale_x_continuous(limits=ggxlims, breaks=ggxbreaks)}
    }
  }

  if(plotType=='distribution'){
    colnames(modelExp)=1:ncol(modelExp)
    modelExp2=melt(modelExp)[,-1]
    ggMeans = ddply(modelExp2, .(X2), summarise, sMean=mean(value))
    ggDensity = ddply(modelExp2, .(X2), .fun=function(x){
      tmp = density(x$value); x1 = tmp$x; y1 = tmp$y
      q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
      q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
      data.frame(x=x1,y=y1,q95=q95, q90=q90) } )

    ggMeans$X2 = as.factor(ggMeans$X2)
    ggDensity$X2 = as.factor(ggDensity$X2)

    temp = ggplot()
    temp = temp + geom_line(data=ggDensity, aes(x=x,y=y,color=X2))
    temp = temp + geom_vline(data=ggMeans,
      aes(xintercept=sMean, color=X2),linetype='solid',size=1)
    temp = temp + geom_ribbon(data=subset(ggDensity,q95),
      aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.5)
    temp = temp + geom_ribbon(data=subset(ggDensity,q90),
      aes(x=x,ymax=y,fill=X2),ymin=0,alpha=0.9)
    temp = temp + theme(legend.position='none')
    if(specX){temp = temp + scale_x_continuous(limits=ggxlims, breaks=ggxbreaks)}
  }
  
  if(specY){temp = temp + scale_y_continuous(limits=ggylims, breaks=ggybreaks)}
  temp = temp + xlab(xlabel) + ylab(ylabel)
  temp = temp + theme(panel.border = element_blank(), 
  axis.line = element_line(), axis.ticks = element_blank(),
  panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
  axis.title.x = element_text(vjust=-0.2),
  axis.title.y = element_text(vjust=0.2))
  temp
}
#######################################################################

#######################################################################
# Par mfrow equiv for ggplot
multiplot <- function(plots, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  # plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}
#######################################################################