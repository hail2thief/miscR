#######################################################################
# GG coefficient plot
library(ggplot2)
theme_set(theme_bw())

ggcoefplot = function(coefData, vars, varNames, estimates, serrors,
	Noylabel=FALSE, coordFlip=TRUE, 
	specY=FALSE, ggylims=NULL, ggybreaks=NULL, revVar=TRUE,
	facet=FALSE, facetName=NULL, facetDim=NULL, facetBreaks=NULL, facetLabs=NULL,
	facetColor=FALSE, colorGrey=FALSE, grSTA=0.8, grEND=0.2, xAngle=45){
  
  # Calculate confidence intervals
  relevRows = which(rownames(coefData) %in% vars)
  estimates = coefData[relevRows, 'Estimate']
  serrors = coefData[relevRows, 'Std. Error']
  facetVar = coefData[relevRows, facetName]
  if(facet){ 
  	VARS = data.frame(cbind(x=varNames, y=vars))
  	temp = rownames(coefData[relevRows,])
  	varNames = as.character(VARS$x[match(temp, VARS$y)])
  }
  ninetyfive_upper_CI = estimates + qnorm(.975)*serrors
  ninetyfive_lower_CI = estimates - qnorm(.975)*serrors
  ninety_upper_CI = estimates + qnorm(.95)*serrors
  ninety_lower_CI = estimates - qnorm(.95)*serrors
  if(facet){
    coefData = cbind(varNames, estimates, serrors, ninetyfive_upper_CI,
                      ninetyfive_lower_CI, ninety_upper_CI, ninety_lower_CI, facetVar)
    } else {
	    coefData = cbind(varNames, estimates, serrors, ninetyfive_upper_CI,
	                      ninetyfive_lower_CI, ninety_upper_CI, ninety_lower_CI)    	
    }
  
  # Reorganize data as numeric
  ggcoefData = data.frame(coefData, row.names=NULL)
  if(facet){
    names(ggcoefData) = c("Variable", "Mean", "SEs", "upper95", "lower95",
                           "upper90","lower90", 'Facet')
    } else {
	    names(ggcoefData) = c("Variable", "Mean", "SEs", "upper95", "lower95",
	                           "upper90","lower90")    	
    }
  ggcoefData$Mean = numSM(ggcoefData$Mean)
  ggcoefData$lower90 = numSM(ggcoefData$lower90)
  ggcoefData$upper90 = numSM(ggcoefData$upper90)
  ggcoefData$lower95 = numSM(ggcoefData$lower95)
  ggcoefData$upper95 = numSM(ggcoefData$upper95)
  if(facet){ggcoefData$Facet = numSM(ggcoefData$Facet)}
  if(revVar){ggcoefData$Variable = factor(ggcoefData$Variable, levels=rev(unique(varNames)))
  	} else {ggcoefData$Variable = factor(ggcoefData$Variable, levels=unique(varNames))}
  
  # Add in variable for colors
  ggcoefData$sig = NULL
  ggcoefData$sig[ggcoefData$lower90 > 0 & ggcoefData$lower95 < 0] = "Positive at 90"
  ggcoefData$sig[ggcoefData$lower95 > 0] = "Positive"
  ggcoefData$sig[ggcoefData$upper90 < 0 & ggcoefData$upper95 > 0] = "Negative at 90"
  ggcoefData$sig[ggcoefData$upper95 < 0] = "Negative"
  ggcoefData$sig[ggcoefData$lower90 < 0 & ggcoefData$upper90 > 0] = "Insig"
  coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
                    "Negative"= rgb(222, 45, 38, maxColorValue=255),
                    "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
                    "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
                    "Insig" = rgb(150, 150, 150, maxColorValue=255))
  
  if(facet){
  if(!facetColor){coefp = ggplot(ggcoefData, aes(as.factor(Facet), Mean, color = sig))}
  if(facetColor){coefp = ggplot(ggcoefData, aes(as.factor(Facet), Mean, color = as.factor(Facet)))}
  coefp = coefp + geom_linerange(aes(ymin=lower95, ymax=upper95), alpha = .3, size = 0.3)
  coefp = coefp + geom_linerange(aes(ymin=lower90, ymax=upper90),alpha = 1, size = 1)
  coefp = coefp + geom_hline(aes(yintercept=0), linetype=2, color = "black")
  coefp = coefp + geom_point(aes(as.factor(Facet),Mean), size=4, shape=20)
  coefp = coefp + geom_errorbar(aes(ymin=lower95,ymax=upper95),linetype = 1,width = 0.1)
  if(specY){coefp = coefp + scale_y_continuous(limits=ggylims, breaks=ggybreaks)}
  if(!colorGrey){
    if(!facetColor){coefp = coefp + scale_colour_manual(values = coefp_colors)}
    if(facetColor){coefp = coefp + scale_colour_brewer()}
    } else { coefp = coefp + scale_colour_grey(start=grSTA,end=grEND) }
  coefp = coefp + xlab("") + ylab("") 
  if(!facetColor){coefp = coefp + facet_wrap(~Variable, scales="free_y", nrow=facetDim[1],ncol=facetDim[2])}
  if(facetColor){coefp = coefp + facet_wrap(~Variable, nrow=facetDim[1],ncol=facetDim[2])}  	
  coefp = coefp + scale_x_discrete(breaks=facetBreaks, labels=facetLabs)
  if(Noylabel){coefp = coefp + theme(axis.text.y = element_blank())}
  if(coordFlip){coefp = coefp + coord_flip()} else {
  	coefp = coefp + theme(axis.text.x=element_text(angle=xAngle, hjust=1)) }
  coefp = coefp + theme(legend.position='none', legend.title=element_blank(),
    axis.ticks=element_blank(), panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())
  coefp 
  	} else {
	  coefp = ggplot(ggcoefData, aes(as.factor(Variable), Mean, color = sig))
	  coefp = coefp + geom_linerange(aes(ymin=lower95, ymax=upper95), alpha = .3, size = 0.3)
	  coefp = coefp + geom_linerange(aes(ymin=lower90, ymax=upper90),alpha = 1, size = 1)
	  coefp = coefp + geom_hline(aes(yintercept=0), linetype=2, color = "black")
	  coefp = coefp + geom_point(aes(as.factor(Variable),Mean), size=4, shape=20)
	  coefp = coefp + geom_errorbar(aes(ymin=lower95,ymax=upper95),linetype = 1,width = 0.1)
	  if(specY){coefp = coefp + scale_y_continuous(limits=ggylims, breaks=ggybreaks)}
	  if(!colorGrey){
	  	coefp = coefp + scale_colour_manual(values = coefp_colors)
	  	} else { coefp = coefp + scale_colour_grey(start=grSTA,end=grEND) }
	  coefp = coefp + xlab("") + ylab("") 
	  if(Noylabel){coefp = coefp + theme(axis.text.y = element_blank())}
	  if(coordFlip){coefp = coefp + coord_flip()} else {
	  	coefp = coefp + theme(axis.text.x=element_text(angle=xAngle, hjust=1)) }
	  coefp = coefp + theme(legend.position='none', legend.title=element_blank(),
	    axis.ticks=element_blank(), panel.grid.major=element_blank(),
	    panel.grid.minor=element_blank(), panel.border = element_blank(),
	    axis.line = element_line(color = 'black'))
	  coefp 
  	}
}
#######################################################################