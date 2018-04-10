library(rioja)
#----------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
library(neotoma) #1.7
library(analogue)#0.17-0
library(rioja)#0.9-15
library(maps)#3.2.0
#---------------------------------------------------------------------------------------------------------------

#set working directory and location of helper files
setwd('~/workflow_stepps_calibration/expert_elicitation/')
help.fun.loc <- 'elicitation_helper_funs/'
data.loc <- 'data/'
plot.loc <- 'plots/'




mount_davis <- read.csv('~/Downloads/dataset15338.csv')

spec_inc <- c('Tsuga','Picea','Pinus','Abies','Betula','Quercus','Fagus','Fraxinus','Castanea','Carpinus',
              'Cyperaceae','Poaceae','Artemisia','Asteraceae un','Rumex','Ambrosia')


col_use <- sapply(spec_inc,function(x){
  grep(x,mount_davis$name)
})

pollen_use <- mount_davis[unlist(col_use),]

pollen <- pollen_use[,-c(1:5)]
pollen[is.na(pollen)] <- 0
pollen <- t(pollen)
pollen.perc <- 100*pollen/rowSums(pollen)
colnames(pollen.perc) <- mount_davis$name[unlist(col_use)]
depth <- mount_davis[mount_davis$name=='Depth',-c(1:5)]
sqrt.pollen.perc <- sqrt(pollen.perc)


pdf(paste(plot.loc,"/Mount_Davis_Marsh_sqrt.pdf",sep=''),width = 11.69,height = 8.29)
strat.plot(d =sqrt(pollen.perc),
            yvar = t(depth), 
           scale.percent=TRUE, 
           y.rev = TRUE,ylabel = 'Depth [cm]',
           title='Mount Davis')
dev.off()
