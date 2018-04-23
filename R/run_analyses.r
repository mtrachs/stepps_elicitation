#change the code so that we define a folder where the entire repository is cloned and then 
# 
folder_location <- '~/git_upload/stepps_elicitation/'
source(paste(folder_location,'expert_elicitation/R/Elicitation Plots.R',sep=''))
#In order for this file to source you will have to download tree composition from 
#https://portal.lternet.edu/nis/mapbrowse?packageid=msb-paleon.1.0
source(paste(folder_location,'calibration/R/prep_data_calibration.R',sep=''))
source(paste(folder_location,'calibration/R/pie_maps_NEUS.R',sep=''))
