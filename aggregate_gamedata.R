# By: James Tan

# Date: 6/3/2019

p_load(reshape2, beepr, scales, zoo, tidyverse)

source('~/.Rprofile')
setwd(datapath)

gamedata_output <- tibble()

for(i in 1:6){
  gamedata <- readRDS(file=sprintf('g4_%s_interpolated_scaled_100m_gamedata.RData', sprintf('model_%d', i)))
  gamedata_output <- rbind(gamedata_output, gamedata) 
}

saveRDS(gamedata_output, file=sprintf('g4_all_models_interpolated_scaled_gamedata.RData'))
write_csv(gamedata_output, path=sprintf('g4_all_models_interpolated_scaled_gamedata.csv'))
