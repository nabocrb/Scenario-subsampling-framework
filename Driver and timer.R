# Driver to run all code and time it
# subsampling and ranking paper
# Nathan Bonham

rm(list=ls())

library(here)

tik=Sys.time()
time_info=list(tik=tik)
saveRDS(object = time_info, file=here('data', 'time_list.rds'))

source(here('subsampling experiments and robustness calcs - FINAL.R'))
source(here('boxplots of rank corr vs n - clhs 50n.R'))
source(here('linear models -Corr v SFM.R'))

tok=Sys.time()

time_info=readRDS(file=here('data', 'time_list.rds'))
time_info$tok=tok

saveRDS(object = time_info, file=here('data', 'time_list.rds'))

elapsed=tok-time_info$tik

print(paste('elapsed time:', elapsed))
