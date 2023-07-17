# Subsampling experiments and space filling metrics
# cLHS study
# Nathan Bonham
# May 2023

rm(list=ls())

library(dplyr)
library(here)

# load helper functions
source(here('helper functions.R'))
# load robustness metric functions
source(here('robustness function library.R'))

# load set of 500 scenarios, which will be sampled from
scenarios=read.table(here('data', '500 scenarios.txt'))
keep_feature=c('Demand','Mead.PE','Powell.PE','Driest20yrAVG', 'Wettest20yrAVG', 'Driest2yrAVG', 'Wettest2yrAVG')
scenarios=scenarios[keep_feature]

##### load objectives from testing in 500 SOW

objectives=read.table(here('data', 'objectives_all463.txt'))
obj_names=c( 'LB.AVG', "LB.AVG.Pol", 'LF.Def', 'M1000', "P3490", "LB.Dur",
             'LB.Freq', "LB.Max", "P.WYR")
colnames(objectives)=c('TraceNumber', 'policy', obj_names)
objectives[c('LB.AVG', 'LB.AVG.Pol', 'LB.Max')]=objectives[c('LB.AVG', 'LB.AVG.Pol', 'LB.Max')]/1000 # convert to Thousand Acre Feet
objectives['P.WYR']=objectives['P.WYR']/1e6

my_order=c('LF.Def', 'P.WYR', 'P3490', 'M1000', 'LB.AVG', 'LB.Freq', 'LB.Max', 'LB.Dur')
objectives=objectives[c('TraceNumber', 'policy', my_order)]

obj_names=my_order

# compute robustness scores using 500 SOW

robust500=robustness(data=objectives, keepSOW = 1:500)
r500ranks=robustness_ranks(robust500)

### define sample sizes

N=seq(50,450,50)

niters=30

start_df=T # flag to create data frame on first iteration

for (n in N){
  
  print(paste('--------------------------------------------------------'))
  print(paste('--------------------------------------------------------'))
  print(paste('n =',n))
  print(paste('--------------------------------------------------------'))
  print(paste('--------------------------------------------------------'))
  
  t0n=Sys.time()
  
  #robustness.list[[as.character(n)]]=list()
  #corr.list[[as.character(n)]]=list()
  #diff.list[[as.character(n)]]=list()
  
  
  for (i in 1:niters){
   
    t0=Sys.time()

    temp.robustness=list()

    set.seed(i)

    ########################## original cLHS ###################################
    
    print(paste('clhs, iter', i, '/', niters))
    clhs_temp=clhs(scenarios, size=n, simple = F, iter=1000, progress = T, weights = list(numeric=1, factor=0, correlation=0), use.cpp=T)
    # note that use.cpp=T uses C++ for annealing process, which is ~150 times faster than my uCLHS implementation above (which stays R native).
    # no progress bar shows when use.cpp=T.
    temp_design=clhs_temp$sampled_data

    list2=robustness(keepSOW = clhs_temp$index_samples)
    temp.robustness[['clhs']]=list2
    add.corr=data.frame(method='clhs',n=n, iter=i,rank_corr(baseline_ranks = r500ranks, list2))
    add.diff=data.frame(method='clhs', n=n, iter=i,rel_difference(robust500,list2))

    ## compute metrics
    
    temp_scores=SFD_metrics(temp_design)
    to_add=data.frame(method='cLHS', n=n, iter=i, temp_scores)
    
    if(start_df){
      scores=to_add
      corr.df=add.corr
      diff.df=add.diff
      start_df=F
    } else {
      scores=rbind(scores, to_add)
      corr.df=rbind(corr.df, add.corr)
      diff.df=rbind(diff.df, add.diff)
    }
 
 
    outfile=paste0('n',n,'i',i, '.rds')

    saveRDS(temp.robustness, file = here('data','robustness lists', outfile))

    t1=Sys.time()
    print(paste('iter time:'))
    print(difftime(t1,t0, units = 'mins'))
    
      
  } # niters

  write.table(scores, here('data','scores', 'SFD scores.txt'))
  write.table(corr.df, here('data','scores', 'rank correlation.txt'))
  write.table(diff.df, here('data','scores', 'difference.txt'))
  
  t1n=Sys.time()
  print(paste('time to complete n =', n, ':'))
  print(difftime(t1n,t0n, units = 'mins'))

}


