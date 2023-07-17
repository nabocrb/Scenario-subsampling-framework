# Scatter plots of subsampling experiments and robustness results
# Nathan Bonham
# April 2023

rm(list=ls())

library(here)
library(dplyr)
library(ggpubr)
source(here('robustness function library.R'))
source(here('helper functions.R'))
library(ggplot2)

SFD_corr=read.table(here('data', 'scores', 'SFD_corr.txt'))


######## rank correlation and SFD metrics ############

SFD_corr$method=factor(SFD_corr$method, levels=c('uclhs', 'cLHS', 'FSCS', 'random'), ordered = T)

n_keep=seq(50,450,50)

SFD_corr=dplyr::filter( SFD_corr, n %in% n_keep)

######## compute robustness metrics using all 500 scenarios ##############

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

##### load objectives from MOEA optimization, for use only in percent_deviation()

# MOEA_archive=read.table(here('data','Archive_463solns.txt')) # data from MOEA optimization.
# MOEA_data=MOEA_archive[,15:22] # subset only the objectives (remove decision levers)
# names=c('M1000', 'LB.Dur', 'LB.Freq','LB.AVG', 'LB.Max', 'P3490', 'P.WYR', 'LF.Def') 
# colnames(MOEA_data)=names
# ## unit conversion for MOEA_data
# vol_index2=which(colnames(MOEA_data)%in% c('LB.AVG', 'LB.Max'))
# MOEA_data[vol_index2]=MOEA_data[vol_index2]/1000
# MOEA_data$P.WYR=MOEA_data$P.WYR/1e6
# 
# MOEA_data=MOEA_data[my_order]

# compute robustness scores using 500 SOW

robust500=robustness(data=objectives, keepSOW = 1:500)
r500ranks=robustness_ranks(robust500, ties = 'min')

################## end compute robustness for 500 scenarios #############


################# Rank diagrams for  corrs #######################

robustness_types=c('satisficing','regret_best')

corrs=c(.95, .975, 1)


plot_list=list()

  objectives=c('M1000', 'LB.Dur', 'LB.Max')
  rows=1
  cols=2
  wf=1.4;hf=1

for(robust_type in robustness_types){
  
  robust_plots=list()
  
  if(robust_type=='satisficing'){

    rank_correction=-1

  } else {

    rank_correction=1
  }

  c=1
  
  for(objective in objectives){
    
    tempn500df=data.frame(r500ranks[[robust_type]])
    tempn500ranks=tempn500df[[objective]]
    
    iter1=T
    
    
    for(corr in corrs){
      
      col.name=paste(robust_type,objective,sep = '.')
      
      row=which.min(abs(SFD_corr[[col.name]]-corr))
      method=SFD_corr$method[row]
      n=SFD_corr$n[row]
      i=SFD_corr$iter[row]
      ni=paste0('n',n,'i',i,'.rds')
      tempcorr=SFD_corr[[col.name]][row]
      label=paste0(round(tempcorr, 3), '_',n)
      
      
      templist=readRDS(here('data', 'robustness lists', ni))#[[method]]  
      temprobustness=templist$clhs[[robust_type]][[objective]] # robustness scores for given metric, objective, method, n, and iter
      
      tempranks=rank(temprobustness*rank_correction, ties.method = 'min')
      
      add.df=data.frame(sample.rank=tempranks, n500.rank=tempn500ranks, corr=tempcorr, robust_type, objective, corr_n=label)
      
      if(iter1){
        rank.df=add.df
        iter1=F
      }else{
        rank.df=rbind(rank.df, add.df)
      }
      
    } # end corrs
    
    rank.df$corr=round(rank.df$corr,2)
    rank.df$corr=factor(rank.df$corr)
    
    rank.plot=ggplot()+
      geom_point(data=rank.df, aes(y=sample.rank, x=n500.rank, color=corr_n), size=.8)+
      ggtitle(objective)+
      geom_abline(intercept=0, slope=1, size=.6)+
      #geom_line(aes(x=order(tempn500ranks), y=order(tempn500ranks)), size=1)+
      theme_minimal()+
      theme(legend.position = c(.8,.25), legend.title = element_blank())
    
    # if(c %in% (1:3)){
    #   rank.plot=rank.plot+xlab(NULL)
    # }
    # 
    # if(c %in% c(2,3,5,6)){
    #   rank.plot=rank.plot+ylab(NULL)
    # }

    c=c+1
    
    robust_plots[[objective]]=rank.plot
    
  } # end objectives

  combined=ggpubr::ggarrange(plotlist = robust_plots,ncol=3,nrow=1, common.legend = F, align='v')
  
  combined=ggpubr::annotate_figure(combined, top=text_grob(robust_type))
  outfile=paste0('rank diagram', robust_type, '.tiff')
  
  ggexport(combined, filename = here('figures', outfile), width=2200*wf, height=1000*hf, pointsize = 12, res=400)
  
  
  plot_list[[robust_type]]=robust_plots

}


  
  ################# Top 10 Rank diagrams for chosen corr threshold #######################
  
  robustness_types=c('satisficing','regret_best')
  
  corrs=c(.975)
  
  plot_list=list()
  
  objectives=c('M1000', 'LB.Dur', 'LB.Max')
  rows=1
  cols=2
  wf=1.4;hf=1
  
  for(robust_type in robustness_types){
    
    robust_plots=list()
    
    if(robust_type=='satisficing'){
      
      rank_correction=-1
      
    } else {
      
      rank_correction=1
    }
    
    c=1
    
    for(objective in objectives){
      
      tempn500df=data.frame(r500ranks[[robust_type]])
      tempn500ranks=tempn500df[[objective]]
      
      iter1=T
      
      
      for(corr in corrs){
        
        col.name=paste(robust_type,objective,sep = '.')
        
        row=which.min(abs(SFD_corr[[col.name]]-corr))
        method=SFD_corr$method[row]
        n=SFD_corr$n[row]
        i=SFD_corr$iter[row]
        ni=paste0('n',n,'i',i,'.rds')
        tempcorr=SFD_corr[[col.name]][row]
        label=paste0(round(tempcorr, 3), '_',n)
        
        
        templist=readRDS(here('data', 'robustness lists', ni))#[[method]]  
        temprobustness=templist$clhs[[robust_type]][[objective]] # robustness scores for given metric, objective, method, n, and iter
        
        tempranks=rank(temprobustness*rank_correction, ties.method = 'min')
        
        add.df=data.frame(sample.rank=tempranks, n500.rank=tempn500ranks, corr=tempcorr, robust_type, objective, corr_n=label)
        
        if(iter1){
          rank.df=add.df
          iter1=F
        }else{
          rank.df=rbind(rank.df, add.df)
        }
        
      } # end corrs
      
      
      rank.df$corr=round(rank.df$corr,2)
      rank.df$corr=factor(rank.df$corr)
     
       rank.plot=ggplot()+
        geom_abline(intercept=0, slope=1, size=.6)+
        geom_point(data=rank.df, aes(y=sample.rank, x=n500.rank), color='forestgreen', size=3.5, alpha=.4)+
        ggtitle(objective)+
        #geom_line(aes(x=order(tempn500ranks), y=order(tempn500ranks)), size=1)+
        theme_minimal()+
        coord_cartesian(xlim=c(0,10), ylim=c(0,10))+
        scale_x_continuous(breaks=seq(0,10,2))+
        scale_y_continuous(breaks=seq(0,10,2))+
        ylab('subsample rank')+
        xlab('all scenarios rank')+
        ggtitle(objective)

      c=c+1
      
      robust_plots[[objective]]=rank.plot
      
    } # end objectives
    
    combined=ggpubr::ggarrange(plotlist = robust_plots,ncol=3,nrow=1, common.legend = F, align='v')
    
    combined=ggpubr::annotate_figure(combined, top=text_grob(robust_type))
    outfile=paste0('T10 rank diagram ', robust_type, '.tiff')
    
    ggexport(combined, filename = here('figures', outfile), width=2200*wf, height=1000*hf, pointsize = 12, res=400)
    
    
    plot_list[[robust_type]]=robust_plots
    
  }
  
