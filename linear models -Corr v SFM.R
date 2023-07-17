# Scatter plots of subsampling experiments and robustness results
# Nathan Bonham
# April 2023

rm(list=ls())

library(here)
library(ggplot2)
library(dplyr)
library(ggpubr)

######## rank correlation and SFD metrics ############
SFD=read.table(here('data','scores', 'SFD scores.txt'))
corr=read.table(here('data','scores', 'rank correlation.txt'))
corr$method='cLHS'

SFD_corr=left_join(SFD, corr, by=c('method','n', 'iter'))
write.table(SFD_corr,here('data', 'scores', 'SFD_corr.txt'))

# load set of 500 scenarios, the baseline
scenarios=read.table(here('data', '500 scenarios.txt'))
keep_feature=c('Demand','Mead.PE','Powell.PE','Driest20yrAVG', 'Wettest20yrAVG', 'Driest2yrAVG', 'Wettest2yrAVG')
scenarios=scenarios[keep_feature]

source(here('helper functions.R'))

benchmark=SFD_metrics(scenarios)
benchmark=data.frame(method='all scenarios', n=500, corr=1,benchmark)


SF_metrics=c('mindist', "MSTmean", 'MSTsd')
# robust_metrics=c('satisficing.delivery', 'satisficing.storage', 'avg.M1000',
#                  'avg.LB.Dur', 'regret_best.M1000' ,'regret_best.LB.Dur',
#                  'maximin90.M1000' ,'maximin90.LB.Dur',
#                  'maximin100.M1000' ,'maximin100.LB.Dur')
robust_metrics=c('satisficing.M1000', 'satisficing.LB.Dur', 'satisficing.LB.Max', 'satisficing.aggregated','regret_best.M1000','regret_best.LB.Dur', 'regret_best.LB.Max')


combined_list=list()
b=1

for(robust_metric in robust_metrics){

x_range=c(min(SFD_corr[[robust_metric]])-.05, 1)
ymin=min(SFD_corr[[robust_metric]])
ymax=max(SFD_corr[[robust_metric]])


plot_list=list()
c=1


for(SFD_metric in SF_metrics){

  formula=as.formula(paste(robust_metric, SFD_metric, sep = "~"))
  temp.mod=lm(formula, data = SFD_corr)
  slope=temp.mod$coefficients[2]
  intercept=temp.mod$coefficients[1]
  eqn=paste0('y=', round(slope,2), "x", "+", round(intercept,2))
  R2=summary(temp.mod)$r.squared
  #add_text=paste0(eqn, ', ', "italic(R) ^ 2 =", round(R2,2))
  add_text=paste0(eqn, ', ', 'R2',"=", round(R2,2))
  
  xmin=(min(SFD_corr[[SFD_metric]]))
  xmax=max(SFD_corr[[SFD_metric]])
  
    temp.plot=ggplot(data=SFD_corr, aes(y=.data[[robust_metric]], x=.data[[SFD_metric]]))+
      geom_point(size=2, alpha=.5, color='grey50')+
      geom_smooth(method='lm', se=FALSE, color='black')+
      annotate(geom = 'text', x=xmax,y=ymax, label=add_text, parse=F, hjust=1, vjust=1, size=2.5)+
      #geom_point(data=benchmark, aes(x=corr, y=.data[[SFD_metric]]), size=4, shape=8)+ #, size=.data[[robust_metric]]
      theme_minimal()+
      theme(axis.title = element_text(size=8))+
      #scale_x_continuous(limits=x_range)+
      scale_color_gradient2(low='#edf8b1', mid = '#c7e9b4', high='#225ea8')+
      xlab(SFD_metric)+
      ylab('rank corr')
    
  if (c %in% 2:3){
    temp.plot=temp.plot+ylab(NULL)+
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  } 
  
  if(!(b %in% 19:21)){
    temp.plot=temp.plot+xlab(NULL)+
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }  
    
  b=b+1
      
  plot_list[[c]]=temp.plot
  c=c+1
  
}

combined=ggpubr::ggarrange(plotlist = plot_list,ncol=3,nrow=1, align='v')

#combined=ggpubr::annotate_figure(combined, top=text_grob(paste(robust_metric)))
outfile=paste0('Cor vs space filling-', robust_metric, '.tiff')

#ggexport(combined, filename = here('figures', outfile), width=480*6, height=480*2, pointsize = 12, res=400)

combined_list[[robust_metric]]=combined


}

all=ggpubr::ggarrange(plotlist = combined_list,ncol=1,nrow=7, align='v', heights = c(rep(1, 6), 1.4))


############################## MSTmean only ######################################
##################################################################################

y_range=c(.85, 1)
ymin=.85
ymax=1

y0=1
x0=benchmark$MSTmean

SFD_corr$MSTmean=SFD_corr$MSTmean/x0 # scale by MSTmean of all scenarios

x0=1


MSTlist=list()
c=1

for(robust_metric in robust_metrics){
  
  SFD_metric='MSTmean'

  formula=as.formula(paste(robust_metric, SFD_metric, sep = "~"))
  temp.mod=lm(formula, data = SFD_corr)
  slope=temp.mod$coefficients[2]
  intercept=temp.mod$coefficients[1]
  eqn=paste0('y=', round(slope,2), "x", "+", round(intercept,2))
  R2=summary(temp.mod)$r.squared
  #add_text=paste0(eqn, ', ', "italic(R) ^ 2 =", round(R2,2))
  add_text=paste0(eqn, ', ', 'R2',"=", round(R2,2))
  
  xmin=(min(SFD_corr[[SFD_metric]]))
  xmax=max(SFD_corr[[SFD_metric]])
  
  temp.plot=ggplot(data=SFD_corr, aes(y=.data[[robust_metric]], x=.data[[SFD_metric]]))+
    geom_point(size=2, alpha=.5, color='grey50')+
    geom_abline(slope=slope, intercept = intercept, color='black')+
    #geom_smooth(method='lm', se=FALSE, color='black')+
    annotate(geom='point', x=x0, y=1, color='black', size=2)+
    annotate(geom = 'text', x=xmin,y=ymin, label=add_text, parse=F, hjust=0, vjust=0, size=3)+
    #geom_point(data=benchmark, aes(x=corr, y=.data[[SFD_metric]]), size=4, shape=8)+ #, size=.data[[robust_metric]]
    theme_minimal()+
    theme(axis.title = element_text(size=8))+
    coord_cartesian(ylim=y_range)+
    #scale_x_continuous(limits=x_range)+
    scale_color_gradient2(low='#edf8b1', mid = '#c7e9b4', high='#225ea8')+
    xlab(SFD_metric)+
    ylab('corr')
  
  if (c %in% c(2:4, 6:8)){ # no y
    temp.plot=temp.plot+ylab(NULL)+
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  } 
  
  if(c %in% 1:4){ # no x 
    temp.plot=temp.plot+xlab(NULL)+
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }  
  
  MSTlist[[c]]=temp.plot
  c=c+1
  
}

MSTplot=ggpubr::ggarrange(plotlist = MSTlist,ncol=4,nrow=2, align='v', heights = c(1,1.2))
MSTplot

##################### summary plot ######################################
########################################################################

long=tidyr::pivot_longer(SFD_corr, cols = !c('method', 'n', 'iter','mindist', 'MSTmean', 'MSTsd'),
                         names_to = 'metric.objective',values_to =  'corr')
long$metric=gsub("\\..*","", long$metric.objective )
long$objective=gsub("^.*?\\.","", long$metric.objective )

# long$group=NA
# 
# long$group[long$metric.objective %in% c("avg.M1000", "avg.LB.Dur", "avg.LB.Max")]='mean'
# long$group[long$metric.objective == "satisficing.storage"]='reliability, satisficing'
# long$group[long$metric.objective == "regret_best.M1000"]='reliability, regret'
# long$group[long$metric.objective == "regret_best.LB.Dur"]='resiliency, regret'
# long$group[long$metric.objective == "regret_best.LB.Max"]='vulnerability, regret'

#long.filter=long[!is.na(long$group),]

long.filter=dplyr::filter(long, metric.objective != 'satisficing.aggregated')

save_slope=c()
save_intercept=c()
save_group=c()
save_R2=c()
formula=as.formula("corr ~ MSTmean")

iter1=T

for(mo in unique(long.filter$metric.objective)){ # get slope, intercept, R2 for each group
  
  temp.long=filter(long.filter, metric.objective==mo)
  temp.mod=lm(formula, data = temp.long)
  slope=temp.mod$coefficients[2]
  intercept=temp.mod$coefficients[1]
  R2=summary(temp.mod)$r.squared
  temp.95=predict(temp.mod, interval = 'prediction', level = .95)
  temp.90=predict(temp.mod, interval = 'prediction', level = .90)
  residuals=temp.mod$residuals
  
  add.df=data.frame(temp.long, temp.95, temp.90, residuals)
  
  if(iter1){
    save.df=add.df  
    iter1=F
  } else {
    save.df=rbind(save.df,add.df)
  }
  
  
  save_slope=c(save_slope,slope)
  save_intercept=c(save_intercept, intercept)
  save_group=c(save_group, mo)
  save_R2=c(save_R2, R2)
  
}

summary_models=data.frame(group=save_group, slope=save_slope, intercept=save_intercept, R2=save_R2)

save.df$metric.objective=factor(save.df$metric.objective, 
                                levels = c('satisficing.M1000', 'satisficing.LB.Dur', 'satisficing.LB.Max', 'regret_best.M1000', 'regret_best.LB.Dur', 'regret_best.LB.Max'))


save.df$metric=factor(save.df$metric, levels = c('satisficing', 'regret_best'))
save.df$objective1=factor(save.df$objective, levels = c('M1000', 'LB.Dur', 'LB.Max'))

save.df[["robust.type"]]=save.df$metric
save.df[['objective']]=NA

save.df$objective[save.df$objective1 == 'M1000']='reliability'
save.df$objective[save.df$objective1 == 'LB.Dur']='resiliency'
save.df$objective[save.df$objective1 == 'LB.Max']='vulnerability'

save.df$objective=factor(save.df$objective, levels=c('reliability', 'resiliency', 'vulnerability'))

summary_plot=ggplot(data=save.df, aes(x=MSTmean, y=fit, color=objective, linetype=robust.type))+ #, color=group, fill=group
  #geom_smooth(method='lm', se = F)+
  geom_line(size=1)+
  theme_minimal()+
  scale_y_continuous(limits = c(.9,1), breaks=seq(.9,1,length.out=6))+
  scale_x_continuous(limits = c(1,2.2), breaks=seq(1,2.2,length.out=7))+
  scale_color_brewer(type='qual', palette = 'Dark2')+
  scale_fill_brewer(type='qual', palette = 'Dark2')+
  coord_cartesian(xlim=c(1,2))+
  theme(legend.position = c(.35,.2), legend.box='horizontal',
        legend.background = element_rect(fill="transparent", color = "transparent"), title = element_text(size=10))+
  ggtitle('Rank correlation vs MSTmean')+
  ylab('correlation')
summary_plot

prediction_intervals=ggplot(data=save.df, aes(x=MSTmean, y=fit, color=objective, fill=objective, linetype=robust.type))+ #, color=group, fill=group
  #geom_smooth(method='lm', se = F, show.legend = F)+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.2, show.legend = F)+
  theme_minimal()+
  #scale_y_continuous(limits = c(.9,1), breaks=seq(.9,1,length.out=6))+
  #scale_x_continuous(limits = c(1,2.2), breaks=seq(1,2.2,length.out=7))+
  #coord_cartesian(xlim=c(1,2))+
  scale_color_brewer(type='qual', palette = 'Set2')+
  scale_fill_brewer(type='qual', palette = 'Set2')+
  facet_wrap(~metric.objective)+
  ggtitle('95% prediction intervals')+
  theme(title = element_text(size=10),  strip.background = element_blank(),
        strip.text.x = element_blank())
  
prediction_intervals

residuals=ggplot(data=save.df, aes(x=fit, y=residuals))+ #, color=group, fill=group
  geom_point(color='gray50', alpha=.5)+
  geom_smooth(se=F)+
  theme_minimal()+
  #scale_y_continuous(limits = c(.9,1), breaks=seq(.9,1,length.out=6))+
  #scale_x_continuous(limits = c(1,2.2), breaks=seq(1,2.2,length.out=7))+
  #coord_cartesian(xlim=c(1,2))+
  theme(legend.title = element_blank(), legend.position = c(.2,.2))+
  facet_wrap(~metric.objective)+
  xlab('predicted correlation')+
  ggtitle('Residuals vs Predictions')+
  theme(title = element_text(size=10))
residuals

residuals_hist=ggplot(data=save.df, aes(x=residuals))+ #, color=group, fill=group
  geom_histogram()+
  theme_minimal()+
  #scale_y_continuous(limits = c(.9,1), breaks=seq(.9,1,length.out=6))+
  #scale_x_continuous(limits = c(1,2.2), breaks=seq(1,2.2,length.out=7))+
  #coord_cartesian(xlim=c(1,2))+
  facet_wrap(~metric.objective)+
  xlab('residuals')
residuals_hist


# slope conversion

# #### force model to go through y=1, x0 = 1 (assume will converge to that of all scenarios)
# 
# for(robust_metric in robust_metrics){
#   
#   SFD_metric='MSTmean'
#   
#   formula=as.formula(paste0("I(",robust_metric,"-",y0,")", "~", "I(",SFD_metric,"-",x0,")","+", 0))
#   
#   # in formula above, I subtract y0 and x0, and force the intercept to be 0. This forces lm to find
#   # a model that goes through the point y0 x0. Below, I then use the calculated slope and known x0 and y0 
#   # to obtain the y intercept. 
#   
#   #formula=as.formula(paste(robust_metric, SFD_metric, sep = "~"))
#   temp.mod=lm(formula, data = SFD_corr)
#   slope=temp.mod$coefficients[1]
#   #slope=temp.mod$coefficients[2]
#   intercept=y0-slope*(x0)
#   
#   predicted=slope*SFD_corr[[SFD_metric]]+intercept
#   actual=SFD_corr[[robust_metric]]
#   R2=cor(predicted,actual)^2
#   
#   #intercept=temp.mod$coefficients[1]
#   eqn=paste0('y=', round(slope,2), "x", "+", round(intercept,2))
#   #R2=summary(temp.mod)$r.squared
#   #add_text=paste0(eqn, ', ', "italic(R) ^ 2 =", round(R2,2))
#   add_text=paste0(eqn, ', ', 'R2',"=", round(R2,2))
#   
#   xmin=(min(SFD_corr[[SFD_metric]]))
#   xmax=max(SFD_corr[[SFD_metric]])
#   
#   temp.plot=ggplot(data=SFD_corr, aes(y=.data[[robust_metric]], x=.data[[SFD_metric]]))+
#     geom_point(size=2, alpha=.5, color='grey50')+
#     geom_abline(slope=slope, intercept = intercept, color='black')+
#     #geom_smooth(method='lm', se=FALSE, color='black')+
#     annotate(geom='point', x=x0, y=1, color='black', size=2)+
#     annotate(geom = 'text', x=xmin,y=ymin, label=add_text, parse=F, hjust=0, vjust=0, size=3)+
#     #geom_point(data=benchmark, aes(x=corr, y=.data[[SFD_metric]]), size=4, shape=8)+ #, size=.data[[robust_metric]]
#     theme_minimal()+
#     theme(axis.title = element_text(size=8))+
#     coord_cartesian(ylim=y_range)+
#     #scale_x_continuous(limits=x_range)+
#     scale_color_gradient2(low='#edf8b1', mid = '#c7e9b4', high='#225ea8')+
#     xlab(SFD_metric)+
#     ylab('corr')
#   
#   if (c %in% c(2:4, 6:8)){ # no y
#     temp.plot=temp.plot+ylab(NULL)+
#       theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
#   } 
#   
#   if(c %in% 1:4){ # no x 
#     temp.plot=temp.plot+xlab(NULL)+
#       theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
#   }  
#   
#   MSTlist[[c]]=temp.plot
#   c=c+1
#   
# }
# 
# satisficing=MSTlist[[9]]
# MSTplot=ggpubr::ggarrange(plotlist = MSTlist,ncol=4,nrow=2, align='v')
# MSTplot_point=MSTplot