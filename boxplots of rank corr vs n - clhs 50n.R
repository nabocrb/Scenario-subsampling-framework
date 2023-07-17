# grouped boxplots of rank correlation vs n
# Nathan Bonham
# May 2023

rm(list=ls())
library(here)
library(ggplot2)

accurate_threshold=0.975

######## rank correlation and SFD metrics ############

SFD_corr=read.table(here('data', 'scores', 'SFD_corr.txt'))
SFD_corr$n=factor(SFD_corr$n, levels=seq(50, 500, 50))

long=tidyr::pivot_longer(SFD_corr, cols=!c('method', 'n', 'iter', 'mindist','MSTmean','MSTsd'), 
                         names_to = 'robust.metric', values_to = 'rank.corr')

long$metric=gsub("\\..*","", long$robust.metric )
long$objective=gsub("^.*?\\.","", long$robust.metric )

long=dplyr::filter(long, objective != 'aggregated')

long$objective1=factor(long$objective, levels = c('M1000', 'LB.Dur', 'LB.Max'))

long[["robust.type"]]=long$metric
long[['objective']]=NA

long$objective[long$objective1 == 'M1000']='reliability'
long$objective[long$objective1 == 'LB.Dur']='resiliency'
long$objective[long$objective1 == 'LB.Max']='vulnerability'

long$objective=factor(long$objective, levels=c('reliability', 'resiliency', 'vulnerability'))
long$robust.type=factor(long$robust.type, levels=c('satisficing', 'regret_best'))

x_order=seq(50, 450,50)
#keep=c(10, seq(25, 500, 25))
keep=seq(50, 450,50)
xlabs=c()
for(i in x_order){

  if (i %in% keep){
    xlabs=c(xlabs, as.character(i))
  }  else {
    xlabs=c(xlabs, "")
  }

}

x_order=as.factor(x_order)

ggplot(data=long, aes(x=n, y=rank.corr, color=objective))+
  geom_hline(aes(yintercept=accurate_threshold), size=.5)+
  geom_boxplot(fill='gray97' ,size=.64, outlier.size = .75)+
  #geom_point(aes(x=as.factor(500), y=1), color='black', size=3,shape=8)+
  scale_x_discrete(limits=x_order,breaks=x_order, labels=xlabs)+
  coord_cartesian(ylim=c(.85,1))+
  #scale_y_continuous(limits = c(.8, 1))+
  #scale_x_discrete(limits=x_order)+
  #scale_fill_manual(values=my_cols)+
  scale_color_brewer(type='qual', palette = 'Dark2')+
  #scale_fill_brewer(type='qual', palette = 'Set3')+
  ylab('correlation')+
  xlab('n')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=0), legend.position = 'right', plot.title = element_text(size=10) )+
  facet_wrap(~robust.type, nrow = 2)+
  ggtitle('Rank correlation vs Number scenarios')

ggsave(here('figures', 'corr v n scenarios.png'), device = 'png', height = 4.5, width = 6.5, units = 'in', dpi = 300)

# 
# plot_list=list()
# 
# metrics=c('satisficing', 'regret_best')
# 
# c=1
# 
# for(m in metrics){
#   
#   # if(m == 'satisficing'){
#   #   my_cols=c('#66c2a5', '#fc8d62')
#   # } else {
#   #   my_cols=c('#8da0cb','#e78ac3','#a6d854','#fdb462')##ffd92f
#   # }
#   
#   filter.long=dplyr::filter(long, metric == m )
#   
#   temp=ggplot()+
#     geom_hline(aes(yintercept=accurate_threshold), size=.5)+
#     geom_boxplot(data=filter.long, aes(x=n, y=rank.corr, color=objective), fill='gray97' ,size=.64, outlier.size = .75)+
#     #geom_point(aes(x=as.factor(500), y=1), color='black', size=3,shape=8)+
#     scale_x_discrete(limits=x_order,breaks=x_order, labels=xlabs)+
#     coord_cartesian(ylim=c(.84,1))+
#     #scale_y_continuous(limits = c(.8, 1))+
#     #scale_x_discrete(limits=x_order)+
#     #scale_fill_manual(values=my_cols)+
#     scale_color_brewer(type='qual', palette = 'Dark2')+
#     #scale_fill_brewer(type='qual', palette = 'Set3')+
#     ggtitle(m)+
#     ylab('correlation')+
#     xlab('n')+
#     theme_minimal()+
#     theme(axis.text.x = element_text(angle=0), legend.position = 'right', plot.title = element_text(size=10) )
#   
#   #if(c==3){ # keep both
#    # plot_list[[m]]=temp
#     
#   #} else if (c ==2){ # remove x and y
#     
#    # temp=temp+scale_x_discrete(limits=x_order, labels=NULL)+xlab(NULL)+
#     #  scale_y_continuous(limits = c(.8, 1), labels=NULL)+ylab(NULL)
#     
#     #plot_list[[m]]=temp
#     
#   #} else if (c==1){ # keep y, no x
#    # plot_list[[m]]=temp+ scale_x_discrete(limits=x_order, labels=NULL)+xlab(NULL)
# 
#   #} else { # c==4, keep x, no y
#   #  plot_list[[m]]=temp+scale_y_continuous(limits = c(.8, 1), labels=NULL)+ylab(NULL)
#     
#   #}
#   
#   if (c %in% c(1,2)){
#     temp=temp+scale_x_discrete(limits=x_order, labels=NULL)+xlab(NULL)
#     
#     if(c==1){
#       temp=temp+ylab(NULL)
#     }
#     
#   } else {
#     temp=temp+ylab(NULL)
#   }
#   
#   plot_list[[m]]=temp
#   
#   c=c+1
#   
# }
# 
# combined=ggpubr::ggarrange(plotlist = plot_list, nrow=3,ncol=1, common.legend = F, legend = 'right', align='v', heights = c(.82,.82,1))
# combined
