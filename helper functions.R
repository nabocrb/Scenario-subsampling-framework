
library(clhs)
library(DiceDesign) # space filling metrics
library(FNN) # fast nearest neighbor search, used in FSCS
library(lhs) # for runifint()
library(Kendall)
library(ggplot2)
library(plotly)

################## helper functions ##################

scale01=function(x){ # scale data 0-1
  new_val=(x-min(x))/(max(x)-min(x))
  return(new_val)
}

SFD_metrics=function(design){ # wrapper function to compute all metrics and format output
  scaled_data=apply(design, MARGIN = 2, FUN=scale01)
  
  #discrep=discrepancyCriteria(scaled_data, type='Mix2') # Mixture discrepancy at recommendation of Zhou et al. 2013 and Fang et al. 2018
  #df=list2DF(discrep)
  
  MST=mstCriteria(scaled_data)
  
  df=data.frame(
    mindist=mindist(scaled_data),
    MSTmean=MST$stats[1],
    MSTsd=MST$stats[2]
  )

  return(df)
  
}

robustness=function(data=objectives, keepSOW=1:500){
  
  #obj_names=c('LF.Def', 'P.WYR', 'P3490', 'M1000', 'LB.AVG', 'LB.Freq', 'LB.Max', 'LB.Dur')
  obj_names=c('M1000', 'LB.Dur','LB.Max')

    # filter data by keepSOW
  
  filter.data=dplyr::filter(data, TraceNumber %in% keepSOW )
  
  robust.list=list()
  
  # satisficing M1000
  sM1000=satisficing(data=filter.data, objectives = c('M1000'), thresholds = 10) # 10 was used in A2018 and Bonham 2022
  
  # satisficing LB.Dur
  sLBDur=satisficing(data=filter.data, objectives = c('LB.Dur'), thresholds = 10) # 10 was used in Bonham 2022
  
  # satisficing LB.Max
  sLBMax=satisficing(data=filter.data, objectives = c('LB.Max'), thresholds = 1375) #1375 is max combined 'shortage' under IG+Min323+DCP (see Fig 4 Bonham 2022)
  
  #satisficing aggregated
  sAll=satisficing(data=filter.data, objectives = c('M1000', 'LB.Dur', 'LB.Max'), thresholds = c(10,10, 1375))
  
  robust.list[['satisficing']]=data.frame(policy=sM1000$policy, M1000=sM1000$satisficing, LB.Dur=sLBDur$satisficing, LB.Max=sLBMax$satisficing, aggregated=sAll$satisficing)
  
  # mean
  
  #robust.list[['avg']]=LaplacePIR(data=filter.data, objectives=obj_names)
  
  # 90% regret from best
  
  robust.list[['regret_best']]=regret2(data=filter.data, objectives = obj_names, best_if = rep("min", length(obj_names)),
                                       SOW_agg_method = 'percentile',percentile = 0.9, scale = F, Obj_agg_method = 'none')$regret2
  
  # 90% maximin
  #robust.list[['maximin90']]=maximin(data=filter.data, objectives = obj_names,percentile = rep(90, length(obj_names)))
  
  # 100% maximin
  #robust.list[['maximin100']]=maximin(data=filter.data, objectives = obj_names,percentile = rep(100, length(obj_names)))
  
  # percent deviation from optimization
  # 
  # robust.list[['dev_opt']]=percent_deviation(data=filter.data, baseline = MOEA_data, objectives = obj_names, percentile = 90,
  #                                            max_objectives = 'none')
  # 
  return(robust.list)
  
  
}

# test data
#df1=robust500$maximin90[,-1]
#df2=robustness(data=objectives, keepSOW=200:299)$maximin90[,-1]

robustness_ranks=function(robustness_list, ties='average'){ # loop through list of computed robustness metrics (output from robustness() function), compute ranks
  
  ranks_list=list()
  
  for(n in names(robustness_list)){
    
    temp.df=robustness_list[[n]][,-1]
    correction=ifelse(n=='satisficing',-1,1) # if satisficing, apply -1 correction factor (maximization)
    temp.rank=apply(correction*temp.df, 2, FUN = rank, ties.method=ties)
    ranks_list[[n]]=temp.rank  
    
  }
  
  return(ranks_list)
  
}


#baseline_ranks=robustness_ranks(robust500)
#list2=robustness(keepSOW = 200:299)

rank_corr=function(baseline_ranks, list2){
  
  list2_ranks=robustness_ranks(list2)
  
  check=sum(names(baseline_ranks)!=names(list2_ranks))
  if(check>0){stop('mismatch in robustnes metric lists')}
  
  #corr_list=list()
  iter1=T
  
  for(n in names(baseline_ranks)){ # loop robustness metrics
    
    xmat=data.frame(baseline_ranks[[n]])
    ymat=data.frame(list2_ranks[[n]])
    
    corr.df=data.frame(matrix(NA, nrow=1, ncol=ncol(xmat)))
    colnames(corr.df)=paste(rep(n,ncol(xmat)), colnames(xmat), sep = '.')
    
    for(c in 1:ncol(xmat)){ # loop objectives
    
      x=xmat[[c]]; y=ymat[[c]]
      corr.df[1,c]=Kendall(x,y)$tau

    }
  
  
    if(iter1){
      save.df=corr.df; iter1=F    
    } else {
      save.df=data.frame(save.df, corr.df)
    }
    
    #corr_list[[n]]=corr.df
          
  }

  return(save.df)
  
}


#test=rank_corr(baseline_ranks, list2)

#list1=robust500
#list2=robustness(keepSOW = 200:299)

rel_difference=function(list1, list2){

  check=sum(names(list1)!=names(list2))
  if(check>0){stop('mismatch in robustnes metric lists')}
  
  #difference=list()
  iter1=T
  
  for(n in names(list1)){ # loop robustness metrics
    
    xmat=data.frame(list1[[n]][,-1])
    ymat=data.frame(list2[[n]][,-1])
    
    diff.df=data.frame(matrix(NA, nrow=1, ncol=ncol(xmat)))
    colnames(diff.df)=paste(rep(n,ncol(xmat)), colnames(xmat), sep = '.')
    
    for(c in 1:ncol(xmat)){ # loop objectives
      
      x=xmat[[c]]; y=ymat[[c]]
      #temp.diff=abs(x-y)/((x+y)/2)*100 # percent difference relative to average. Because my data has many zeros in it, this denominator results in NA
      temp.diff=x-y # use simple difference instead
      diff.df[1,c]=mean(temp.diff) # difference averaged over all policies
      
    }
    
    if(iter1){
      save.df=diff.df; iter1=F    
    } else {
      save.df=data.frame(save.df, diff.df)
    }
    
    
    #difference[[n]]=diff.df
    
  }
  
  return(save.df)
  
}

### reservoir operation diagrams

# import Lake Mead DV values

bar_plot_data=readRDS(here("data",'data for stacked bar plot.rds'))
long_data=bar_plot_data$long_data
wide_data=bar_plot_data$wide_data
wide_data$ID=1:nrow(wide_data)

# function to change ggplot legend size: https://stackoverflow.com/questions/52297978/decrease-overal-legend-size-elements-and-text
addSmallLegend <- function(myPlot=bar_plot, pointSize = 0.25, textSize = 8, spaceLegend = 0.03) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_blank(), #element_text(size = textSize)
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}

DV_plot=function(long.data=long_data, wide.data=wide_data, metric.df= NULL, metric_name,
                 xlabel='', ylabel='', ID_label="ID", preferred_direction='max', y_axis2=F,
                 labelsize=3, shrink_legend=F, interactive=F, volume_labs=F, v_lab_nudge=-4, xmax=10){
  

  filter.long=long.data; filter.wide=wide.data
  
    # filter for chosen policies
#  filter.long=dplyr::filter(long.data, ID %in% to_plot)
#  filter.wide=dplyr::filter(wide.data, ID %in% to_plot)
  
  # get rank, append to data frames
  
  correction=ifelse(preferred_direction == 'min', 1, -1) # to handle metrics that should be minimized and maximizied accordingly in the ranking
  decreasing=ifelse(preferred_direction == 'min', F, T) # needed for add_lines in plot function
  
  if (!is.null(metric.df)){
    
    filter.metric.df=metric.df
    #filter.metric.df=dplyr::filter(metric.df, ID %in% to_plot)
    filter.metric.df$rank=rank(correction*filter.metric.df[[metric_name]], ties.method = 'first')
    
  }
  
  filter.wide$rank=if(is.null(metric.df)){1:length(to_plot)} else {filter.metric.df$rank}
  filter.long$rank=NA
  for(i in 1:length(filter.long$rank)){ # for loop to avoid errors when different number of tiers
    filter.long$rank[i]=filter.wide$rank[which(filter.wide$ID==filter.long$policy[i])]
  }
  
  
  ############################# plotting ################################
  text_size=labelsize
  
  n_policies=nrow(filter.wide)
  
  filter.long$volume[filter.long$volume==0]=NA
  
  breaks=seq(0,2400, length.out=13)
  breaks[length(breaks)]=2401
  my_labs= c("[0-200)","[200-400)", "[400-600)", "[600-800)", "[800-1000)", "[1000-1200)","[1200-1400)",
             "[1400-1600)","[1600-1800)", "[1800-2000)", "[2000-2200)", "[2200-2400]")
  filter.long$vol_group=cut(filter.long$volume, breaks = breaks, include.lowest = T, right = F, labels = my_labs)
  
  color_func=colorRampPalette(colors=c("greenyellow","lightgoldenrod1", "goldenrod1", "darkorange1", "maroon2","firebrick3"))
  my_cols=color_func(12)
  names(my_cols)=my_labs
  
  
  bar_plot=ggplot()+
    geom_bar(data=filter.long, aes(fill=vol_group,x=rank, y=delta), position = 'stack', stat = 'identity', color='darkgrey')+ 
    # geom_text(data=filter.wide, aes(x=rank, y= policy_lab_y, label=SOM_node), nudge_y = 10, size=text_size, check_overlap = T)+ I have removed SOM node for NOW
    scale_fill_manual(values=my_cols, na.value="black", name="Volume [KAF]", na.translate=F, drop=F)+
    xlab(xlabel)+
    ylab(ylabel)+
    theme_minimal()+
    theme(plot.title = element_text(size=10), plot.margin=margin(t=0, r=0, b=0, l=0, unit='pt'))+
    coord_cartesian(ylim=c(905,1110), xlim=c(0.5, min((n_policies+.5),xmax)))
  
  if(!is.null(ID_label)){
    
    bar_plot=bar_plot+
      geom_text(data=filter.wide, aes_string(x="rank", y= "policy_lab_y", label=ID_label), nudge_y = 4, size=text_size, check_overlap = T)
    
  }
  
  
  if(volume_labs){bar_plot=bar_plot+geom_text(data=filter.long, aes(x=rank, y= elevation, label=v_lab),color='black', nudge_y = v_lab_nudge, size=text_size, check_overlap = F)}
  
  if(is.null(metric.df)){bar_plot=bar_plot+theme(axis.ticks.x=element_blank(), axis.text.x = element_blank())}
  
  if(shrink_legend){bar_plot=addSmallLegend(bar_plot)}
  
  
  # ggtitle(paste(metric_name, 'rank for selected policies', sep=' '))+
  
  
  if(interactive){
    
    int_plot=ggplotly(p=bar_plot, tooltip = c('rank','elevation', 'volume'), dynamicTicks=T, originalData=F) # convert ggplot to interactive plotly html
    # add legend title in correct location
    int_plot=int_plot %>%   layout(legend = list(
      orientation = "v", title=list(text=" Tier "))
    )
    
    if (y_axis2==T & !is.null(metric.df)){ # only add second y axis if y_axis2==T and a metric was given
      
      int_plot_2y=int_plot %>%
        add_lines(data=filter.metric.df, x=~sort(filter.metric.df$rank), y=~sort(filter.metric.df[[metric_name]], decreasing = decreasing), yaxis='y2',
                  inherit=FALSE, showlegend=FALSE, line=list(color='black', width=2, dash='dash')) %>%
        layout(yaxis2 = list(overlaying = "y", side = "right",
                             tickfont = list(color = 'black', size=10), color = 'black',
                             title = metric_name, showgrid=F), 
               legend = list(x = 1.05, y = 0.95), xaxis=list(range=c(0, min((n_policies+1),xmax))), yaxis=list(range=c(885,1110))
        )
      
    } else { # do not add second axis.
      
      int_plot_2y=int_plot %>%
        layout(legend = list(x = 1.05, y = 0.95), xaxis=list(range=c(0, min((n_policies+1),xmax)), title=""), yaxis=list(range=c(885,1110)))
      
    }
    
    
    int_plot_2y$x$layout$xaxis$autorange = FALSE # need to tell plotly to NOT change the axis range to fit all data
    int_plot_2y$x$layout$yaxis$autorange = FALSE
    return(int_plot_2y)
    
  } else {
    return(bar_plot) # not interactive
  }
  
  
}


