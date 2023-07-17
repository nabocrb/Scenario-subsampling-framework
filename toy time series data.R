
library(here)
library(ggplot2)


x=0:20


y=c(60, 52, 30, 30, 55, 58, 60, 60, 55, 50, 58, 45, 40, 35, 40, 45, 40, 60, 58, 60, 58)

yscaled=(y - min(y))/(max(y)-min(y))*100

yscaled[3:4]=yscaled[3:4]+15

data=data.frame(x,yscaled)

smoothed=as.data.frame(spline(x,yscaled))

fail=80

ggplot()+
  geom_line(smoothed, mapping=aes(x,y))+
  #geom_point(data, mapping=aes(x,yscaled))+
  theme_minimal()+
  geom_hline(yintercept=fail, linetype='dashed', size=1)+
  scale_x_continuous(breaks=seq(0,20,4),expand = expansion(add=0))+
  scale_y_continuous(breaks = seq(0,100,20), expand = expansion(add=4))+
  ylab('state variable')+
  xlab('time step')

ggsave()

reliability_time_steps=sum(smoothed$y<fail)
reliability=reliability_time_steps/nrow(smoothed)


tin=smoothed$x[34] # time step 34 is when failure begins
tout=smoothed$x[53] # time step 53 is when failiure ends
resiliency=tout-tin

vuln=fail-min(smoothed$y)



