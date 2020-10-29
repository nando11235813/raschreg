library(hexSticker)

x<-seq(-7,7,0.01)
ic1<- 1/(1+exp(-(x--3)))
ic2<- 1/(1+exp(-(x--2)))
ic3<- 1/(1+exp(-(x--1)))
ic4<- 1/(1+exp(-(x-0)))
ic5<- 1/(1+exp(-(x-1)))
ic6<- 1/(1+exp(-(x-2)))
ic7<- 1/(1+exp(-(x-3)))

dat<-data.frame(x=c(x,x,x,x,x,x,x),icc=c(ic1,ic2,ic3,ic4,ic5,ic6,ic7),cual=factor(rep(seq(7),each=length(x))))
library(ggplot2)
p<-ggplot(dat,aes(x=x,y=icc,color=cual))+
     geom_line(size=1) + xlab('') + ylab('') +
     theme_void() +
     theme(legend.position='none')

celeste <- rgb(222/255,235/255,247/255)

s <- sticker(p,
             package = "raschreg", p_size=25, s_x=0.95, s_y=1.1, s_width=1.6, s_height=1, p_y=0.55,
             p_color = 'navy', h_color = 'navy', h_fill = celeste,
             filename="hex_raschreg.png")

