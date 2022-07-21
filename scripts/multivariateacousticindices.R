# explore long-duration acoustic indices -----

library(tidyverse)
# library(purrr)
library(lubridate)
library(ggsidekick)

# run if not done before
# source("scripts/organize-acoustic-indices.R") 

# set these for a specific machine
# where to find the compiled dataframes?
output_parent_directory <- "data/"


# choose which data set/sample to compile
site_file_name <- "denman"
# site_file_name <- "collishaw"
# site_file_name <- "neckpt"

d <- readRDS(paste0(output_parent_directory, "towsey-summary-scores.rds")) 

glimpse(d)
d2 <- d %>% pivot_longer(1:23, names_to = "index_type", values_to = "score")


## making a multivariate plot
# data matrix
dm<-d2%>%
  filter(samp.tot.sec!=60)%>%
  mutate(score = (score-min(score))/(max(score)-min(score)))%>%
  select(datetime,site,herring.hs,boat,waves,index_type,score)%>%
  pivot_wider(names_from=index_type,values_from=score)

# just the indices
im<-dm%>%
  select(-datetime,-site,-herring.hs,-boat,-waves)

# the "environmental" variables
em<-dm%>%
  select(site,herring.hs,boat,waves)

# index NMDS
library(vegan)
irda<-rda(im~.,data=em)
plot(irda)

(sR2a.all <- RsquareAdj(irda)$adj.r.squared)
(soveral.rda<-anova(irda,pemutations=how(nperm=999)))
(saxis.sig<-anova(irda, by="axis", permutations=how(nperm=999)))
# make a try without list or correlated indices to remove
# see if it improves anything
rm.ind<-c("TemporalEntropy","AvgSnrOfActiveFrames","ClippingIndex",
          "EntropyofVarianceSpectrum","EventsperSecond",
          "EventsPerSecond","HighFreqCover","SptDensity",
          "ThreeGramCount","AvgSignalAmplitude")
im2<-im[,!colnames(im)%in%rm.ind]
irda2<-rda(im2~.,data=em)
plot(irda2)

(sR2a.all2 <- RsquareAdj(irda2)$adj.r.squared)
(soveral.rda2<-anova(irda2,pemutations=how(nperm=999)))
(saxis.sig2<-anova(irda2, by="axis", permutations=how(nperm=999)))


irda2.scores<-data.frame(scores(irda2,choices = c(1,2,3),display="sites",scaling = 3))

em2<-bind_cols(em,irda2.scores)
# theme_set(theme_bw()+theme(panel.grid = element_blank()))
# 
# ggplot(em2,aes(x=RDA1,y=RDA3,color=as.factor(herring.hs)))+
#   geom_point(alpha=.5)+
#   scale_color_viridis_d(option = "C",begin=.0,end=.8)

ind.sc <- scores(irda2, choices=1:3, display="sp",scaling=3)
ind.good <- goodness(irda2)
(ind.sc1<-data.frame(ind.sc[which(ind.good[,1]>=0.4),])%>%
    mutate(index=row.names(.)))
(ind.sc2<-data.frame(ind.sc[which(ind.good[,2]>=0.4),])%>%
    mutate(index=row.names(.)))
(ind.sc3<-data.frame(ind.sc[which(ind.good[,3]>=0.4),])%>%
    mutate(index=row.names(.)))
ind.sc.use<-bind_rows(ind.sc1,ind.sc2,ind.sc3)%>%
  distinct()
# 
# ggplot()+
#   geom_point(data=em2,aes(x=RDA1,y=RDA2,color=as.factor(herring.hs)),alpha=.2)+
#   geom_segment(data=ind.sc.use,aes(x=0,y=0,xend=RDA1,yend=RDA2,color=index),linetype="dashed")+
#   scale_color_viridis_d()

library(plotly)

fig <- plot_ly(dm, x = ~Snr, y = ~LowFreqCover, z = ~ClusterCount, color = ~as.factor(herring.hs),alpha=.5,colors=c("#0D0887FF", "#B12A90FF" ,"#FCA636FF"))
fig <- fig %>% add_markers()
fig
# 
# p1<-ggplot(dm)+
#   geom_point(aes(x=Snr,y=LowFreqCover,color=as.factor(herring.hs)),alpha=.3)+
#   scale_color_viridis_d(option = "C",end=.8)
# p2<-ggplot(dm)+geom_point(aes(x=Snr,y=ClusterCount,color=as.factor(herring.hs)),alpha=.3)+
#   scale_color_viridis_d(option = "C",end=.8)
# p3<-ggplot(dm)+geom_point(aes(x=ClusterCount,y=LowFreqCover,color=as.factor(herring.hs)),alpha=.3)+
#   scale_color_viridis_d(option = "C",end=.8)
# 
# library(patchwork)
# p1/p2/p3

# write out dataset for false color code  
write_rds(em2,"data/multivariate_output_for_falsecolor.rds")

# try making a tile figure out of multivariate
em3<-data.frame(datetime=dm$datetime,em2,Snr=dm$Snr,
                ClusterCount=dm$ClusterCount,
                LowFreqCover=dm$LowFreqCover)%>%
  mutate(r=scales::rescale(RDA1,to=c(0,1)),
         g=scales::rescale(RDA2,to=c(0,1)),
         b=scales::rescale(RDA3,to=c(0,1)),
         r2=scales::rescale(Snr,to=c(0,1)),
         g2=scales::rescale(ClusterCount,to=c(0,1)),
         b2=scales::rescale(LowFreqCover,to=c(0,1)),
         dom=day(datetime),
         hr=hour(datetime),
         hrm=hour(datetime)+(minute(datetime)/60),
         hrm2=hms(paste(hour(datetime),minute(datetime),"00")),
         col=rgb(r, g, b, maxColorValue = 1),
         col2=rgb(r2,g2,b2,maxColorValue = 1))

tid<-data.frame(hrm=unique(em3$hrm))%>%
  arrange(hrm)%>%
  mutate(tid=row_number())

em3<-left_join(em3,tid)%>%
  group_by(hr)%>%
  mutate(hid=ifelse(hrm==min(hrm),1,0))

yalab<-em3%>%
  select(tid,hr,hid)%>%
  filter(hid==1)%>%
  distinct()%>%
  filter(hr %in% c(0,5,10,15,20))

theme_set(theme_bw()+
            theme(panel.grid = element_blank(),
                  panel.background = element_rect(fill="grey"),
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust=0.5)))

(df<-ggplot(data=em3%>%
         filter(site=="Denman (2020)"))+
  geom_tile(aes(x=dom,y=tid,fill=col2))+
  scale_fill_identity()+
  scale_y_continuous(name="Hour",breaks=yalab$tid,labels=yalab$hr,limits=c(0,768),expand = c(0,0))+
  xlab("Date (March)")+
  ggtitle("Denman",subtitle="False Color (SNR, Cluster Count, Low Frequency Cover)"))

(dh<-ggplot(data=em3%>%
                filter(site=="Denman (2020)"))+
    geom_tile(aes(x=dom,y=tid,fill=as.factor(herring.hs)))+
    scale_y_continuous(name="Hour",breaks=yalab$tid,labels=yalab$hr,limits=c(0,768),expand = c(0,0))+
    xlab("Date (March)")+
    ggtitle("",subtitle="Herring score"))

(db<-ggplot(data=em3%>%
              filter(site=="Denman (2020)"))+
    geom_tile(aes(x=dom,y=tid,fill=as.factor(boat)))+
    scale_y_continuous(name="Hour",breaks=yalab$tid,labels=yalab$hr,limits=c(0,768),expand = c(0,0))+
    xlab("Date (March)")+
    ggtitle("",subtitle="Herring score")+
    theme(legend.position = "left"))

library(patchwork)

db+df+dh



(nf<-ggplot(data=em3%>%
              filter(site=="Neck Point (2021)"))+
    geom_tile(aes(x=dom,y=tid,fill=col2))+
    scale_fill_identity()+
    scale_y_continuous(name="Hour",breaks=yalab$tid,labels=yalab$hr,limits=c(0,768),expand = c(0,0))+
    xlab("Date (March)")+
    ggtitle("Neck Point",subtitle="False Color (SNR, Cluster Count, Low Frequency Cover)"))

(nh<-ggplot(data=em3%>%
              filter(site=="Neck Point (2021)"))+
    geom_tile(aes(x=dom,y=tid,fill=as.factor(herring.hs)))+
    scale_y_continuous(name="Hour",breaks=yalab$tid,labels=yalab$hr,limits=c(0,768),expand = c(0,0))+
    xlab("Date (March)")+
    ggtitle("",subtitle="Herring score"))

(nb<-ggplot(data=em3%>%
              filter(site=="Neck Point (2021)"))+
    geom_tile(aes(x=dom,y=tid,fill=as.factor(boat)))+
    scale_y_continuous(name="Hour",breaks=yalab$tid,labels=yalab$hr,limits=c(0,768),expand = c(0,0))+
    xlab("Date (March)")+
    ggtitle("",subtitle="Herring score")+
    theme(legend.position = "left"))

nb+nf+nh


ggplot(data=em3,aes(x=herring.hs,y=boat,color=col2))+
  geom_jitter()+
  scale_color_identity()+
  facet_grid(~site)

# use 60 second data


# try making a tile figure out of multivariate
em4<-data.frame(datetime=d2$datetime,site=d2$site,
                herring=d2$herring.hs,
                boat=d2$boat,
                waves=d2$waves,
                samp.tot.sec=d2$samp.tot.sec,
                index=d2$index_type,
                score=d2$score)%>%
  #filter(samp.tot.sec!=60)%>%
  filter(index %in% c("Snr","ClusterCount","LowFreqCover"))%>%
  pivot_wider(names_from = index,values_from=score)%>%
  mutate(r2=scales::rescale(Snr,to=c(0,1)),
         g2=scales::rescale(ClusterCount,to=c(0,1)),
         b2=scales::rescale(LowFreqCover,to=c(0,1)),
         dom=day(datetime),
         hr=hour(datetime),
         hrm=hour(datetime)+(minute(datetime)/60),
         hrm2=hms(paste(hour(datetime),minute(datetime),"00")),
         #col=rgb(r, g, b, maxColorValue = 1),
         col2=rgb(r2,g2,b2,maxColorValue = 1))  


#ggplot(data=em4,aes(x=Snr,y=ClusterCount,color=LowFreqCover,shape=site))+
#ggplot(data=em3,aes(x=RDA1,y=RDA2,color=RDA3,shape=site))+
#ggplot(data=em3,aes(x=RDA2,y=RDA3,color=RDA1,shape=site))+
ggplot(data=em3,aes(x=RDA1,y=RDA3,color=RDA2,shape=site))+
  geom_jitter(alpha=.4)+
  #scale_color_identity()+
  scale_color_viridis_c(option="A",end=.8)+
  facet_grid(rows=vars(herring.hs),cols=vars(boat))


# one more way of vizualizing it
herring.col<-data.frame(herring.hs=unique(em3$herring.hs))%>%
  arrange(herring.hs)%>%
  mutate(herring.index=viridis::magma(4,end=.8))

boat.col<-data.frame(boat=unique(em3$boat))%>%
  arrange(boat)%>%
  mutate(boat.index=viridis::magma(3,end=.8))

waves.col<-data.frame(waves=unique(em3$waves))%>%
  arrange(waves)%>%
  mutate(waves.index=viridis::magma(4,end=.8))

em3b<-left_join(em3,herring.col)%>%
  left_join(boat.col)%>%
  left_join(waves.col)%>%
  rename(FalseColor.index=col2,FalseColor.rda=col)%>%
  pivot_longer(c(FalseColor.index,FalseColor.rda,herring.index,boat.index,waves.index),
               names_to="Index",
               values_to="colors")%>%
  select(site,datetime,Index,colors)%>%
  distinct()

em3b$Index<-factor(em3b$Index,levels=c("FalseColor.index","FalseColor.rda","herring.index",
                                       "boat.index","waves.index"),
                   labels = c("False Color\nIndex",
                              "False Color\nRDA","Herring Score",
                              "Boat Score","Waves Score"))

ggplot(em3b,aes(x=datetime,y=1,fill=colors))+
  geom_tile()+
  scale_fill_identity()+
  facet_grid(Index~site,scales="free")+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y=element_blank())
