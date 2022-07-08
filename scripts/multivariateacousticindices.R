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

# library(plotly)
# 
# fig <- plot_ly(dm, x = ~Snr, y = ~LowFreqCover, z = ~ClusterCount, color = ~as.factor(herring.hs),alpha=.5,colors=c("#0D0887FF", "#B12A90FF" ,"#FCA636FF"))
# fig <- fig %>% add_markers()
# fig
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
