# script to look at performance of ACI ratio

# load packages and data
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("patchwork")
theme_set(theme_bw()+theme(panel.grid = element_blank()))
rat.dat<-read_rds("wdata/ratio-df-ACI-all.rds")%>%
  group_by(site)


#write function

prop.files<-function(data,ff){
  t1<-sample_frac(data,ff,replace=FALSE)
  t1<-t1%>%
    mutate(herring.present = ifelse(herring.hs==0,0,1),
           herring.abundant = ifelse(herring.hs >=2,1,0),
           kept.with.ratio=ifelse(hb_ratio>0,1,0),
           herring.present.ratio=ifelse(herring.present==1 & kept.with.ratio==1,1,0),
           herring.abundant.ratio=ifelse(herring.abundant==1 & kept.with.ratio==1,1,0))%>%
    group_by(site)%>%
    summarize(total.files=n(),
              total.files.ratio=sum(kept.with.ratio),
              herring.present=sum(herring.present),
              herring.abundant=sum(herring.abundant),
              herring.present.ratio=sum(herring.present.ratio),
              herring.abundant.ratio=sum(herring.abundant.ratio),
              present.files=herring.present/total.files,
              present.ratio.files=herring.present.ratio/total.files.ratio,
              present.ratio.files=ifelse(!is.na(present.ratio.files),present.ratio.files,0),
              abundant.files=herring.abundant/total.files,
              abundant.ratio.files=herring.abundant.ratio/total.files.ratio,
              abundant.ratio.files=ifelse(!is.na(abundant.ratio.files),abundant.ratio.files,0),
              ratio.missed.presence=ifelse(herring.present>0 & herring.present.ratio==0,1,0),
              ratio.missed.abundant=ifelse(herring.abundant>0 & herring.abundant.ratio==0,1,0))
  return(t1)  
}

rat.eval<-prop.files(data=rat.dat,ff=.1)

for(i in 1:999){
  rat.eval<-bind_rows(rat.eval,
                      prop.files(data=rat.dat,ff=.1))
}

# when using the ratio would cause you to miss herring
rat.eval2<-rat.eval%>%
  group_by(site)%>%
  summarize(times.presence.missed=sum(ratio.missed.presence),
            times.abundantherring.missed=sum(ratio.missed.abundant))

(miss.presence<-ggplot(data=rat.eval2)+
  geom_bar(aes(x=site,y=times.presence.missed/1000),stat="identity")+
  ylab("Proportion of iterations where applying the \nratio led to missing herring presence")+
  ylim(c(0,1)))

(miss.abundant<-ggplot(data=rat.eval2)+
  geom_bar(aes(x=site,y=times.abundantherring.missed/1000),stat="identity")+
  ylab("Proportion of iterations where applying the \nratio led to missing abundant herring")+
    ylim(c(0,1)))

# how does using ration reduce effort?
rat.eval<-rat.eval%>%
  mutate(diff.presence=present.ratio.files-present.files,
         diff.abundant=abundant.ratio.files-abundant.files,
         diff.files=total.files.ratio-total.files)

(file.diff<-ggplot(rat.eval%>%filter(total.files.ratio!=0))+
    geom_boxplot(aes(x=site,y=diff.files))+
    ylab("Difference in number of files to review \n(Ratio Applied - Without Ratio)"))

(ppres.diff<-ggplot(rat.eval)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=.4)+
    geom_boxplot(aes(x=site,y=diff.presence))+
    ylab("Difference in proportion of files with herring present \n(Ratio Applied - Without Ratio)"))

(pabund.diff<-ggplot(rat.eval)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=.4)+
    geom_boxplot(aes(x=site,y=diff.abundant))+
    ylab("Difference in proportion of files with abundant herring \n(Ratio Applied - Without Ratio)"))

no.ratio.files<-rat.eval%>%
  filter(total.files.ratio==0)%>%
  group_by(site)%>%
  summarise(n.files=n()/1000)

no.ratio.files2=data.frame(site=unique(rat.eval$site))%>%
  left_join(no.ratio.files)%>%
  mutate(n.files=ifelse(is.na(n.files),0,n.files))

(nofiles<-ggplot(data=no.ratio.files2)+
    geom_bar(aes(x=site,y=n.files),stat="identity")+
    ylab("Proportion of iterations where no files \nwere left after applying the ratio")+
    ylim(c(0,1)))


(nofiles + miss.presence + miss.abundant)/(file.diff+ppres.diff+pabund.diff)

ggsave("figs/evaluating_ratio.png",height = 10,width=15)



prop2.files<-function(data,ff){
  t1<-sample_frac(data,ff,replace=FALSE)
  t1<-t1%>%
    mutate(rustle.present = ifelse(rustle==0,0,1),
           rustle.abundant = ifelse(rustle >=2,1,0),
           kept.with.ratio=ifelse(hb_ratio>0,1,0),
           rustle.present.ratio=ifelse(rustle.present==1 & kept.with.ratio==1,1,0),
           rustle.abundant.ratio=ifelse(rustle.abundant==1 & kept.with.ratio==1,1,0))%>%
    group_by(site)%>%
    summarize(total.files=n(),
              total.files.ratio=sum(kept.with.ratio),
              rustle.present=sum(rustle.present),
              rustle.abundant=sum(rustle.abundant),
              rustle.present.ratio=sum(rustle.present.ratio),
              rustle.abundant.ratio=sum(rustle.abundant.ratio),
              present.files=rustle.present/total.files,
              present.ratio.files=rustle.present.ratio/total.files.ratio,
              present.ratio.files=ifelse(!is.na(present.ratio.files),present.ratio.files,0),
              abundant.files=rustle.abundant/total.files,
              abundant.ratio.files=rustle.abundant.ratio/total.files.ratio,
              abundant.ratio.files=ifelse(!is.na(abundant.ratio.files),abundant.ratio.files,0),
              ratio.missed.presence=ifelse(rustle.present>0 & rustle.present.ratio==0,1,0),
              ratio.missed.abundant=ifelse(rustle.abundant>0 & rustle.abundant.ratio==0,1,0))
  return(t1)  
}

rat.eval.r<-prop2.files(data=rat.dat,ff=.1)

for(i in 1:999){
  rat.eval.r<-bind_rows(rat.eval.r,
                      prop2.files(data=rat.dat,ff=.1))
}



# when using the ratio would cause you to miss rustle
rat.eval.r2<-rat.eval.r%>%
  group_by(site)%>%
  summarize(times.presence.missed=sum(ratio.missed.presence),
            times.abundantrustle.missed=sum(ratio.missed.abundant))

(miss.presence<-ggplot(data=rat.eval.r2)+
    geom_bar(aes(x=site,y=times.presence.missed/1000),stat="identity")+
    ylab("Proportion of iterations where applying the \nratio led to missing rustle presence")+
    ylim(c(0,1)))

(miss.abundant<-ggplot(data=rat.eval.r2)+
    geom_bar(aes(x=site,y=times.abundantrustle.missed/1000),stat="identity")+
    ylab("Proportion of iterations where applying the \nratio led to missing abundant rustle")+
    ylim(c(0,1)))

# how does using ration reduce effort?
rat.eval.r<-rat.eval.r%>%
  mutate(diff.presence=present.ratio.files-present.files,
         diff.abundant=abundant.ratio.files-abundant.files,
         diff.files=total.files.ratio-total.files)

(file.diff<-ggplot(rat.eval.r%>%filter(total.files.ratio!=0))+
    geom_boxplot(aes(x=site,y=diff.files))+
    ylab("Difference in number of files to review \n(Ratio Applied - Without Ratio)"))

(ppres.diff<-ggplot(rat.eval.r)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=.4)+
    geom_boxplot(aes(x=site,y=diff.presence))+
    ylab("Difference in proportion of files with rustle present \n(Ratio Applied - Without Ratio)"))

(pabund.diff<-ggplot(rat.eval.r)+
    geom_hline(aes(yintercept=0),linetype="dashed",alpha=.4)+
    geom_boxplot(aes(x=site,y=diff.abundant))+
    ylab("Difference in proportion of files with abundant rustle \n(Ratio Applied - Without Ratio)"))

no.ratio.files<-rat.eval.r%>%
  filter(total.files.ratio==0)%>%
  group_by(site)%>%
  summarise(n.files=n()/1000)

no.ratio.files2=data.frame(site=unique(rat.eval.r$site))%>%
  left_join(no.ratio.files)%>%
  mutate(n.files=ifelse(is.na(n.files),0,n.files))

(nofiles<-ggplot(data=no.ratio.files2)+
    geom_bar(aes(x=site,y=n.files),stat="identity")+
    ylab("Proportion of iterations where no files \nwere left after applying the ratio")+
    ylim(c(0,1)))


(nofiles + miss.presence + miss.abundant)/(file.diff+ppres.diff+pabund.diff)

ggsave("figs/evaluating_ratio_rustle.png",height = 10,width=15)
