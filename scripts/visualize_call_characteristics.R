#call characterizations

library(tidyverse)
library(Rraven)
library(patchwork)

calls<-imp_raven(path="Call characterization",
                 all.data = T)


hs<-filter(calls,`Sound Type`=="hs")

# get mean and confidence intervals
sum.hs<-hs%>%
  summarize(m.peak=mean(`Peak Freq (Hz)`),
            ci.peak.90=qnorm(0.95)*(m.peak/sqrt(n())),
            low.peak.90=m.peak-ci.peak.90,
            high.peak.90=m.peak+ci.peak.90,
            ci.peak.95=qnorm(0.975)*(m.peak/sqrt(n())),
            low.peak.95=m.peak-ci.peak.95,
            high.peak.95=m.peak+ci.peak.95,
            ci.peak.99=qnorm(0.995)*(m.peak/sqrt(n())),
            low.peak.99=m.peak-ci.peak.99,
            high.peak.99=m.peak+ci.peak.99,
            max.freq=max(`High Freq (Hz)`),
            max.peak=max(`Peak Freq (Hz)`))

table(hs$site,hs$spl_class)

bp<-ggplot(data=hs)+
  theme_bw()+
  theme(panel.grid = element_blank())

(duration<-bp+
  geom_density(aes(`Delta Time (s)`,fill=site),alpha=.4)+
    facet_wrap(~spl_class))

(peakhz<-bp+
    geom_density(aes(`Peak Freq (Hz)`,fill=site),alpha=.4)+
    facet_wrap(~spl_class))

(lowhz<-bp+
    geom_density(aes(`Low Freq (Hz)`,fill=site),alpha=.4)+
    facet_wrap(~spl_class))

(highhz<-bp+
    geom_density(aes(`High Freq (Hz)`,fill=site),alpha=.4)+
    facet_wrap(~spl_class))

(inbandpower<-bp+
    geom_density(aes(`Inband Power (dB FS)`,fill=site),alpha=.4)+
    facet_wrap(~spl_class))

(peakp<-bp+
    geom_density(aes(`Peak Power Density (dB FS)`,fill=site),alpha=.4)+
    facet_wrap(~spl_class))

lowhz/highhz/peakhz/duration/inbandpower/peakp + plot_layout(guides = "collect")

ggsave("figures/callcharacteristics.jpg")

(sum.hs <- hs %>% #group_by(site, spl_class) %>% 
  summarise(mean_peak = mean(`Peak Freq (Hz)`),
            med_peak = median(`Peak Freq (Hz)`),
            mean_low = mean(`Low Freq (Hz)`),
            med_low = median(`Low Freq (Hz)`),
            mean_high = mean(`High Freq (Hz)`),
            med_high = median(`High Freq (Hz)`)))

