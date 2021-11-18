#call characterizations

library(tidyverse)
library(Rraven)
library(patchwork)

calls<-imp_raven(path="Call characterization",
                 all.data = T)


hs<-filter(calls,`Sound Type`=="hs")


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
