library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
data<-read_excel("/Users/smaguire/Desktop/Template switching/CE output/11-6-17/CE-TSO-2nd nucleotide.xlsx")


summarise(s.peak = sum(Area)) %>%
  spread(.,key=Peak,value = s.peak,fill=0, drop = T)

theme.sean<-function (base_size = 12, base_family = "sans") 
{
  (theme_foundation(base_size = base_size, base_family = base_family) + 
     theme(line = element_line(colour = "black"), rect = element_rect(fill = ggthemes_data$fivethirtyeight["ltgray"], 
                                                                      linetype = 0, colour = NA), text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]), 
           axis.text = element_text(), 
           axis.ticks = element_blank(), axis.line = element_blank(), 
           legend.background = element_rect(), legend.position = "right", 
           legend.direction = "vertical", legend.box = "horizontal", 
           panel.grid = element_line(colour = NULL), panel.grid.major = element_line(colour = ggthemes_data$fivethirtyeight["medgray"]), 
           panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0, 
                                                                         size = rel(1.5), face = "bold"), plot.margin = unit(c(1, 
                                                                                                                               1, 1, 1), "lines"), strip.background = element_rect()))
}
#combine small and main peaks
data %>%
  group_by(nucleotide,name,peak.type) %>%
  summarise(sum.area = sum(area)) %>%
  spread(.,key = peak.type, value = sum.area) %>%
  mutate(ts.eff = (tso/(prod+tso))*100) %>%
  group_by(nucleotide) %>%
  summarise(mean.eff = mean(ts.eff),sd.eff = sd(ts.eff)) %>%
  ggplot(aes(x=nucleotide, y= mean.eff))+geom_col()+
  geom_errorbar(aes(ymin = mean.eff-sd.eff,ymax = mean.eff+sd.eff))+
  ylab("Template Switching Efficiency (Percent)")+
  xlab("Second Nucleotide")+theme.sean()

scale_fill_sean<-function (...) 
{
  discrete_scale("fill", "economist", fivethirtyeight_pal(), 
                 ...)
}
data %>%
  filter(!(peak.type == "prod" & peak.class == "small")) %>%
  group_by(nucleotide,name,peak.type) %>%
  summarise(sum.area = sum(area)) %>%
  spread(.,key = peak.type, value = sum.area) %>%
  mutate(ts.eff = (tso/(prod+tso))*100) %>%
  group_by(nucleotide) %>%
  summarise(mean.eff = mean(ts.eff),sd.eff = sd(ts.eff)) %>%
  ggplot(aes(x=nucleotide, y= mean.eff))+geom_col(aes(fill=nucleotide))+
  geom_errorbar(aes(ymin = mean.eff-sd.eff,ymax = mean.eff+sd.eff))+
  ylab("Template Switching Efficiency (Percent)")+
  xlab("Second Nucleotide")+theme.sean()+scale_fill_manual(values = c("#008FD5","#FF2700","#77AB43","gray45"))


data %>%
  filter(peak.class == "main") %>%
  group_by(nucleotide,name,peak.type) %>%
  summarise(sum.area = sum(area)) %>%
  spread(.,key = peak.type, value = sum.area) %>%
  mutate(ts.eff = (tso/(prod+tso))*100) %>%
  group_by(nucleotide) %>%
  summarise(mean.eff = mean(ts.eff),sd.eff = sd(ts.eff)) %>%
  ggplot(aes(x=nucleotide, y= mean.eff))+geom_col()+
  geom_errorbar(aes(ymin = mean.eff-sd.eff,ymax = mean.eff+sd.eff))+
  ylab("Template Switching Efficiency (Percent)")+
  xlab("Second Nucleotide") + 
