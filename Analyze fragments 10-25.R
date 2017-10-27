library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

data<-read_excel("results_summary.xlsx")

p.data<-
data %>% 
  group_by(Sample,Code, Peak) %>%
  summarise(s.peak = sum(Area)) %>%
  spread(.,key=Peak,value = s.peak,fill=0, drop = T)

write.csv(p.data,"data_summary_processed.csv")

data.sum<-read_excel("results_summary2.xlsx")

summary <-
data.sum %>%
  group_by(Nucleotide,Mix,Template) %>%
  summarise(mean.total = mean(Total), mean.per = mean(Percentage))

library(ggthemes)

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

ggplot(summary,aes(x=Nucleotide,y=mean.per,fill=Template))+
  geom_bar(stat = 'identity',position = "dodge")+facet_wrap(~Mix)+
  theme.sean()+scale_fill_fivethirtyeight()+ylab("Percentage Transcript\nTagged With TSO")+
  xlab("Transcript Starting Nucleotide")
  

ggplot(summary,aes(x=Nucleotide,y=mean.total,fill=Template))+
  geom_bar(stat = 'identity',position = "dodge")+facet_wrap(~Mix)+
  theme.sean()+scale_fill_fivethirtyeight()+ylab("Total Amount of Transcript (RFU)")+
  xlab("Transcript Starting Nucleotide")
