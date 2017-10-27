library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
data<-read_excel("/Users/smaguire/Desktop/Template switching/CE output/10-27-17/10-27-17-CE.xlsx")
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

data %>%
  mutate(nucleotide = unlist(strsplit(data$sample.name,"-"))[seq(from=2,to=2*nrow(data),by=2)]) %>%
  select(sample.id,area,peak_id,nucleotide)%>%
  spread(peak_id,area) %>%
  mutate(percent.nta = (nta/(main+nta))*100) %>%
  group_by(nucleotide) %>%
  summarise(mean.per = mean(percent.nta), sd = sd(percent.nta)) %>%
  ggplot(aes(x = nucleotide,y=mean.per))+geom_col(fill="gray60") +
  geom_errorbar(aes(ymin = mean.per-sd, ymax = mean.per+sd),width = .5) +
  ylab("Percentage of Transcripts\nWith Non-Template Addition") + xlab(NULL) +
  theme.sean()
  
data %>%
  mutate(nucleotide = unlist(strsplit(data$sample.name,"-"))[seq(from=2,to=2*nrow(data),by=2)]) %>%
  select(sample.id,size,peak_id,nucleotide)%>%
  spread(peak_id,size) %>%
  mutate(size.diff = nta - main) %>%
  group_by(nucleotide) %>%
  summarise(mean.size.diff = mean(size.diff), sd = sd(size.diff)) %>%
  ggplot(aes(x = nucleotide,y=mean.size.diff))+geom_col(fill="gray60") +
  geom_errorbar(aes(ymin = mean.size.diff-sd, ymax = mean.size.diff+sd),width = .5) +
  ylab("Estimated Average Length of NTA") + xlab(NULL) +
  theme.sean()

ggsave("/Users/smaguire/Desktop/Template switching/CE output/10-27-17/per_nta.jpeg")

ggsave("/Users/smaguire/Desktop/Template switching/CE output/10-27-17/length_nta.jpeg")
