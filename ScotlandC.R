library(reprex)
library(tidyverse)
library(xlsx)

fancy <- theme(legend.justification = c("right", "centre"),
               legend.key = element_rect(fill="white", size = 3),
               legend.key.size = unit(1,"cm"),
               legend.text = element_text(size=30),
               text = element_text(size=20))+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(colour = "grey", size = 10))+
  theme(axis.text.y = element_text(colour = "grey", size = 10))

mydata <- read.xlsx("C:/Users/samue/Downloads/R/ScotlandC.xlsx", 1)

sSize <- 0.35
kn <- 1000
S <- 1
S2 <- 1.5

base <- ggplot(mydata) +
  geom_abline(color="grey50") +
  scale_y_continuous(minor_breaks = seq(0,100,1),breaks = seq(0, 100, 5), expand = c(0,0), limits = c(0.1,53)) +
  xlim(as.Date("2021-5-6"),as.Date("2026-5-7"))+
  geom_vline(xintercept=as.Date("2021-5-6"), linetype="solid", alpha=0.5)+
  geom_vline(xintercept=as.Date("2026-5-7"), linetype="solid", alpha=0.5)+
  #scatter
  geom_point(aes(Date, SNP, color="SNP"), alpha=0.6, size=S2)+
  geom_point(aes(Date, CON, color="Conservative"), alpha=0.6, size=S2)+
  geom_point(aes(Date, LAB, color="Labour"), alpha=0.6, size=S2)+
  geom_point(aes(Date, LDM, color="Liberal Democrats"), alpha=0.6, size=S2)+
  geom_point(aes(Date, GRN, color="Green"), alpha=0.6, size=S2)+
  scale_colour_manual(name="Parties", values=c("#0087dc","#00b140","#e4003b","#faa61a","#f2d70c"))+
  geom_smooth(aes(Date, SNP),color="#f2d70c", fill = "lightblue3",method="loess", size=S, se=FALSE,n=kn, span = sSize)+
  geom_smooth(aes(Date, CON), color="#0087dc",method="loess", size=S, se=FALSE,n=kn, span = sSize)+
  geom_smooth(aes(Date, LAB),color="#e4003b", fill = "lightblue3",method="loess", size=S, se=FALSE,n=kn, span = sSize)+
  geom_smooth(aes(Date, LDM), color="#faa61a",method="loess", size=S, se=FALSE,n=kn, span = sSize)+
  geom_smooth(aes(Date, GRN),color="#00b140", fill = "lightblue3",method="loess", size=S, se=FALSE,n=kn, span = 1)+
  
  guides(color = guide_legend(override.aes = list(size=10, stroke=NA, alpha=1)))+
  ylab("Voteshare (%)")+
  xlab("Date")

plot(base+fancy)

ggsave(file="polls.svg", plot=graph, width=18, height=8)