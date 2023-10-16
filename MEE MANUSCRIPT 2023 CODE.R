#### CODE FOR ANALYSIS PRESENTED IN "INCLUDING THE INVISIBLE FRACTION IN WHOLE POPULATION STUDIES: A GUIDE TO THE GENETIC SAMPLE OF UNHATCHED BIRD EGGS" - FAY MORLAND, SELINA PATEL, ANNA SANTURE, PATRICIA BREKKE, NICOLA HEMMINGS
#### CONTACT FAY MORLAND FAY.MORLAND@OTAGO.AC.NZ WITH ANY QUERIES 
#### DATA FILES ASSOCIATED WITH THIS SCRIPT CAN BE FOUND AT https://github.com/fmorland/unhatchedeggs

## import data ####
library(readr)
microsat_data<- read_csv("early embryo microsat data.csv")
mircosat_fails_combo<-read.csv("microsat_fails_combined.csv")
### summary statistics #####

## get DNA quantity for every stage 

microsat_data$DNA.quantity<-as.numeric(microsat_data$DNA.quantity)



quantity_summary<-filter(microsat_data,!is.na(DNA.quantity)) %>%
  group_by(part,HH) %>%
  summarise(mean=mean(DNA.quantity), n=n(), se=sd(DNA.quantity)/sqrt(n()))


quantity_summary2<-filter(microsat_data,!is.na(DNA.quantity)) %>%
  summarise(mean=mean(DNA.quantity), min=min(DNA.quantity), max=max(DNA.quantity), n=n())

## DNA concentration for every stage

conc_summary<-filter(microsat_data,!is.na(DNA.conc)) %>%
  group_by(part,HH) %>%
  summarise(mean=mean(DNA.conc), n=n(), se=sd(DNA.conc)/sqrt(n()))

## mistying by stage

mistype_summary<-filter(microsat_data,!is.na(percent.mismatched)) %>%
  group_by(part,HH) %>%
  summarise(mean=mean(percent.mismatched), n=n(), se=sd(percent.mismatched)/sqrt(n()))    

mistype_summary2<-filter(microsat_data,!is.na(percent.mismatched)) %>%
  group_by(HH) %>%
  summarise(mean=mean(percent.mismatched), n=n(), se=sd(percent.mismatched)/sqrt(n()))    

mistype_summary


### AOV of very early embryo samples of sample type affect on DNA yield ####

## anova for part/sample type

early_microsat_data<-filter(microsat_data, HH == "1")
#remove values which have NA for part or quantity
early_microsat_data<-early_microsat_data %>% filter_at(vars(part,DNA.quantity),all_vars(!is.na(.)))

aov1<-aov(DNA.quantity ~ part, data=early_microsat_data)

summary(aov1)
summary.aov(aov1)

TukeyHSD(aov1)


### cor test of length of microsat and microsatellite fail rate ####

cor.test(microsat_fails_combo[-c(10,18,20),]$length, microsat_fails_combo[-c(10,18,20),]$percent)
cor.test(microsat_fails_combo$length, microsat_fails_combo$percent)


### test affect of processing latency and refrigeration vs freezing on quantity of DNA yield and microsat success rate ####


## processing latency vs DNA quantity and number amplified 

cor.test(microsat_data$processing_latency,microsat_data$DNA.quantity)

cor.test(microsat_data$processing_latency,microsat_data$number.amplified)

## AOV for fridge vs freezer on DNA quantity
# samples were refrigerated in 2019 and frozen in 2020

microsat_data$year<-as.factor(microsat_data$year)
aov3<-aov(DNA.quantity ~ year, data=microsat_data)

summary.aov(aov3)




