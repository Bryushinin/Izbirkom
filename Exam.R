library("dplyr")
library("tidyr")
library("ggplot2")
library("EnvStats")
Party<-read.table(file="party_results.tsv",header=TRUE,sep='\t',colClasses = c("character",rep("numeric",32)))
Uiks<-read.table(file="uiks.tsv",header=TRUE,sep="\t")
Leaders<-subset(Party,select=c("uik_id","X10","er","kprf","ldpr"))

byull<-sum(Leaders$X10)
byull.er<-sum(Leaders$er)

Leaders<-mutate(Leaders, er.per = er/X10 *100, kprf.per = kprf/X10 * 100, 
                ldpr.per = ldpr/X10 * 100, rest = 100 - er.per -kprf.per -ldpr.per)
Leaders<-mutate(Leaders, ldpr.per.2=ldpr.per+23)
Leaders<-mutate(Leaders, ldpr.per.3=ldpr.per-2)
Leaders<-mutate(Leaders, ldpr.per.4=ldpr.per-4)
dev.new()
qqPlot(x=Leaders$kprf.per,y=Leaders$ldpr.per,add.line = TRUE)


ggplot(data = Leaders)+ 
  geom_histogram(binwidth=1,color="black",fill="red", aes(x=kprf.per),alpha=0.5) + 
  geom_histogram(binwidth=1.1,color="blue",fill="yellow", aes(x=ldpr.per.3),alpha=0.5)+ 
  labs(x="Процент голосов", y = "Число участков") +
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))


Leaders<-mutate(Leaders, rest = 100 - er.per -kprf.per -ldpr.per)

ggplot(data = Leaders)+ 
  geom_histogram(binwidth=1,color="blue",fill="black", aes(x=rest),alpha=0.5) + 
  geom_histogram(binwidth=0.76,color="blue",fill="yellow", aes(x=ldpr.per.4),alpha=0.5)+ 
  labs(x="Процент голосов", y = "Число участков") +
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))


Leaders<- Leaders %>% filter(er.per>=23)

ggplot(data = Leaders) + geom_histogram(binwidth=1,color="white",fill="blue", aes(x=er.per),alpha=0.5) + 
  geom_histogram(binwidth=1,color="blue",fill="yellow", aes(x=ldpr.per.2),alpha=0.5)+ 
  labs( x="Процент голосов", y = "Число участков") +
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))
Leaders<- Leaders %>% group_by(er.per,ldpr.per.2) %>% summarise(across(.cols=er,.fns=mean)) 

Leaders<-mutate(Leaders, difference=(er.per-ldpr.per.2)/100)

Leaders<-mutate(Leaders, people.d=difference*er)
#"Лишние" голоса
(fake<-sum(Leaders$people.d))
#Процент проголосовавших за ЕР
(byull.er/byull*100)
#"Чистый" процент проголосовавших за ЕР
((byull.er-fake)/byull * 100)


rm(Party)
rm(Uiks)
rm(Leaders)
gc()