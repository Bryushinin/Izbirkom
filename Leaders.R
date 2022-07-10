library("dplyr")
library("tidyr")
library("ggplot2")
Party<-read.table(file="party_results.tsv",header=TRUE,sep='\t',colClasses = c("character",rep("numeric",32)))
Uiks<-read.table(file="uiks.tsv",header=TRUE,sep="\t")
Leaders<-subset(Party,select=c("uik_id","X10","er","kprf","ldpr"))

Leaders<-mutate(Leaders, er.per = er/X10 *100, kprf.per = kprf/X10 * 100, 
       ldpr.per = ldpr/X10 * 100, rest = 100 - er.per -kprf.per -ldpr.per)


Weird2<-subset(Leaders, rest>75)
Rest.Dom<-left_join(x=Weird2,y=Party,by=c("uik_id"="uik_id"))
Rest.Dom$X10.x
Rest.Dom<-filter(Rest.Dom, X10.x>1000)

party_name.v<-c("apple","er","gp","green","gs","kommumist","kprf","ldpr","pens","parnas","partiot","rodina","rost","sr")
Place<-left_join(x=Rest.Dom,y=Uiks,by=c("uik_id"="id"))  %>% select(apple:sr)

Place.paintable<- gather(Place, key="party_name",value="value")
ggplot(Place.paintable, aes(x="", y=value, fill=party_name)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) + theme_void() + ggtitle("УИК №8214 (г.Тирасполь)") + theme(plot.title = element_text(hjust = 0.5))


Voronezh<- left_join(x=(Uiks %>% filter(region_id=="voronezh")),y=Party,by=c("id"="uik_id")) %>% 
  select(c("X10",all_of(party_name.v))) %>%
  summarise(across(.fns=sum))
voronezh.byull<-Voronezh$X10[1]
Voronezh<- Voronezh %>% select(all_of(party_name.v)) %>% 
  gather(key="party_name",value="value") %>% mutate(value=value/voronezh.byull * 100)

ggplot(Voronezh, aes(x="", y=value, fill=party_name)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) + theme_void() + ggtitle("Воронежская область") + theme(plot.title = element_text(hjust = 0.5))
ggplot(Voronezh, aes(x=party_name,y=value,fill = party_name)) +
  geom_bar(stat="identity") + labs(title = "Результаты голосования в Воронежской области", x="Партии", y = "Процент голосов")


byull<-sum(Party$X10)
Russia<-Party %>% select(all_of(party_name.v)) %>% summarise(across(.fns=sum)) %>% 
  gather(key="party_name",value="value") %>% mutate(value=value/byull * 100)
ggplot(Russia, aes(x=party_name,y=value,fill = party_name)) +
  geom_bar(stat="identity") + labs(title = "Результаты голосования", x="Партии", y = "Процент голосов")+
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))

Compare.vor.Rus<-left_join(x=Russia,y=Voronezh,suffix=c(".Rus",".vor"),by=c("party_name"="party_name"))
ggplot(Compare.vor.Rus) + geom_bar(stat="identity",aes(x=party_name,y=value.vor,fill="Voronezh"),fill="grey",alpha=0.75) +
  geom_bar(stat = "identity", aes(x=party_name,y=value.Rus,fill=party_name),alpha=0.5) +
  labs(title = "Результаты голосования в РФ и Воронежской области", x="Партии", y="Процент голосов")+
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))

ggplot(data = Leaders) + geom_histogram(binwidth=1,color="white",fill="blue", aes(x=er.per),alpha=0.5) + 
  geom_histogram(binwidth=1,color="black",fill="red", aes(x=kprf.per),alpha=0.5) + 
  geom_histogram(binwidth=1,color="blue",fill="yellow", aes(x=ldpr.per),alpha=0.5)+ 
  labs(title = "Гистограмма процентов голосов за ЕР, КПРФ и ЛДПР", x="Процент голосов", y = "Число участков") +
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))

ggplot(data = Leaders) + geom_density(color="white",fill="blue", aes(x=er.per),alpha=0.75) + 
  geom_density(color="black",fill="red", aes(x=kprf.per),alpha=0.5) + 
  geom_density(color="blue",fill="yellow", aes(x=ldpr.per),alpha=0.5)+ 
  labs(title = "Плотности (ЕР, КПРФ и ЛДПР)", x="Процент голосов", y = "")+
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))

ggplot(data = Leaders) + geom_histogram(binwidth=0.5,color="blue",fill="black", aes(x=rest))+ 
  labs(title = "Гистограмма процентов голосов за остальные партии", x="Процент голосов", y = "Число участков")+
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))
rm(Party)
rm(Uiks)
rm(Leaders)
gc()