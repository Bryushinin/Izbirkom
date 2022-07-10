library("dplyr")
library("ggplot2")
Odnom<-read.table(file="odnomandats.tsv",header=TRUE, sep = '\t', colClasses = c("character","character","character", "character", "character", "numeric", "numeric"))
Odnom.parties.nominated<-subset(Odnom,nominated==1,select=c("party_id","party_name","nominated"))
Odnom.parties.quantity<-Odnom.parties.nominated %>% group_by(party_id,party_name) %>% summarize(quantity=sum(nominated))

Odnom.parties.quantity<-Odnom.parties.quantity[order(Odnom.parties.quantity$quantity, decreasing = TRUE),]
Odnom.parties.quantity$party_id<-reorder(Odnom.parties.quantity$party_id,Odnom.parties.quantity$quantity)
ggplot(Odnom.parties.quantity, aes(x=party_id, y=quantity, fill = party_name))+ 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle =  45, hjust = 1),legend.position = "bottom") + 
  guides(fill=guide_legend(ncol=1,byrow = FALSE)) + theme(legend.box = "horizontal") +
  labs(title="Выдвинутые на голосование по одномандатным округам кандидаты",x="",y="Количество кандидатов")+
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))
