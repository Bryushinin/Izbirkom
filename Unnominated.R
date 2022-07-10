library("dplyr")
library("ggplot2")
Odnom<-read.table(file="odnomandats.tsv",header=TRUE, sep = '\t', colClasses = c("character","character","character", "character", "character", "numeric", "numeric"))
Odnom.parties.not.nominated<-subset(Odnom,nominated==0,select=c("party_id","party_name","nominated"))
Odnom.parties.not.nominated$nominated<-1
Odnom.parties.quantity.n<-Odnom.parties.not.nominated %>% group_by(party_id,party_name) %>% summarize(quantity=sum(nominated))
Odnom.parties.quantity.n<-Odnom.parties.quantity.n[order(Odnom.parties.quantity.n$quantity, decreasing = TRUE),]

Odnom.parties.quantity.n$party_id<-reorder(Odnom.parties.quantity.n$party_id,Odnom.parties.quantity.n$quantity)#Odnom.parties.quantity.n$party_name<-reorder(Odnom.parties.quantity.n$party_name,rev(Odnom.parties.quantity.n$quantity))
ggplot(Odnom.parties.quantity.n, aes(x=party_id, y=quantity, fill = party_name, palette="Set1"))+ 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle =  45, hjust = 1),legend.position = "bottom") + 
  guides(fill=guide_legend(ncol=1,byrow = FALSE)) + theme(legend.box = "horizontal") + scale_fill_brewer(palette="Spectral") +
  labs(x="",y="Количество кандидатов")+
  theme(axis.title = element_text(size=14),legend.text = element_text(size=14))