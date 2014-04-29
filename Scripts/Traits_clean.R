#file to standardise trait values from TRY
#to produce mean vales where more than one value is available

#first SLA
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/TRY stuff/Data")

SLA<-read.csv("SLA.csv")
str(SLA)
SLA_m<-data.frame(tapply(SLA$StdValue,SLA$AccSpeciesName,mean))
SLA_m$Sp<-rownames(SLA_m)
rownames(SLA_m)<-NULL
colnames(SLA_m)<-c("SLA","Sp")

library(ggplot2)

a<-ggplot(SLA_m,aes(y=SLA,x=Sp))+geom_point()+theme(
           axis.text.x  = element_text(angle=90))
a+geom_point(data=SLA,aes(y=StdValue,x=AccSpeciesName),colour="red",alpha=0.5)
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Data")

AGC<-read.csv("AGC.csv")
merge(AGC,SLA_m,by.x="Dominant.native",by.y="Sp")

#next height
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/TRY stuff/Data")
Height<-read.csv("Height.csv")
str(SLA)
Height_m<-data.frame(tapply(Height$StdValue,Height$AccSpeciesName,mean))
Height_m$Sp<-rownames(Height_m)
rownames(Height_m)<-NULL
colnames(Height_m)<-c("H","Sp")

#next root depth
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/TRY stuff/Data")
Root<-read.csv("Root.csv")
str(Root)
Root_m<-data.frame(tapply(Root$StdValue,Root$AccSpeciesName,mean))
Root_m$Sp<-rownames(Root_m)
rownames(Root_m)<-NULL
colnames(Root_m)<-c("H","Sp")

Height_m2<-Height_m
colnames(Height_m2)<-c("H_Nat","Sp")
AGC2<-merge(AGC,Height_m,by.x="Invasive",by.y="Sp",all.x=T)
AGC3<-merge(AGC2,Height_m2,by.x="Dominant.native",by.y="Sp",all.x=T)
AGC3$H_Diff<-log(AGC3$H)-log(AGC3$H_Nat.y)
AGC3$M_Diff<-log(AGC3$M_I)-log(AGC3$M_UI)
AGC4<-subset(AGC3,!is.na(H_Diff))
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Data")
write.csv(AGC4,"AGC_height.csv",row.names=F)

AGC5<-subset(AGC3,H<10)

plot(AGC3$H,exp(AGC3$M_Diff)-1,xlim=c(0,3),ylim=c(-1,5))



library(metafor)

ROM_AGC<-escalc("ROM",m1i=M_I,m2i=M_UI,sd1i=SE_I*sqrt(SS_I),sd2i=SE_UI*sqrt(SS_UI),n1i=SS_I,n2i=SS_UI,append=T,data=AGC5)
ROM_AGC$Rich_Diff<-log(ROM_AGC$Invaded.richness)-log(ROM_AGC$Uninvaded.richness)

#test the predictive ability of both traits
Null_mod<-rma(yi,vi,data=ROM_AGC)
M_I<-rma(yi,vi,mods=yi~H,data=ROM_AGC)

plot(M_I)
M_Diff<-rma(yi,vi,mods=yi~H_Diff,data=ROM_AGC)

plot(M_I)

str(ROM_AGC)

?rma

summary(M_Diff)


Null_mod$fit[5,1]
M_I$fit[5,1]
M_Diff$fit[5,1]

summary(ROM_AGC$H_Diff)



Pred_C<-data.frame(H=seq(-.4784,1.567,length.out=100))
Pred_C$pred<-predict(M_Diff,newmods=Pred_C$H)$pred
Pred_C$cilb<-predict(M_Diff,newmods=Pred_C$H)$ci.lb
Pred_C$ciub<-predict(M_Diff,newmods=Pred_C$H)$ci.ub

theme_set(theme_bw(base_size=10))
a<-ggplot(data=ROM_AGC,aes(x=exp(H_Diff)-1,y=exp(yi)-1))+geom_jitter(shape=1,size=3)
b<-a+geom_line(data=Pred_C,aes(x=exp(H)-1,y=exp(pred)-1),size=1)+geom_line(data=Pred_C,aes(x=exp(H)-1,y=exp(cilb)-1),size=0.5,lty=2)+geom_line(data=Pred_C,aes(x=exp(H)-1,y=exp(ciub)-1),size=0.5,lty=2)
c<-b+ylab("Proportional change in aboveground\ncarbon storage")
d<-c+xlab("Proportional difference between\ninvasive and native plant height")
d+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Figures")
ggsave("AGC_Height.pdf",height=4,width=6,units="in",dpi=400)



#now for water quality
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Data")
#import WQ data
WQ<-read.csv("WQ.csv")
WQ2<-merge(WQ,Height_m,by.x="Inv",by.y="Sp")
WQ3<-merge(WQ2,Height_m,by.x="Nat",by.y="Sp")
WQ3$H_Diff<-log(WQ3$H.x)-log(WQ3$H.y)
WQ3$Diff<-log(WQ3$M_I)-log(WQ3$M_UI)
WQ4<-subset(WQ3,!is.na(Diff))
write.csv(WQ4,"WQ_height.csv",row.names=F)


plot(WQ3$H_Diff,WQ3$Diff)


plot(WQ4$H_Diff,-WQ4$Diff)

ROM_WQ<-escalc("ROM",m1i=M_I,m2i=M_UI,sd1i=SE_I*sqrt(SS_I),sd2i=SE_UI*sqrt(SS_UI),n1i=SS_I,n2i=SS_UI,append=T,data=WQ4)
ROM_WQ<-subset(ROM_WQ,!is.na(yi)&!is.na(H_Diff))
#test the predictive ability of both traits
Null_WQ<-rma(-yi,vi,data=ROM_WQ)
M_I_WQ<-rma(-yi,vi,mods=yi~H.x,data=ROM_WQ)
M_Diff_WQ<-rma(-yi,vi,mods=yi~H_Diff,data=ROM_WQ)

AICc(Null_WQ)
AICc(M_I_WQ)
AICc(M_Diff_WQ)

summary(ROM_WQ$H_Diff)

Pred_WQ<-data.frame(H=seq(-2,2.3,length.out=100))
Pred_WQ$pred<-predict(M_Diff_WQ,newmods=Pred_WQ$H)$pred
Pred_WQ$cilb<-predict(M_Diff_WQ,newmods=Pred_WQ$H)$ci.lb
Pred_WQ$ciub<-predict(M_Diff_WQ,newmods=Pred_WQ$H)$ci.ub

theme_set(theme_bw(base_size=10))
a<-ggplot(data=ROM_WQ,aes(x=H_Diff,y=exp(yi)-1))+geom_jitter(shape=1,size=3)
b<-a+geom_line(data=Pred_WQ,aes(x=H,y=exp(pred)-1),size=1)+geom_line(data=Pred_WQ,aes(x=H,y=exp(cilb)-1),size=0.5,lty=2)+geom_line(data=Pred_WQ,aes(x=H,y=exp(ciub)-1),size=0.5,lty=2)
b
c<-b+ylab("Proportional change in aboveground\ncarbon storage")
d<-c+xlab("Proportional difference between\ninvasive and native plant height")
d+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))

#do the same for belowground carbon
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Data")
#import BGC data
BGC<-read.csv("BGC.csv")
BGC2<-merge(BGC,Height_m,by.x="Inv",by.y="Sp",all.x=T)
BGC3<-merge(BGC2,Height_m,by.x="Nat",by.y="Sp",all.x=T)
BGC3$H_Diff<-log(BGC3$H.x)-log(BGC3$H.y)
BGC3$Diff<-log(BGC3$M_I)-log(BGC3$M_UI)
BGC4<-subset(BGC3,!is.na(H_Diff))
write.csv(BGC4,"BGC_height.csv",row.names=F)

plot(BGC3$H_Diff,BGC3$Diff)


ROM_BGC<-escalc("ROM",m1i=M_I,m2i=M_UI,sd1i=SE_I*sqrt(SS_I),sd2i=SE_UI*sqrt(SS_UI),n1i=SS_I,n2i=SS_UI,append=T,data=BGC4)
ROM_BGC<-subset(ROM_BGC,!is.na(yi)&!is.na(H_Diff))
#test the predictive ability of both traits
Null_BGC<-rma(yi,vi,data=ROM_BGC)
M_I_BGC<-rma(yi,vi,mods=yi~H.x,data=ROM_BGC)
M_Diff_BGC<-rma(yi,vi,mods=yi~H_Diff,data=ROM_BGC)

plot(M_Diff_BGC)

AICc(Null_BGC)
AICc(M_I_BGC)
AICc(M_Diff_BGC)

summary(ROM_BGC$H_Diff)

plot(ROM_BGC$H_Diff,ROM_BGC$Diff)

Pred_BGC<-data.frame(H=seq(-.8,2.3,length.out=100))
Pred_BGC$pred<-predict(M_Diff_BGC,newmods=Pred_BGC$H)$pred
Pred_BGC$cilb<-predict(M_Diff_BGC,newmods=Pred_BGC$H)$ci.lb
Pred_BGC$ciub<-predict(M_Diff_BGC,newmods=Pred_BGC$H)$ci.ub

theme_set(theme_bw(base_size=10))
a<-ggplot(data=ROM_BGC,aes(x=exp(H_Diff)-1,y=exp(yi)-1))+geom_jitter(shape=1,size=3)
b<-a+geom_line(data=Pred_BGC,aes(x=exp(H)-1,y=exp(pred)-1),size=1)+geom_line(data=Pred_BGC,aes(x=exp(H)-1,y=exp(cilb)-1),size=0.5,lty=2)+geom_line(data=Pred_BGC,aes(x=exp(H)-1,y=exp(ciub)-1),size=0.5,lty=2)
b
c<-b+ylab("Proportional change in belowground\ncarbon storage")
d<-c+xlab("Proportional difference between\ninvasive and native plant height")
d+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Figures")
ggsave("BGC_Height.pdf",height=4,width=6,units="in",dpi=400)

#do the same for water provision
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Data")
#import WP data
WP<-read.csv("WP.csv")
WP2<-merge(WP,Height_m,by.x="Invasive",by.y="Sp")
WP3<-merge(WP2,Height_m,by.x="Dominant.native",by.y="Sp")
WP3$H_Diff<-log(WP3$H.x)-log(WP3$H.y)
WP3$Diff<-log(WP3$M_I)-log(WP3$M_UI)
plot(WP3$H.x,WP3$Diff)
WP4<-subset(WP3,!is.na(Diff))
write.csv(WP3,"WP_height.csv",row.names=F)
plot(WP4$H_Diff,WP4$Diff)

#merge with root

WP5<-merge(WP3,Root_m,by.x="Invasive",by.y="Sp",all=T)

str(WP5)

plot(WP5$I_Root,exp(WP5$Diff)-1)




