#remove everything
rm(list=ls())

#script to analyse the links between aboveground 
#carbon changes and functional traits
library(metafor)
library(MuMIn)
library(gridExtra)

#import data
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Data")
AGC<-read.csv("AGC_Height.csv")

#calculate effect sizes for changes in
#aboveground carbon stocks
ROM<-escalc("ROM",m1i=M_I,m2i=M_UI,sd1i=SE_I*sqrt(SS_I),sd2i=SE_UI*sqrt(SS_UI),n1i=SS_I,n2i=SS_UI,append=T,data=AGC)

head(ROM)

plot(ROM$H_inv,ROM$yi)


#Test predictive ability of model using invasive trait
#and model using both invasive and native traits
Null_mod<-rma(yi,vi,data=ROM)
M_Diff_H<-rma(yi,vi,mods=yi~H_inv,data=ROM)
M_Diff<-rma(yi,vi,mods=yi~H_Diff,data=ROM)

summary(Null_mod)

#make model selection table
AGC_sel<-data.frame(Model=c("Null model","Invasive trait","Invasive and native traits"))
AGC_sel$AICc<-c(AICc(Null_mod),AICc(M_Diff),AICc(M_Diff_H))
AGC_sel$Delta<-AGC_sel$AICc-min(AGC_sel$AICc)
AGC_sel$R2<-c(1-(logLik(Null_mod)[1]/logLik(Null_mod)[1]),
            1-(logLik(M_Diff)[1]/logLik(Null_mod)[1]),
            1-(logLik(M_Diff_H)[1]/logLik(Null_mod)[1]))
AGC_sel$rel_lik<-exp((AGC_sel$AICc[1]-AGC_sel$AICc)/2)
AGC_sel$weight<-AGC_sel$rel_lik/(sum(AGC_sel$rel_lik))
#sort so that best model is top
AGC_sel<-AGC_sel[with(AGC_sel, order(Delta)), ]
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Tables")
write.csv("AGC_AIC.csv",x=AGC_sel,row.names=F)

#do plots of these relationships
library(ggplot2)
Pred_C<-data.frame(H=seq(-.4784,1.567,length.out=100))
Pred_C$pred<-predict(M_Diff,newmods=Pred_C$H)$pred
Pred_C$cilb<-predict(M_Diff,newmods=Pred_C$H)$ci.lb
Pred_C$ciub<-predict(M_Diff,newmods=Pred_C$H)$ci.ub

theme_set(theme_bw(base_size=10))
a<-ggplot(data=ROM,aes(x=exp(H_Diff)-1,y=exp(yi)-1))+geom_jitter(shape=1,size=3)
b<-a+geom_line(data=Pred_C,aes(x=exp(H)-1,y=exp(pred)-1),size=1)+geom_line(data=Pred_C,aes(x=exp(H)-1,y=exp(cilb)-1),size=0.5,lty=2)+geom_line(data=Pred_C,aes(x=exp(H)-1,y=exp(ciub)-1),size=0.5,lty=2)
c<-b+ylab("Proportional change in aboveground\ncarbon storage")
d<-c+xlab("Proportional difference between\ninvasive and native plant height")
d+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Figures")
ggsave("AGC_Height_Diff.png",height=4,width=6,units="in",dpi=400)

#now predict just using invasive height
Pred_C<-data.frame(H=seq(.2,2.5,length.out=100))
Pred_C$pred<-predict(M_Diff_H,newmods=Pred_C$H)$pred
Pred_C$cilb<-predict(M_Diff_H,newmods=Pred_C$H)$ci.lb
Pred_C$ciub<-predict(M_Diff_H,newmods=Pred_C$H)$ci.ub


theme_set(theme_bw(base_size=10))
a<-ggplot(data=ROM,aes(x=H_inv,y=exp(yi)-1))+geom_jitter(shape=1,size=3)
b<-a+geom_line(data=Pred_C,aes(x=H,y=exp(pred)-1),size=1)+geom_line(data=Pred_C,aes(x=H,y=exp(cilb)-1),size=0.5,lty=2)+geom_line(data=Pred_C,aes(x=H,y=exp(ciub)-1),size=0.5,lty=2)
c<-b+ylab("Proportional change in aboveground\ncarbon storage")
d<-c+xlab("Height of invasive species (m")
d+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Figures")
ggsave("AGC_Height.png",height=4,width=6,units="in",dpi=400)


