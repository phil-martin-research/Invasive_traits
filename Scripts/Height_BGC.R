#script to analyse the links between belowground 
#carbon changes and functional traits
library(metafor)
library(MuMIn)

setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Data")
#do the same with below ground carbon
BGC<-read.csv("BGC_height.csv")

ROM_BGC<-escalc("ROM",m1i=M_I,m2i=M_UI,sd1i=SE_I*sqrt(SS_I),sd2i=SE_UI*sqrt(SS_UI),n1i=SS_I,n2i=SS_UI,append=T,data=BGC)

#test the predictive ability of both traits
Null_mod<-rma(yi,vi,data=ROM_BGC)
M_Diff<-rma(yi,vi,mods=yi~H_Diff,data=ROM_BGC)
M_Diff_I<-rma(yi,vi,mods=yi~I_H,data=ROM_BGC)

summary(M_Diff_I)

#make model selection table
AGC_sel<-data.frame(Model=c("Null model","Invasive trait","Invasive and native traits"))
AGC_sel$AICc<-c(AICc(Null_mod),AICc(M_Diff_I),AICc(M_Diff))
AGC_sel$Delta<-AGC_sel$AICc-min(AGC_sel$AICc)
AGC_sel$R2<-c(1-(logLik(Null_mod)[1]/logLik(Null_mod)[1]),
            1-(logLik(M_Diff_I)[1]/logLik(Null_mod)[1]),
            1-(logLik(M_Diff)[1]/logLik(Null_mod)[1]))
AGC_sel$rel_lik<-exp((AGC_sel$AICc[1]-AGC_sel$AICc)/2)
AGC_sel$weight<-AGC_sel$rel_lik/(sum(AGC_sel$rel_lik))
#sort so that best model is top
AGC_sel<-AGC_sel[with(AGC_sel, order(Delta)), ]
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Tables")
write.csv("BGC_AIC.csv",x=AGC_sel,row.names=F)

#WQ and height
#create predictions for model including both
dfih_BGC<-data.frame(H_Diff=seq(-2,2.32,0.01))
BGC_preds<-predict(M_Diff,newmods=dfih_BGC$H_Diff,se.fit=T)
dfih_BGC$preds<-BGC_preds$pred
dfih_BGC$ci.lb<-BGC_preds$ci.lb
dfih_BGC$ci.ub<-BGC_preds$ci.ub

#BCG and height difference
theme_set(theme_bw(base_size=12))
WQ_Diff_plot1<-ggplot(data=ROM_BGC,aes(x=exp(H_Diff)-1,y=(exp(yi)-1)))+geom_point(shape=1)
WQ_Diff_plot2<-WQ_Diff_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1,colour="black",fill=NA))
WQ_Diff_plot3<-WQ_Diff_plot2+xlab("Proportional difference in height between invasive \nand native species")+ylab("Proportional difference\nin belowground carbon storage\nfollowing invasion")
WQ_Diff_plot4<-WQ_Diff_plot3+geom_line(data=dfih_BGC,aes(y=exp(preds)-1))+geom_line(data=dfih_BGC,aes(y=exp(ci.lb)-1),lty=2)+geom_line(data=dfih_BGC,aes(y=exp(ci.ub)-1),lty=2)
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Figures")
ggsave("BGC_Height.png",width=8,height=4,dpi=400,units="in")

