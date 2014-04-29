#script to analyse the links between water 
#quality changes and functional traits
library(metafor)
library(MuMIn)
library(gridExtra)

setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Data")

#import WQ data
WQ<-read.csv("WQ_height.csv")


ROM_WQ<-escalc("ROM",m1i=M_I,m2i=M_UI,sd1i=SE_I*sqrt(SS_I),sd2i=SE_UI*sqrt(SS_UI),n1i=SS_I,n2i=SS_UI,append=T,data=WQ)
ROM_WQ<-subset(ROM_WQ,!is.na(H_Diff))


#test the predictive ability of both traits
Null_mod<-rma(-yi,vi,data=ROM_WQ)
M_Diff_I<-rma(-yi,vi,mods=yi~I_Height,data=ROM_WQ)
M_Diff<-rma(-yi,vi,mods=yi~H_Diff,data=ROM_WQ)




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
write.csv("WQ_AIC.csv",x=AGC_sel,row.names=F)



#WQ and height
#create predictions for model including just invasive height
summary(WQ)
dfih_WQ<-data.frame(H_Diff=seq(-2.07,2.4,0.01))
WQ_preds<-predict(M_Diff,newmods=dfih_WQ$H_Diff,se.fit=T)
dfih_WQ$preds<-WQ_preds$pred
dfih_WQ$ci.lb<-WQ_preds$ci.lb
dfih_WQ$ci.ub<-WQ_preds$ci.ub


theme_set(theme_bw(base_size=12))
WQ_IH_plot1<-ggplot(data=ROM_WQ_HC,aes(x=I_Height,y=-(exp(yi)-1)))+geom_point(shape=1)
WQ_IH_plot2<-WQ_IH_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1,colour="black",fill=NA))
WQ_IH_plot3<-WQ_IH_plot2+xlab("Invasive species \nheight (m)")+ylab("Proportional difference\nin water quality indicator\nfollowing invasion")
WQ_IH_plot4<-WQ_IH_plot3+coord_cartesian(ylim=c(-5,1),xlim=c(0,30))

#WQ and height difference
theme_set(theme_bw(base_size=12))
WQ_Diff_plot1<-ggplot(data=ROM_WQ,aes(x=exp(H_Diff)-1,y=-(exp(yi)-1)))+geom_point(shape=1)
WQ_Diff_plot2<-WQ_Diff_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1,colour="black",fill=NA))
WQ_Diff_plot3<-WQ_Diff_plot2+xlab("Proportional difference in height between invasive \nand native species")+ylab("Proportional difference\nin water quality indicator\nfollowing invasion")
WQ_Diff_plot4<-WQ_Diff_plot3+ylim(-2,1)+xlim(-1,10)+geom_line(data=dfih_WQ,aes(y=exp(preds)-1))+geom_line(data=dfih_WQ,aes(y=exp(ci.lb)-1),lty=2)+geom_line(data=dfih_WQ,aes(y=exp(ci.ub)-1),lty=2)
setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Figures")
ggsave("WQ_Height.png",width=8,height=4,dpi=400,units="in")

pdf("WQ_Height.png",width=8,height=6)
grid.arrange(WQ_IH_plot4,WQ_Diff_plot4,ncol=2)
dev.off()

#produce AICc tables with r squared values
AICc_Height_WQ<-data.frame(AICc=c(AICc(Null_mod2),AICc(M_Diff_I),AICc(M_Diff)),
           R_squared=c(1-(logLik(Null_mod2)[1]/logLik(Null_mod2)[1]),
            1-(logLik(M_Diff_I)[1]/logLik(Null_mod2)[1]),
            1-(logLik(M_Diff)[1]/logLik(Null_mod2)[1])))

AICc_Height_WQ$delta<-AICc_Height_WQ$AICc-min(AICc_Height_WQ$AICc)

#relative liklihood of model
AICc_Height_WQ$rel_lik<-exp((AICc_Height_WQ$AICc[1]-AICc_Height_WQ$AICc)/2)

#calculate the AICc weight
AICc_Height_WQ$weight<-AICc_Height_WQ$rel_lik/(sum(AICc_Height_WQ$rel_lik))

#put in variabes
AICc_Height_WQ$Var<-c("Null model","Invasive height","Proportional difference in height")

#reorder sorting
AICc_Height_WQ<-AICc_Height_WQ[order(AICc_Height_WQ$AICc),]

setwd("~/My Dropbox/Work/PhD/Publications, Reports and Responsibilities/Chapters/3. Effects of invasive species on ecosystem services/Invasive_traits/Tables")

write.csv("WQ_AIC.csv",x=AICc_Height_AGC,row.names=F)

