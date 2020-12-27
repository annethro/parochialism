################################################################################
library(colorspace)
  Type <- c("Int",rep("Focal",9),rep("Alter",8),"Dyadic","Dyadic","Dyadic",rep("Parochial",4))
  
  IDs <- c("Int",
         "Age","Male","Embera","Cant Work","Grip Strength","Depressed","Food Insecure","Material Wealth","Religious",  
         "Age","Male","Cant Work","Grip Strength","Depressed","Food Insecure","Material Wealth","Out Migrated", 
         "Relatedness","Married","Same Sex",
         "Same Ethnicity - Afrocolombian","Same Ethnicity - Embera","Same Religion - None", "Same Religion - One")

maker1 <- function(X){
fit_Basic <- X
################################################################################
BG<-rstan::extract(fit_Basic,pars="BG")$BG
BL<-rstan::extract(fit_Basic,pars="BL")$BL
BP<-rstan::extract(fit_Basic,pars="BP")$BP
BT<-rstan::extract(fit_Basic,pars="BT")$BT
BF<-rstan::extract(fit_Basic,pars="BF")$BF
ZZ <- 0

Z <- rep(ZZ,25)
sample_eff<-apply(BG,2,quantile,probs=c(0.05,0.5,0.95))          
df_Hg<-data.frame(IDs=IDs, Type=Type, Group="Allocation",
                 LI=sample_eff[1,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 Median=sample_eff[2,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 HI=sample_eff[3,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1)
                 ) 
               
sample_eff<-apply(BL,2,quantile,probs=c(0.05,0.5,0.95))        
df_Hl<-data.frame(IDs=IDs, Type=Type, Group="Leaving (Taking)",
                 LI=sample_eff[1,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 Median=sample_eff[2,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 HI=sample_eff[3,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1)
                 ) 
                             
sample_eff<-apply(BP,2,quantile,probs=c(0.05,0.5,0.95))          
df_Hp<-data.frame(IDs=IDs, Type=Type, Group="Reducing",
                 LI=sample_eff[1,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 Median=sample_eff[2,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 HI=sample_eff[3,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1)
                 ) 

sample_eff<-apply(BT,2,quantile,probs=c(0.05,0.5,0.95))       
df_Ht<-data.frame(IDs=IDs, Type=Type, Group="Transfers",
                 LI=sample_eff[1,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 Median=sample_eff[2,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 HI=sample_eff[3,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1)
                 ) 

sample_eff<-apply(BF,2,quantile,probs=c(0.05,0.5,0.95))       
df_Hf<-data.frame(IDs=IDs, Type=Type, Group="Friendships",
                 LI=sample_eff[1,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 Median=sample_eff[2,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 HI=sample_eff[3,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1)
                 ) 


FA<-rstan::extract(fit_Basic,pars="FA_corr")$FA_corr
FA<-FA[,,1,2]
sample_eff<-apply(FA,2,quantile,probs=c(0.05,0.5,0.95))    
Z<-rep(ZZ,5)
df_fa<-data.frame(IDs="Generalized", Type="Reciprocity", Group=c("Allocation", "Leaving (Taking)", "Reducing", "Transfers", "Friendships"),
                 LI=sample_eff[1,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 Median=sample_eff[2,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 HI=sample_eff[3,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1)
                 ) 

DD<-rstan::extract(fit_Basic,pars="D_corr")$D_corr
DD<-DD[,,1,2]
sample_eff<-apply(DD,2,quantile,probs=c(0.05,0.5,0.95))    
Z<-rep(ZZ,5)
df_dd<-data.frame(IDs="Dyadic", Type="Reciprocity", Group=c("Allocation", "Leaving (Taking)", "Reducing", "Transfers", "Friendships"),
                 LI=sample_eff[1,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 Median=sample_eff[2,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1),
                 HI=sample_eff[3,]/ifelse(Z==1,sample_eff[3,]-sample_eff[1,],1)
                 ) 
                                                                
################################################################################
df_H1 <- rbind(df_Ht,df_Hg,df_Hl,df_Hp,df_Hf,df_fa,df_dd)

df_H1$Type <- factor(df_H1$Type, levels(df_H1$Type)[c(3,1,2,4,5,6)])

df_H1 <- df_H1[which(df_H1$Type != "Int"),]
colnames(df_H1)[1] <- "Variable"
return(df_H1)
}

df_H1c <- maker1(fit[[1]])
df_H1c$Loc <- "Coast"

df_H1i <- maker1(fit[[2]])
df_H1i$Loc <- "Inland"


df_H1c1 <- maker1(fit[[3]])
df_H1c1 <- df_H1c1[which(df_H1c1$Variable %in% c("Same Ethnicity - Afrocolombian","Same Ethnicity - Embera","Embera") ),]
df_H1c1$Loc <- "Coast NC"

df_H1i1 <- maker1(fit[[4]])
df_H1i1 <- df_H1i1[which(df_H1i1$Variable %in% c("Same Ethnicity - Afrocolombian","Same Ethnicity - Embera","Embera") ),]
df_H1i1$Loc <- "Inland NC"


df_H1c2 <- maker1(fit[[5]])
df_H1c2 <- df_H1c2[which(df_H1c2$Variable %in% c("Same Religion - None", "Same Religion - One", "Religious") ),]
df_H1c2$Loc <- "Coast NC"

df_H1i2 <- maker1(fit[[6]])
df_H1i2 <- df_H1i2[which(df_H1i2$Variable %in% c("Same Religion - None", "Same Religion - One", "Religious") ),]
df_H1i2$Loc <- "Inland NC"



df_H1 <- rbind(df_H1c,df_H1i,df_H1c1,df_H1i1,df_H1c2,df_H1i2)
df_H1$Group <- factor(df_H1$Group, levels(df_H1$Group)[c(5,1,2,3,4)])
df_H1$Variable <- factor(df_H1$Variable, levels(df_H1$Variable)[c(rev(c(7,1,2,3,5,6,8,10,11,4,13)),9,12,18,16,17,15,14,19,20)])
df_H1$Loc <- factor(df_H1$Loc)


p <- ggplot(df_H1,aes(x=Variable,y=Median,ymin=LI,ymax=HI,color=Loc))+ 
     geom_linerange(size=1,aes(color=Loc), position = position_dodge2(width=0.5,preserve = "total",padding = 1))+
     geom_point(size=2,aes(color=Loc), position = position_dodge2(width=0.5,preserve = "total",padding = 1))+
     facet_grid(Type~Group,scales="free",space = "free_y")+geom_hline(aes(yintercept=0),color="black",linetype="dashed")+
     labs(y="Regression parameters", x="") + theme(strip.text.x = element_text(size=14,face="bold"), 
     strip.text.y = element_text(size=14,face="bold"),axis.text=element_text(size=12),axis.title.y=element_text(size=14,
     face="bold"), axis.title.x=element_blank())+theme(strip.text.y = element_text(angle = 360))  + coord_flip() + theme(panel.spacing = unit(1, "lines")) +
     scale_color_manual(values=c("Coast"="royalblue4","Inland"="orange3","Coast NC"="royalblue1","Inland NC"="orange1"))

 p  
   
ggsave("All_Games-NonStandardized_SRM.pdf",p,height=8, width=14)

                  
