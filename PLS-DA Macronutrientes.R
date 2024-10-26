library(readxl);library(tidyr);library(dplyr)

Macronutrientes_UDA <- read_excel("Macronutrientes.xlsx", 
                             sheet = "macronutrientes")

metabolomica_UDA <- read_excel("Metabolomica.xlsx")

parametros_macronutrientes <- read_excel("Macronutrientes.xlsx",sheet = "Planilha1")

metabolomica_UDA <- separate(metabolomica_UDA,Name,into = c("Names"),sep = " ",remove = F)
metabolomica_UDA <- separate(metabolomica_UDA,Group,into = c("PrePos"),remove = F,sep = " ")


count(metabolomica_UDA,Names)
count(Macronutrientes_UDA,Nomes)

setdiff(c(count(metabolomica_UDA,Names)$Names),c(count(Macronutrientes_UDA,Nomes)$Nomes))
setdiff(c(count(Macronutrientes_UDA,Nomes)$Nomes),c(count(metabolomica_UDA,Names)$Names))

Metabolomica_Nutrientes_UDA <- right_join(metabolomica_UDA,Macronutrientes_UDA,by = c("Names"="Nomes"))

Metabolomica_Nutrientes_UDA <- filter(Metabolomica_Nutrientes_UDA,PrePos%in%c("Pós","Pré"))

colors=c("blue","red")
library(ropls);library(NAPRMN);library(ggplot2);library(ggpubr)

Metabolomica_Nutrientes_Norm <- DataNormalization(Metabolomica_Nutrientes_UDA[9:51],rowNorm = "SumNorm",scaleNorm = "ParetoNorm")

Metabolomica_Nutrientes_Norm <- cbind(Metabolomica_Nutrientes_UDA[1:8],Metabolomica_Nutrientes_Norm,Metabolomica_Nutrientes_UDA[52:66])


OPLS_prepos <- opls(Metabolomica_Nutrientes_Norm[9:51],Metabolomica_Nutrientes_Norm$PrePos,predI=1,orthoI=1
                    ,plotSubC="Pre x Pos")
OPLS_prepos_data <- extract_ropls_data(OPLS_prepos)
OPLS_prepos_data$Loadings$bins <- rownames(OPLS_prepos_data$Loadings)

scores <- ggscatter(OPLS_prepos_data$Scores,x="p1",y="o1",color="Group",size = 4,label = "Samples",
                    font.label = c(14,"plain","black"))+
  stat_ellipse(aes(color=Group,fill=Group),geom = "polygon",alpha=0.3)+
  scale_color_manual(values = colors)+
  scale_fill_manual(values = colors)+
  labs(x=paste("Predictive Comp"," (",OPLS_prepos@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_prepos@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Scores) - Pre x Pos")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))
png("OPLS-DA (Scores) - Pre x Pos.png",width = 4600,height = 3200,units = "px",res = 300)
scores
dev.off()

loading <- ggscatter(OPLS_prepos_data$Loadings,x="p1",y="o1",
                     size = 4,label = "bins",color="Vip",point = T,repel = T
                     ,font.label = c(20,"plain"))+
  scale_color_gradientn(colours = c("#ff0000","black"),values = c(1.5,0.5,0))+
  labs(x=paste("Predictive Comp"," (",OPLS_prepos@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_prepos@modelDF$R2X[2]*100,"%)"),
        title = "OPLS-DA (Loading) - Pre x Pos")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))

png("OPLS-DA (Loading) - Pre x Pos.png",width = 4600,height = 3200,units = "px",res = 300)
loading
dev.off()

Metabolomica_Nutrientes_Norm <- filter(Metabolomica_Nutrientes_Norm,PrePos=="Pós")

#Criação das planilhas dos grupos

Metabolomica_Nutrientes_Norm$PTN_Kg_Dris <- Metabolomica_Nutrientes_Norm$`PTN/Kg`>0.66
Metabolomica_Nutrientes_Norm$PTN_Kg_Dobrow <- (Metabolomica_Nutrientes_Norm$`PTN/Kg`>1.2 & Metabolomica_Nutrientes_Norm$`PTN/Kg`<1.7)

Metabolomica_Nutrientes_Norm$CHO_Kg_Dobrow <- (Metabolomica_Nutrientes_Norm$`CHO/Kg`>7 & Metabolomica_Nutrientes_Norm$`CHO/Kg`<12)

Metabolomica_Nutrientes_Norm$CHO_percent_Dris <- (Metabolomica_Nutrientes_Norm$`% CHO`>45 & Metabolomica_Nutrientes_Norm$`% CHO`<65)

Metabolomica_Nutrientes_Norm$LIP_percent_Dris <- (Metabolomica_Nutrientes_Norm$`% LIP`>20 & Metabolomica_Nutrientes_Norm$`% LIP`<30)
Metabolomica_Nutrientes_Norm$LIP_percent_Dobrow <- (Metabolomica_Nutrientes_Norm$`% LIP`<20)

Metabolomica_Nutrientes_Norm$fibras_Dris_Dobrow <- Metabolomica_Nutrientes_Norm$`fibras (g)`>25

Metabolomica_Nutrientes_Norm$TMB_comp <-  Metabolomica_Nutrientes_Norm$`Kcal cálculo`>=Metabolomica_Nutrientes_Norm$TMB

count(Metabolomica_Nutrientes_Norm,PTN_Kg_Dris)
count(Metabolomica_Nutrientes_Norm,PTN_Kg_Dobrow)

count(Metabolomica_Nutrientes_Norm,CHO_Kg_Dobrow)# Nao rodar

count(Metabolomica_Nutrientes_Norm,CHO_percent_Dris)

count(Metabolomica_Nutrientes_Norm,LIP_percent_Dris)
count(Metabolomica_Nutrientes_Norm,LIP_percent_Dobrow)#Nao Rodar

count(Metabolomica_Nutrientes_Norm,fibras_Dris_Dobrow)# Não rodar

count(Metabolomica_Nutrientes_Norm,TMB_comp)


colors=c("red","blue")

library(ropls)

OPLS_PTN_Kg_Dris <- opls(Metabolomica_Nutrientes_Norm[9:51],as.character(Metabolomica_Nutrientes_Norm$PTN_Kg_Dris),
                         predI=1,orthoI=1,plotSubC="PTN/Kg Dris")

OPLS_PTN_Kg_Dris_data <- extract_ropls_data(OPLS_PTN_Kg_Dris)
OPLS_PTN_Kg_Dris_data$Loadings$bins <- rownames(OPLS_PTN_Kg_Dris_data$Loadings)

scores <- ggscatter(OPLS_PTN_Kg_Dris_data$Scores,x="p1",y="o1",color="Group",size = 4,label = "Samples",
                    font.label = c(14,"plain","black"))+
  stat_ellipse(aes(color=Group,fill=Group),geom = "polygon",alpha=0.3)+
  scale_color_manual(values = colors,labels=c("Inadequado","Adequado"))+
  scale_fill_manual(values = colors,labels=c("Inadequado","Adequado"))+
  labs(x=paste("Predictive Comp"," (",OPLS_PTN_Kg_Dris@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_PTN_Kg_Dris@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Scores) - PTN/Kg (Dris)")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))
png("OPLS-DA (Scores) - PTN_Kg (Dris).png",width = 4600,height = 3200,units = "px",res = 300)
scores
dev.off()

loading <- ggscatter(OPLS_PTN_Kg_Dris_data$Loadings,x="p1",y="o1",
                     size = 4,label = "bins",color="Vip",point = T,repel = T
                     ,font.label = c(20,"plain"))+
  scale_color_gradientn(colours = c("#ff0000","black"),values = c(1.5,0.5,0))+
  labs(x=paste("Predictive Comp"," (",OPLS_PTN_Kg_Dris@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_PTN_Kg_Dris@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Loading) - PTN/Kg (Dris)")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))

png("OPLS-DA (Loading) - PTN_Kg (Dris).png",width = 4600,height = 3200,units = "px",res = 300)
loading
dev.off()


PLS_PTN_Kg_Dobrow <- opls(Metabolomica_Nutrientes_Norm[9:51],as.character(Metabolomica_Nutrientes_Norm$PTN_Kg_Dobrow),
                           predI=1,orthoI=NA,plotSubC="PTN/Kg Dobrow")

PLS_PTN_Kg_Dobrow_data <- extract_ropls_data(PLS_PTN_Kg_Dobrow)
PLS_PTN_Kg_Dobrow_data$Loadings$bins <- rownames(PLS_PTN_Kg_Dobrow_data$Loadings)

scores <- ggscatter(PLS_PTN_Kg_Dobrow_data$Scores,x="p1",y="o1",color="Group",size = 4,label = "Samples",
                    font.label = c(14,"plain","black"))+
  stat_ellipse(aes(color=Group,fill=Group),geom = "polygon",alpha=0.3)+
  scale_color_manual(values = colors,labels=c("Inadequado","Adequado"))+
  scale_fill_manual(values = colors,labels=c("Inadequado","Adequado"))+
  labs(x=paste("Predictive Comp"," (",PLS_PTN_Kg_Dobrow@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",PLS_PTN_Kg_Dobrow@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Scores) - PTN/Kg (Dobrow)")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))
png("OPLS-DA (Scores) - PTN_Kg (Dobrow).png",width = 4600,height = 3200,units = "px",res = 300)
scores
dev.off()

loading <- ggscatter(PLS_PTN_Kg_Dobrow_data$Loadings,x="p1",y="o1",
                     size = 4,label = "bins",color="Vip",point = T,repel = T
                     ,font.label = c(20,"plain"))+
  scale_color_gradientn(colours = c("#ff0000","black"),values = c(1.5,0.5,0))+
  labs(x=paste("Predictive Comp"," (",PLS_PTN_Kg_Dobrow@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",PLS_PTN_Kg_Dobrow@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Loading) - PTN/Kg (Dobrow)")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))

png("OPLS-DA (Loading) - PTN_Kg (Dobrow).png",width = 4600,height = 3200,units = "px",res = 300)
loading
dev.off()


OPLS_CHO_percent_Dris <- opls(Metabolomica_Nutrientes_Norm[9:51],as.character(Metabolomica_Nutrientes_Norm$CHO_percent_Dris),
                              predI=1,orthoI=1,plotSubC="CHO (%) Dris")

OPLS_CHO_percent_Dris_data <- extract_ropls_data(OPLS_CHO_percent_Dris)
OPLS_CHO_percent_Dris_data$Loadings$bins <- rownames(OPLS_CHO_percent_Dris_data$Loadings)

scores <- ggscatter(OPLS_CHO_percent_Dris_data$Scores,x="p1",y="o1",color="Group",size = 4,label = "Samples",
                    font.label = c(14,"plain","black"))+
  stat_ellipse(aes(color=Group,fill=Group),geom = "polygon",alpha=0.3)+
  scale_color_manual(values = colors,labels=c("Inadequado","Adequado"))+
  scale_fill_manual(values = colors,labels=c("Inadequado","Adequado"))+
  labs(x=paste("Predictive Comp"," (",OPLS_CHO_percent_Dris@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_CHO_percent_Dris@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Scores) - CHO % (Dris)")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))
png("OPLS-DA (Scores) - CHO percent (Dris).png",width = 4600,height = 3200,units = "px",res = 300)
scores
dev.off()

loading <- ggscatter(OPLS_CHO_percent_Dris_data$Loadings,x="p1",y="o1",
                     size = 4,label = "bins",color="Vip",point = T,repel = T
                     ,font.label = c(20,"plain"))+
  scale_color_gradientn(colours = c("#ff0000","black"),values = c(1.5,0.5,0))+
  labs(x=paste("Predictive Comp"," (",OPLS_CHO_percent_Dris@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_CHO_percent_Dris@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Loading) - CHO % (Dris)")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))

png("OPLS-DA (Loading) - CHO percent Dris).png",width = 4600,height = 3200,units = "px",res = 300)
loading
dev.off()



OPLS_LIP_percent_Dris <- opls(Metabolomica_Nutrientes_Norm[9:51],as.character(Metabolomica_Nutrientes_Norm$LIP_percent_Dris),
                              predI=1,orthoI=1,plotSubC="Lip (%) Dris")

OPLS_LIP_percent_Dris_data <- extract_ropls_data(OPLS_LIP_percent_Dris)
OPLS_LIP_percent_Dris_data$Loadings$bins <- rownames(OPLS_LIP_percent_Dris_data$Loadings)

scores <- ggscatter(OPLS_LIP_percent_Dris_data$Scores,x="p1",y="o1",color="Group",size = 4,label = "Samples",
                    font.label = c(14,"plain","black"))+
  stat_ellipse(aes(color=Group,fill=Group),geom = "polygon",alpha=0.3)+
  scale_color_manual(values = colors,labels=c("Inadequado","Adequado"))+
  scale_fill_manual(values = colors,labels=c("Inadequado","Adequado"))+
  labs(x=paste("Predictive Comp"," (",OPLS_LIP_percent_Dris@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_LIP_percent_Dris@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Scores) - Lip (%) Dris")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))
png("OPLS-DA (Scores) - Lip (Percent) Dris.png",width = 4600,height = 3200,units = "px",res = 300)
scores
dev.off()

loading <- ggscatter(OPLS_LIP_percent_Dris_data$Loadings,x="p1",y="o1",
                     size = 4,label = "bins",color="Vip",point = T,repel = T
                     ,font.label = c(20,"plain"))+
  scale_color_gradientn(colours = c("#ff0000","black"),values = c(1.5,0.5,0))+
  labs(x=paste("Predictive Comp"," (",OPLS_LIP_percent_Dris@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_LIP_percent_Dris@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Loading) - Lip (%) Dris")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))

png("OPLS-DA (Loading) - Lip (Percent) Dris.png",width = 4600,height = 3200,units = "px",res = 300)
loading
dev.off()


OPLS_TMB_comp <- opls(Metabolomica_Nutrientes_Norm[9:51],as.character(Metabolomica_Nutrientes_Norm$TMB_comp),
                      predI=1,orthoI=1,plotSubC="TMB")

OPLS_TMB_comp_data <- extract_ropls_data(OPLS_TMB_comp)
OPLS_TMB_comp_data$Loadings$bins <- rownames(OPLS_TMB_comp_data$Loadings)

scores <- ggscatter(OPLS_TMB_comp_data$Scores,x="p1",y="o1",color="Group",size = 4,label = "Samples",
                    font.label = c(14,"plain","black"))+
  stat_ellipse(aes(color=Group,fill=Group),geom = "polygon",alpha=0.3)+
  scale_color_manual(values = colors,labels=c("Inadequado","Adequado"))+
  scale_fill_manual(values = colors,labels=c("Inadequado","Adequado"))+
  labs(x=paste("Predictive Comp"," (",OPLS_TMB_comp@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_TMB_comp@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Scores) - TMB")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))
png("OPLS-DA (Scores) - TMB.png",width = 4600,height = 3200,units = "px",res = 300)
scores
dev.off()

loading <- ggscatter(OPLS_TMB_comp_data$Loadings,x="p1",y="o1",
                     size = 4,label = "bins",color="Vip",point = T,repel = T
                     ,font.label = c(20,"plain"))+
  scale_color_gradientn(colours = c("#ff0000","black"),values = c(1.5,0.5,0))+
  labs(x=paste("Predictive Comp"," (",OPLS_TMB_comp@modelDF$R2X[1]*100,"%)"),
       y=paste("Orthogonal Comp"," (",OPLS_TMB_comp@modelDF$R2X[2]*100,"%)"),
       title = "OPLS-DA (Loading) - TMB")+
  theme_bw()+theme(legend.text = element_text(size = 14),
                   axis.text = element_text(size=14),
                   axis.title = element_text(size = 14))

png("OPLS-DA (Loading) - TMB.png",width = 4600,height = 3200,units = "px",res = 300)
loading
dev.off()
