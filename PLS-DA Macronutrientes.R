library(readxl);library(tidyr);library(dplyr)

Macronutrientes_UDA <- read_excel("Macronutrientes.xlsx", 
                             sheet = "macronutrientes")

metabolomica_UDA <- read_excel("Metabolomica.xlsx")

parametros_macronutrientes <- read_excel("Macronutrientes.xlsx",sheet = "Planilha1")

metabolomica_UDA <- separate(metabolomica_UDA,Name,into = c("Names"),sep = " ",remove = F)
metabolomica_UDA <- separate(metabolomica_UDA,Group,into = c("PrePos"),remove = F,sep = " ")

metabolomica_UDA <- filter(metabolomica_UDA,PrePos=="Pós")

count(metabolomica_UDA,Names)
count(Macronutrientes_UDA,Nomes)

setdiff(c(count(metabolomica_UDA,Names)$Names),c(count(Macronutrientes_UDA,Nomes)$Nomes))
setdiff(c(count(Macronutrientes_UDA,Nomes)$Nomes),c(count(metabolomica_UDA,Names)$Names))

Metabolomica_Nutrientes_UDA <- right_join(metabolomica_UDA,Macronutrientes_UDA,by = c("Names"="Nomes"))

#Criação das planilhas dos grupos

Metabolomica_Nutrientes_UDA$PTN_Kg_Dris <- Metabolomica_Nutrientes_UDA$`PTN/Kg`>0.66
Metabolomica_Nutrientes_UDA$PTN_Kg_Dobrow <- (Metabolomica_Nutrientes_UDA$`PTN/Kg`>1.2 & Metabolomica_Nutrientes_UDA$`PTN/Kg`<1.7)

Metabolomica_Nutrientes_UDA$CHO_Kg_Dobrow <- (Metabolomica_Nutrientes_UDA$`CHO/Kg`>7 & Metabolomica_Nutrientes_UDA$`CHO/Kg`<12)

Metabolomica_Nutrientes_UDA$CHO_percent_Dris <- (Metabolomica_Nutrientes_UDA$`% CHO`>45 & Metabolomica_Nutrientes_UDA$`% CHO`<65)

Metabolomica_Nutrientes_UDA$LIP_percent_Dris <- (Metabolomica_Nutrientes_UDA$`% LIP`>20 & Metabolomica_Nutrientes_UDA$`% LIP`<30)
Metabolomica_Nutrientes_UDA$LIP_percent_Dobrow <- (Metabolomica_Nutrientes_UDA$`% LIP`<20)

Metabolomica_Nutrientes_UDA$fibras_Dris_Dobrow <- Metabolomica_Nutrientes_UDA$`fibras (g)`>25


count(Metabolomica_Nutrientes_UDA,PTN_Kg_Dris)
count(Metabolomica_Nutrientes_UDA,PTN_Kg_Dobrow)

count(Metabolomica_Nutrientes_UDA,CHO_Kg_Dobrow)# Nao rodar

count(Metabolomica_Nutrientes_UDA,CHO_percent_Dris)

count(Metabolomica_Nutrientes_UDA,LIP_percent_Dris)
count(Metabolomica_Nutrientes_UDA,LIP_percent_Dobrow)#Nao Rodar

count(Metabolomica_Nutrientes_UDA,fibras_Dris_Dobrow)# Não rodar


library(ropls)

opls(Metabolomica_Nutrientes_UDA[9:51],as.character(Metabolomica_Nutrientes_UDA$PTN_Kg_Dris),predI=2)
opls(Metabolomica_Nutrientes_UDA[9:51],as.character(Metabolomica_Nutrientes_UDA$PTN_Kg_Dobrow),predI=2)
opls(Metabolomica_Nutrientes_UDA[9:51],as.character(Metabolomica_Nutrientes_UDA$CHO_percent_Dris),predI=2)
opls(Metabolomica_Nutrientes_UDA[9:51],as.character(Metabolomica_Nutrientes_UDA$LIP_percent_Dris),predI=2)
