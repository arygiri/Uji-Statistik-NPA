#import data
library(readxl)
Data <- read_excel ("Hasil Kandungan NPA.xlsx",sheet = "Sheet2")
head (Data)
tail(Data)
str(Data)
fix(Data)
#merubah kolom yang faktor
Data$Lokasi <- as.factor(Data$Lokasi)
Data$Titik <- as.factor(Data$Titik)
Data$Titik<- factor(Data$Titik, levels = c("AB1", "AB2", "AB3", "B1", "B2", "B3", "B4", "P1", "P2", "P3", "MK1", "MK2", "MK3", "MKG1", "MKG2", "MKG3", "BT1", "BT2", "BT3"))
str((Data))
Data
summary(Data)

library(ggpubr)

#membuat boxplot data
ggboxplot(Data, x = "Titik", y= "Nitrat",
          color = "Black",fill="Titik", ylab = "Nitrat", xlab = "Titik") + 
  theme(legend.position ="none")+facet_grid(.~Lokasi, labeller= label_both)
ggsave("Boxplot Nitrat.png", width = 20, height = 10, dpi = 500)             

ggboxplot(Data, x = "Titik", y= "Fosfat",
          color = "Black", fill= "Titik", ylab = "Fosfat", xlab = "Titik") + 
  theme(legend.position ="none")+facet_grid(.~Lokasi, labeller= label_both)
ggsave("Boxplot Fosfat.png", width = 20, height = 10, dpi = 500)

ggboxplot(Data, x = "Titik", y= "Amonia",
          color = "Black", fill= "Titik", ylab = "Amonia", xlab = "Titik") + 
  theme(legend.position ="none")+facet_grid(.~Lokasi, labeller= label_both)
ggsave("Boxplot Amonia.png", width = 20, height = 10, dpi = 500)

DataBKL <- read_excel("Hasil Kandungan NPA.xlsx", sheet = "BANGKALAN")
DataBKL$Titik <- as.factor(DataBKL$Titik)
DataBKL$Titik<- factor(DataBKL$Titik, levels = c("AB1", "AB2", "AB3", "B1","B2", "B3", "P1", "P2", "P3", "MK1", "MK2", "MK3"))

DataPMK <- read_excel("Hasil Kandungan NPA.xlsx", sheet = "PAMEKASAN")
DataPMK$Titik <- as.factor(DataPMK$Titik)
DataPMK$Titik<- factor(DataPMK$Titik, levels = c("AB1", "AB2", "AB3", "B1","B2", "B3", "B4", "P1", "P2", "P3", "BT1", "BT2", "BT3", "MKG1", "MKG2", "MKG3"))
fix(DataPMK)
str(DataBKL)


#Uji NORMALITAS#

shapiro.test(DataBKL$Nitrat)
shapiro.test(DataBKL$Fosfat)
shapiro.test(DataBKL$Amonia)



# Uji Beda ----------------------------------------------------------------
#UJI STATISTIKA
library(DescTools)
library(multcomp)
library(FSA)
#uji anova data Bangkalan
DTNitrat1<- aov(Nitrat~Titik, DataBKL)
DTFosfat1<- aov(Fosfat~Titik, DataBKL)
DTAmonia1<- aov(Amonia~Titik, DataBKL)

#uji anova data pamekasan
DTNitrat2<- aov(Nitrat~Titik, DataPMK)
DTFosfat2<- aov(Fosfat~Titik, DataPMK)
DTAmonia2<- aov(Amonia~Titik, DataPMK)

#uji lanjut data bangkalan
PTNitrat1<- glht(DTNitrat1, linfct = mcp(Titik="Tukey"))
PTFosfat1<- glht(DTFosfat1, linfct = mcp(Titik="Tukey"))
PTAmonia1<- glht(DTAmonia1, linfct = mcp(Titik="Tukey"))

#uji lanjut data pamekasan
PTNitrat2<- glht(DTNitrat2, linfct = mcp(Titik="Tukey"))
PTFosfat2<- glht(DTFosfat2, linfct = mcp(Titik="Tukey"))
PTAmonia2<- glht(DTAmonia2, linfct = mcp(Titik="Tukey"))

#menampilkan huruf yang berbeda signifikan pd data bangkalan
cld(PTNitrat1)
cld(PTFosfat1)
cld(PTAmonia1)

#menampilkan huruf yang berbeda signifikan pd data pamekasan
cld(PTNitrat2)
cld(PTFosfat2)
cld(PTAmonia2)
