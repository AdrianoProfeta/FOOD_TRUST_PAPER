# data analysis consumer trust

# clear working space
rm(list = ls())

# load libraries
library(labelled)
library(dplyr)
library(likert)
library(ggplot2)
library(scales)
library(xtable)
library(psych)
library(tidyr)
library(plyr)
library(stringr)
library(ggtext)
require(gridExtra)
library(magick)
library(grid)
library(corrplot)
library(lavaan)
library(semPlot)
library(semTable)

# loading dataset ds 
# path must be adapted via setwd()command to the path where the data-file ds.rds is saved
setwd("/Users/adrianoprofeta/Nextcloud/DIL CONSUMER SCIENCE/6_SURVEY_DATA/2020/6_Vertrauensstudie/3_DATA/")  
ds<- readRDS("ds.rds")

myFont3 <- "Palatino"
# helper-function
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(as.numeric(x[[col]]), na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


# age, gender, school
table(ds$V145)
ds$agegroups<-0
ds$agegroups[ds$V145<30]<-1
ds$agegroups[ds$V145>29 & ds$V145<40]<-2
ds$agegroups[ds$V145>39 & ds$V145<50]<-3
ds$agegroups[ds$V145>49 & ds$V145<60]<-4
ds$agegroups[ds$V145>59]<-5
ds$agegroups_fac <- factor(ds$agegroups,
                           levels = c(1,2,3,4,5),
                           labels = c("18-29 Jahre","30-39 Jahre","40-49 Jahre","50-59 Jahre","60-69 Jahre"))
age<-as.data.frame(round((prop.table(table(ds$agegroups_fac))*100),1))
### gender
table(ds$V129)
gender_d<-round((prop.table(table(ds$V129))*100),1)
gender_d<-as.data.frame(gender_d[1:2])

### federal state
table(ds$MQ45)
fstate<-as.data.frame(round((prop.table(table(ds$MQ45))*100),1))

### education
table(ds$V131)
edu<-as.data.frame(round((prop.table(table(ds$V131))*100),1))

#################################################
# Start Table 2. Socio-demographics of the sample.
# age, gender, school
table(ds$V145)
ds$agegroups<-0
ds$agegroups[ds$V145<30]<-1
ds$agegroups[ds$V145>29 & ds$V145<40]<-2
ds$agegroups[ds$V145>39 & ds$V145<50]<-3
ds$agegroups[ds$V145>49 & ds$V145<60]<-4
ds$agegroups[ds$V145>59]<-5
ds$agegroups_fac <- factor(ds$agegroups,
                           levels = c(1,2,3,4,5),
                           labels = c("18-29 Jahre","30-39 Jahre","40-49 Jahre","50-59 Jahre","60-69 Jahre"))
age<-as.data.frame(round((prop.table(table(ds$agegroups_fac))*100),1))
### gender
table(ds$V129)
gender_d<-round((prop.table(table(ds$V129))*100),1)
gender_d<-as.data.frame(gender_d[1:2])

### federal state
table(ds$MQ45)
fstate<-as.data.frame(round((prop.table(table(ds$MQ45))*100),1))

### education
table(ds$V131)
edu<-as.data.frame(round((prop.table(table(ds$V131))*100),1))

sozio<-cbind(as.data.frame(c("Geschlecht","","Altersgruppen","","","","","Schulabschluss","","","Bundesland","","","","","","","","","","","","","","",""))        ,rbind(gender_d,age,edu[2:4,],fstate[1:16,]))
colnames(sozio)<-c("Merkmal","Ausprägung","%")
sozio_table<-xtable(sozio)
print.xtable(sozio_table, type="html", file="soziodemographie.html")
# End table 2
#####



#################################################################
# Start Figure 2. Trust in different actors in the food sector. #
#################################################################
items12_b <- ds[,substr(names(ds), 1,5) == 'TA02_']
items12_c <- lapply(items12_b , factor, levels = 1:5) 
items12_d <- as.data.frame(items12_c)

items12_d$TA02_01<-recode_factor(items12_d$TA02_01, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_02<-recode_factor(items12_d$TA02_02, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_03<-recode_factor(items12_d$TA02_03, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_04<-recode_factor(items12_d$TA02_04, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_05<-recode_factor(items12_d$TA02_05, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_06<-recode_factor(items12_d$TA02_06, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_07<-recode_factor(items12_d$TA02_07, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_08<-recode_factor(items12_d$TA02_08, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_09<-recode_factor(items12_d$TA02_09, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_10<-recode_factor(items12_d$TA02_10, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_11<-recode_factor(items12_d$TA02_11, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
items12_d$TA02_12<-recode_factor(items12_d$TA02_12, `1` = 'very low trust', `2` = 'low trust', `3` = 'partly/partly',`4` = 'high trust',`5` = 'very high trust')
colnames(items12_d)<-c("conventional farmers(1)","conventional retailers(5)","conventional food producers(3)","German authorities(7)",
                       "EU-authorities","organic farmers(2)","organic food producers(4)","organic retailers(6)","food influencer","food-book authors","nutrional science/medicine",
                       "organic food control")
likertscale<-likert(items12_d)
#### Tukey_test
a<-as.data.frame(ds$TA02_01)
b<-as.data.frame(ds$TA02_02)
c<-as.data.frame(ds$TA02_03)
d<-as.data.frame(ds$TA02_04)
e<-as.data.frame(ds$TA02_05)
f<-as.data.frame(ds$TA02_06)
g<-as.data.frame(ds$TA02_07)
h<-as.data.frame(ds$TA02_08)
i<-as.data.frame(ds$TA02_09)
j<-as.data.frame(ds$TA02_10)
k<-as.data.frame(ds$TA02_11)
l<-as.data.frame(ds$TA02_12)

colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
colnames(h)<-"mittelwert"
colnames(i)<-"mittelwert"
colnames(j)<-"mittelwert"
colnames(k)<-"mittelwert"
colnames(l)<-"mittelwert"

competence_mean<-rbind(a,b,c,d,e,f,g,h,i,j,k,l)
h<-as.data.frame(rep(c("Conventional Farmer","Conventional Retail","Conventional Food Prod.","German Authorities",
                       "EU-authorities","Organic Farmer","Organic Food Prod.","Organic Retail","Food Influencer","Food-Book authors","Nutrional Science/Medicine",
                       "Organic Food Control"), each = length(competence_mean[,1])/12))
colnames(h)<-"Actor"
competence_tukey_test<-cbind(competence_mean,h)
library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Actor, data=competence_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Akteur",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "Actor",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
liste<-outUn$groups[2]

quartz()
dev.new()
myFont3 <- "Palatino"
skala4 <- plot(likertscale, text.size=2.5) +theme(axis.text = element_text(size=8))
sizeannotate<-3.5
skala5<-skala4  + scale_fill_manual(values=c('very low trust'='#FF0000','low trust'='#cf6e6e' , 'partly/partly'='#D7D5D3','high trust'='#93cf6e','very high trust'= '#2d8b39'), 
                                    name='Answers:', breaks = c('very low trust','low trust',
                                                                'partly/partly','high trust','very high trust')) +
  theme_classic(base_family="Palatino")+
  theme( legend.text = element_text(size=8),legend.title=element_text(size=8), 
         axis.text.y = element_text(color = c("blue","blue","blue","blue","black","blue","blue","blue","black","black","black","black")),
         legend.key.size = unit(.15, 'cm'), legend.position=c(-.18,-.15), legend.justification = 'left',legend.box.just = 'left',legend.direction = 'horizontal',   axis.title.x = element_text(size=8),
         plot.margin = unit(c(.1,.1,.7,-.35), "cm" ),legend.spacing.x = unit(0.04, 'cm'))+
  labs(y = "%")+
  annotate("text", x=12, y=85, label=liste[1,1], size = sizeannotate,color = 'blue', family = myFont3) + 
  annotate("text", x=11, y=85, label = liste[2,1], size = sizeannotate,color = 'black', family = myFont3)+
  annotate("text", x=10, y=85, label= liste[3,1], size = sizeannotate,color = 'black', family = myFont3) + 
  annotate("text", x=9, y=85, label= liste[4,1], size = sizeannotate,color = 'blue', family = myFont3) + 
  annotate("text", x=8, y=85, label = liste[5,1], size = sizeannotate,color = 'blue', family = myFont3)+
  annotate("text", x=7, y=85, label= liste[6,1], size = sizeannotate,color = 'blue', family = myFont3) + 
  annotate("text", x=6, y=85, label= liste[8,1], size = sizeannotate,color = 'blue', family = myFont3) + 
  annotate("text", x=5, y=85, label = liste[7,1], size = sizeannotate,color = 'blue', family = myFont3)+
  annotate("text", x=4, y=85, label= liste[10,1], size = sizeannotate,color = 'black', family = myFont3) + 
  annotate("text", x=3, y=85, label= liste[9,1], size = sizeannotate,color = 'blue', family = myFont3) + 
  annotate("text", x=2, y=85, label = liste[11,1], size = sizeannotate,color = 'black', family = myFont3)+
  annotate("text", x=1, y=85, label= liste[12,1], size = sizeannotate,color = 'black', family = myFont3)

setwd("/Users/adrianoprofeta/Nextcloud/DIL CONSUMER SCIENCE/6_SURVEY_DATA/2020/6_Vertrauensstudie/5_FIGURES/") 
ggsave(skala5, filename = "Figure_2.png", 
       width = 13.7, height = 9, unit="cm", dpi = 3000, type = "cairo")

################
# End Figure 2 #
################

####################################################################
# Start Figure 3. Trust in selected labels and supported awareness #
####################################################################

items12_b <- ds[,substr(names(ds), 1,5) == 'SI01_']
items12_c <- lapply(items12_b , factor, levels = 1:5) # factor variable kreieren
items12_d <- as.data.frame(items12_c)


items12_d$SI01_01<-recode_factor(items12_d$SI01_01, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_02<-recode_factor(items12_d$SI01_02, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_03<-recode_factor(items12_d$SI01_03, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_04<-recode_factor(items12_d$SI01_04, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_05<-recode_factor(items12_d$SI01_05, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_06<-recode_factor(items12_d$SI01_06, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_07<-recode_factor(items12_d$SI01_07, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_08<-recode_factor(items12_d$SI01_08, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_09<-recode_factor(items12_d$SI01_09, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_10<-recode_factor(items12_d$SI01_10, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_11<-recode_factor(items12_d$SI01_11, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_12<-recode_factor(items12_d$SI01_12, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_13<-recode_factor(items12_d$SI01_13, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_14<-recode_factor(items12_d$SI01_14, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")
items12_d$SI01_15<-recode_factor(items12_d$SI01_15, `1` = "very low trust", `2` = "low trust", `3` = "partly/partly",`4` = "high trust",`5` = "very high trust")

colnames(items12_d)<-c("German Organic Label(GER)","PDO/PGI","Demeter(DEM)","Stop-Climate-Change","Vegan","QS","EU-Organic-label(EU)","Deut. Tierschutzbund","Initiative Tierwohl","Bioland(BIOL)","MSC-sustainable fishery","DLG-label","Fresenius","No GMO","NUTRI-SCORE")
#likert.options(legend="TEST",legend.position = "bottom")
likertscale<-likert(items12_d)

#### Tukey2
a<-as.data.frame(ds$SI01_01)
b<-as.data.frame(ds$SI01_02)
c<-as.data.frame(ds$SI01_03)
d<-as.data.frame(ds$SI01_04)
e<-as.data.frame(ds$SI01_05)
f<-as.data.frame(ds$SI01_06)
g<-as.data.frame(ds$SI01_07)
h<-as.data.frame(ds$SI01_08)
i<-as.data.frame(ds$SI01_09)
j<-as.data.frame(ds$SI01_10)
k<-as.data.frame(ds$SI01_11)
l<-as.data.frame(ds$SI01_12)
m<-as.data.frame(ds$SI01_13)
n<-as.data.frame(ds$SI01_14)
o<-as.data.frame(ds$SI01_15)


colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
colnames(h)<-"mittelwert"
colnames(i)<-"mittelwert"
colnames(j)<-"mittelwert"
colnames(k)<-"mittelwert"
colnames(l)<-"mittelwert"
colnames(m)<-"mittelwert"
colnames(n)<-"mittelwert"
colnames(o)<-"mittelwert"


competence_mean<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
h<-as.data.frame(rep(colnames(items12_d), each = length(competence_mean[,1])/15))



competence_tukey_test<-cbind(competence_mean,h)
colnames(competence_tukey_test)<-c("mittelwert","Actor")
#danach die -1er raus
competence_tukey_test<-competence_tukey_test[competence_tukey_test$mittelwert != -1, ] 

library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Actor, data=competence_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Actor",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
liste<-outUn$groups[2]

# Using a manual scale instead of hue
skala4 <- plot(likertscale, text.size=1.95) +theme(axis.text = element_text(size=8))
sizeannotate<-3.5
myFont3 <- "Palatino"
skala5<-skala4 + labs(y = "%") + scale_fill_manual(values=c("#FF0000", "#cf6e6e", "#D7D5D3" , "#93cf6e","#2d8b39"), name="Answers:",
                                                   breaks = c("very low trust","low trust","partly/partly","high trust","very high trust")) +
  theme_classic(base_family="Palatino")+
  theme( legend.text = element_text(size=8),legend.title=element_text(size=8), 
         axis.text.y = element_text(color = c("blue","black","blue","black","black","black","blue","black","black","blue","black","black","black","black","black")),
         legend.key.size = unit(.15, 'cm'), legend.position=c(-.40,-.16), legend.justification = 'left',legend.box.just = 'left',legend.direction = 'horizontal',   axis.title.x = element_text(size=8),
         plot.margin = unit(c(.1,0.1,.7,-.35), "cm" ),legend.spacing.x = unit(0.04, 'cm'))  +
  annotate("text", x=15, y=-85, label=liste[1,1], size = sizeannotate,color = 'blue', family = myFont3) + 
  annotate("text", x=14, y=-85, label=liste[2,1], size = sizeannotate,color = 'blue', family = myFont3) + 
  annotate("text", x=13, y=-85, label=liste[4,1], size = sizeannotate,color = 'black', family = myFont3) + 
  annotate("text", x=12, y=-85, label=liste[3,1], size = sizeannotate,color = 'blue', family = myFont3) + 
  annotate("text", x=11, y=-85, label = liste[5,1], size = sizeannotate,color = 'black', family = myFont3)+
  annotate("text", x=10, y=-85, label= liste[6,1], size = sizeannotate,color = 'black', family = myFont3) + 
  annotate("text", x=9, y=-85, label= liste[7,1], size = sizeannotate,color = 'blue', family = myFont3) + 
  annotate("text", x=8, y=-85, label = liste[8,1], size = sizeannotate,color = 'black', family = myFont3)+
  annotate("text", x=7, y=-85, label= liste[11,1], size = sizeannotate,color = 'black', family = myFont3) + 
  annotate("text", x=6, y=-85, label= liste[10,1], size = sizeannotate,color = 'black', family = myFont3) + 
  annotate("text", x=5, y=-85, label = liste[9,1], size = sizeannotate,color = 'black', family = myFont3)+
  annotate("text", x=4, y=-85, label= liste[12,1], size = sizeannotate,color = 'black', family = myFont3) + 
  annotate("text", x=3, y=-85, label= liste[13,1], size = sizeannotate,color = 'black', family = myFont3) + 
  annotate("text", x=2, y=-85, label = liste[14,1], size = sizeannotate,color = 'black', family = myFont3)+
  annotate("text", x=1, y=-85, label= liste[15,1], size = sizeannotate,color = 'black', family = myFont3)

setwd("/Users/adrianoprofeta/Nextcloud/DIL CONSUMER SCIENCE/6_SURVEY_DATA/2020/6_Vertrauensstudie/5_FIGURES/") 
demeter <- image_read("250px-Demeter_Logo.svg.png")
deu_biosiegel<- image_read("deu_biosiegel.png")
vegan<- image_read("vegan.png")
bioland<- image_read("bioland.png")
ohne_gmo<- image_read("ohne_gmo.png")
msc<- image_read("msc.png")
eu_biosiegel<- image_read("eu_biosiegel.png")
fresenius<- image_read("fresenius.png")
dlg<- image_read("dlg.png")
itw<- image_read("itw.svg")
tierschutzbund<- image_read("tierschutzbund.png")
nutriscore<- image_read("nutriscore.png")
qs<- image_read("qs.png")
gga<- image_read("gga.png")
climate<- image_read("climate.png")

image_demeter <- image_fill(demeter, 'none')
image_deu_biosiegel<- image_fill(deu_biosiegel, 'none')
image_vegan<- image_fill(vegan, 'none')
image_bioland <- image_fill(bioland, 'none')
image_demeter <- image_fill(demeter, 'none')
image_ohne_gmo <- image_fill(ohne_gmo, 'none')
image_msc <- image_fill(msc, 'none')
image_eu_biosiegel <- image_fill(eu_biosiegel, 'none')
image_fresenius <- image_fill(fresenius, 'none')
image_dlg <- image_fill(dlg, 'none')
image_itw <- image_fill(itw, 'none')
image_tierschutzbund <- image_fill(tierschutzbund, 'none')
image_nutriscore <- image_fill(nutriscore, 'none')
image_qs <- image_fill(qs, 'none')
image_gga <- image_fill(gga, 'none')
image_climate <- image_fill(climate, 'none')

raster_demeter <- as.raster(image_demeter)
raster_deu_biosiegel <- as.raster(image_deu_biosiegel)
raster_vegan<- as.raster(image_vegan)
raster_bioland <- as.raster(image_bioland)
raster_ohne_gmo <- as.raster(image_ohne_gmo)
raster_msc <- as.raster(image_msc)
raster_eu_biosiegel <- as.raster(image_eu_biosiegel)
raster_fresenius <- as.raster(image_fresenius)
raster_dlg <- as.raster(image_dlg)
raster_itw <- as.raster(image_itw)
raster_tierschutzbund <- as.raster(image_tierschutzbund)
raster_nutriscore <- as.raster(image_nutriscore)
raster_qs <- as.raster(image_qs)
raster_gga <- as.raster(image_gga)
raster_climate <- as.raster(image_climate)

skala6<-skala5 +  annotation_raster(raster_deu_biosiegel,14.3, 15.9,105, 70, interpolate = TRUE) +
  annotation_raster(raster_vegan,13.4, 14.6, 98, 78, interpolate = TRUE)+ 
  annotation_raster(raster_bioland,12.6, 13.4, 90, 80, interpolate = TRUE)+
  annotation_raster(raster_demeter,11.6, 12.4, 95, 75, interpolate = TRUE)+
  annotation_raster(raster_ohne_gmo,10.4, 11.6, 90, 70, interpolate = TRUE)+
  annotation_raster(raster_msc,9.4, 10.6, 90, 70, interpolate = TRUE)+
  annotation_raster(raster_eu_biosiegel,8.6, 9.4, 92, 70, interpolate = TRUE)+
  annotation_raster(raster_fresenius,7.6, 8.4, 90, 70, interpolate = TRUE)+
  annotation_raster(raster_dlg,6.6, 7.4, 90, 70, interpolate = TRUE)+
  annotation_raster(raster_itw,5.6, 6.4, 95, 75, interpolate = TRUE)+
  annotation_raster(raster_tierschutzbund,4.6, 5.4, 95, 65, interpolate = TRUE)+
  annotation_raster(raster_nutriscore,3.6, 4.4, 95, 65, interpolate = TRUE)+
  annotation_raster(raster_qs,2.6, 3.4, 90, 70, interpolate = TRUE)+
  annotation_raster(raster_gga,1.6, 2.4, 90, 70, interpolate = TRUE)+
  annotation_raster(raster_climate,0.6, 1.4, 90, 70, interpolate = TRUE)
skala6

## Labels
options(repr.plot.width=1, repr.plot.height=4)
label_1<-100-round(table(ds$SI01_01)[1]/sum(table(ds$SI01_01))*100,2)
label_2<-100-round(table(ds$SI01_02)[1]/sum(table(ds$SI01_01))*100,2)
label_3<-100-round(table(ds$SI01_03)[1]/sum(table(ds$SI01_01))*100,2)
label_4<-100-round(table(ds$SI01_04)[1]/sum(table(ds$SI01_01))*100,2)
label_5<-100-round(table(ds$SI01_05)[1]/sum(table(ds$SI01_01))*100,2)
label_6<-100-round(table(ds$SI01_06)[1]/sum(table(ds$SI01_01))*100,2)
label_7<-100-round(table(ds$SI01_07)[1]/sum(table(ds$SI01_01))*100,2)
label_8<-100-round(table(ds$SI01_08)[1]/sum(table(ds$SI01_01))*100,2)
label_9<-100-round(table(ds$SI01_09)[1]/sum(table(ds$SI01_01))*100,2)
label_10<-100-round(table(ds$SI01_10)[1]/sum(table(ds$SI01_01))*100,2)
label_11<-100-round(table(ds$SI01_11)[1]/sum(table(ds$SI01_01))*100,2)
label_12<-100-round(table(ds$SI01_12)[1]/sum(table(ds$SI01_01))*100,2)
label_13<-100-round(table(ds$SI01_13)[1]/sum(table(ds$SI01_01))*100,2)
label_14<-100-round(table(ds$SI01_14)[1]/sum(table(ds$SI01_01))*100,2)
label_15<-100-round(table(ds$SI01_15)[1]/sum(table(ds$SI01_01))*100,2)
label_knowledge<-as.data.frame(rbind(label_1,label_2,label_3,label_4,label_5,label_6,label_7,label_8,label_9,label_10,label_11,label_12,label_13,label_14,label_15))
colnames(label_knowledge)<-c("y")
names_labels<-as.data.frame(c("German Organic Label","EU-protected design. (PDO/PGI)","Demeter","Stop-Climate-Change","Vegan","QS","EU Organic Label","Label Deutscher Tierschutzbund","Initiative Tierwohl","Bioland","MSC-sustainable fishery","DLG-label","Fresenius","without genetic engineering","NUTRI-SCORE"))
colnames(names_labels)<-c("labels")
label_knowledge_final<-cbind(label_knowledge,names_labels)

label_knowledge_final$labels <- factor(label_knowledge_final$labels, levels = rev(label_knowledge_final[c(1,5,10,3,14,11,7,13,12,9,8,15,6,2,4),2]))


p<-ggplot(data=label_knowledge_final, aes(labels,y)) +
  geom_bar(stat="identity", fill="darkgreen",width = .8)+
  geom_text(aes(label=y), hjust=1.03, size=2.1,color="white")+
  theme_minimal(base_family="Palatino")+
  coord_flip(ylim=c(40,100))+
  theme(axis.text = element_text(size=8, color="black"))+
  theme(text=element_text(color="black"),axis.text=element_text(color="black"))+
  theme(axis.title.y=element_blank(), axis.title.x= element_text(color="black", size=8), axis.text.y = element_blank(),plot.tag=element_text(size=8))+ 
  ylab("%")+
  #ggtitle("supported awareness")+ 
  theme(plot.title = element_text(size=8), plot.margin = unit(c(.075,.15,-.5,.1), "cm" ))+
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 5, b = 38, l = 0)))+
  #annotate("text", x = 1, y = 40, label = "Arbitrary text") +
  #coord_cartesian( clip = "off")
  labs(tag = "supported \n awareness") +
  theme(plot.tag.position = c(0.5, 0.11))

test<-grid.arrange(skala6,p, ncol=2, widths = c(5,1))
setwd("/Users/adrianoprofeta/Nextcloud/DIL CONSUMER SCIENCE/6_SURVEY_DATA/2020/6_Vertrauensstudie/5_FIGURES/") 
ggsave(test, filename = "Figure_3.png", 
       width = 13.7, height = 9, unit="cm", dpi = 500, type = "cairo")

################
# End Figure 3 #
################

################################################################################################
# Figure 4. Spearman correlation analysis trust in selected organic labels and trust in actors #
################################################################################################

datacor<-cbind(ds[,c(185:188,190:192)], ds[,c(239,241,245,248)])
datacort <- datacor[rowSums(datacor==(-1)) == 0, ] 
test<-cor(datacor, method = c("spearman"))
colnames(datacor) <-c("conventional farmers(1)","conventional retailers(5)","conventional food producers(3)","German authorities(7)",
                      "organic farmers(2)","organic food producers(4)","organic retailers(6)",names_labels[,1][c(1,3,7,10)])
rownames(test) <-c("conventional farmers(1)","conventional retailers(5)","conventional food producers(3)","German authorities(7)",
                   "organic farmers(2)","organic food producers(4)","organic retailers(6)",names_labels[,1][c(1,3,7,10)])
colnames(test) <-c("conventional farmers(1)","conventional retailers(5)","conventional food producers(3)","German authorities(7)",
                   "organic farmers(2)","organic food producers(4)","organic retailers(6)",names_labels[,1][c(1,3,7,10)])

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(datacor, method = c("spearman"))

reso <- 300
length <- 50*reso/72
setwd("/Users/adrianoprofeta/Nextcloud/DIL CONSUMER SCIENCE/6_SURVEY_DATA/2020/6_Vertrauensstudie/5_FIGURES/") 
png("Figure_4.png", units="mm",res=reso,height=length-60,width=length)
par(mar=c(0,0,0,0),family = 'Palatino',cex.axis=14.3,cex.lab=4.4)
corrplot(test, method="color",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE,type="upper"
)
dev.off()
################
# End Figure 4 #
################
######################################################################################################################################################
# Start Figure 5. Trustworthiness of different actors in the food chain in the dimensions competence, consideration of public interest, and openness.#
######################################################################################################################################################
# competence                                                
competence_eco_farmer_alpha<-alpha(ds[c("TA41_09","TA41_10","TA41_11")], check.keys=TRUE)   #eco farmers
ds$competence_eco_farmer_scale<-scale(competence_eco_farmer_alpha$scores)

competence_farmer_alpha<-alpha(ds[c("TA01_01","TA01_02","TA01_03")], check.keys=TRUE)   #  farmers
ds$competence_farmer_scale<-scale(competence_farmer_alpha$scores)

competence_eco_retail_alpha<-alpha(ds[c("TA44_01","TA44_02","TA44_03")], check.keys=TRUE)   # eco retail
ds$competence_eco_retail_scale<-scale(competence_eco_retail_alpha$scores)

competence_retail_alpha<-alpha(ds[c("TA20_01","TA20_02","TA20_03")], check.keys=TRUE)   # retail
ds$competence_retail_scale<-scale(competence_retail_alpha$scores)

competence_eco_producer_alpha<-alpha(ds[c("TA43_01","TA43_02","TA43_03")], check.keys=TRUE)   # eco producer
ds$competence_eco_producer_scale<-scale(competence_eco_producer_alpha$scores)

competence_producer_alpha<-alpha(ds[c("TA18_01","TA18_02","TA18_03")], check.keys=TRUE)   # producer
ds$competence_producer_scale<-scale(competence_producer_alpha$scores)

competence_state_alpha<-alpha(ds[c("TA19_01","TA19_02","TA19_03")], check.keys=TRUE)   # state
ds$competence_state_scale<-scale(competence_state_alpha$scores)

# care                                                
care_eco_farmer_alpha<-alpha(ds[c("TA41_12","TA41_13")], check.keys=TRUE)   #eco farmers
ds$care_eco_farmer_scale<-scale(care_eco_farmer_alpha$scores)

care_farmer_alpha<-alpha(ds[c("TA01_04","TA01_05")], check.keys=TRUE)   #  farmers
ds$care_farmer_scale<-scale(care_farmer_alpha$scores)

care_eco_retail_alpha<-alpha(ds[c("TA44_04","TA44_05")], check.keys=TRUE)   # eco retail
ds$care_eco_retail_scale<-scale(care_eco_retail_alpha$scores)

care_retail_alpha<-alpha(ds[c("TA20_04","TA20_05")], check.keys=TRUE)   # retail
ds$care_retail_scale<-scale(care_retail_alpha$scores)

care_eco_producer_alpha<-alpha(ds[c("TA43_04","TA43_05")], check.keys=TRUE)   # eco producer
ds$care_eco_producer_scale<-scale(care_eco_producer_alpha$scores)

care_producer_alpha<-alpha(ds[c("TA18_04","TA18_05")], check.keys=TRUE)   # producer
ds$care_producer_scale<-scale(care_producer_alpha$scores)

care_state_alpha<-alpha(ds[c("TA19_04","TA19_05")], check.keys=TRUE)   # state
ds$care_state_scale<-scale(care_state_alpha$scores)


# openess                                                
openess_eco_farmer_alpha<-alpha(ds[c("TA41_14","TA41_15","TA41_16")], check.keys=TRUE)   #eco farmers
ds$openess_eco_farmer_scale<-scale(openess_eco_farmer_alpha$scores)

openess_farmer_alpha<-alpha(ds[c("TA01_06","TA01_07","TA01_08")], check.keys=TRUE)   #  farmers
ds$openess_farmer_scale<-scale(openess_farmer_alpha$scores)

openess_eco_retail_alpha<-alpha(ds[c("TA44_06","TA44_07","TA44_08")], check.keys=TRUE)   # eco retail
ds$openess_eco_retail_scale<-scale(openess_eco_retail_alpha$scores)

openess_retail_alpha<-alpha(ds[c("TA20_06","TA20_07","TA20_08")], check.keys=TRUE)   # retail
ds$openess_retail_scale<-scale(openess_retail_alpha$scores)

openess_eco_producer_alpha<-alpha(ds[c("TA43_06","TA43_07","TA43_08")], check.keys=TRUE)   # eco producer
ds$openess_eco_producer_scale<-scale(openess_eco_producer_alpha$scores)

openess_producer_alpha<-alpha(ds[c("TA18_06","TA18_07","TA18_08")], check.keys=TRUE)   # producer
ds$openess_producer_scale<-scale(openess_producer_alpha$scores)

openess_state_alpha<-alpha(ds[c("TA19_06","TA19_07","TA19_08")], check.keys=TRUE)   # state
ds$openess_state_scale<-scale(openess_state_alpha$scores)

trustworthiness<-cbind(
  rbind(t(competence_eco_farmer_alpha$total[,c(2,7,8)]),t(care_eco_farmer_alpha$total[,c(2,7,8)]),t(openess_eco_farmer_alpha$total[,c(2,7,8)])),
  rbind(t(competence_farmer_alpha$total[,c(2,7,8)]),t(care_farmer_alpha$total[,c(2,7,8)]),t(openess_farmer_alpha$total[,c(2,7,8)])),
  rbind(t(competence_eco_producer_alpha$total[,c(2,7,8)]),t(care_eco_producer_alpha$total[,c(2,7,8)]),t(openess_eco_producer_alpha$total[,c(2,7,8)])),
  rbind(t(competence_producer_alpha$total[,c(2,7,8)]),t(care_producer_alpha$total[,c(2,7,8)]),t(openess_producer_alpha$total[,c(2,7,8)])),
  rbind(t(competence_eco_retail_alpha$total[,c(2,7,8)]),t(care_eco_retail_alpha$total[,c(2,7,8)]),t(openess_eco_retail_alpha$total[,c(2,7,8)])),
  rbind(t(competence_retail_alpha$total[,c(2,7,8)]),t(care_retail_alpha$total[,c(2,7,8)]),t(openess_retail_alpha$total[,c(2,7,8)])),
  rbind(t(competence_state_alpha$total[,c(2,7,8)]),t(care_state_alpha$total[,c(2,7,8)]),t(openess_state_alpha$total[,c(2,7,8)]))
)
trustworthiness<-as.data.frame(round(trustworthiness,3))
colnames(trustworthiness)<-c("organic farmer","con. farmer","organic food prod.","con. food prod.","org. retail","con. retail","Ger. Auth.")

# Tukey-Tests
#### Tukey competence
a<-as.data.frame(rowSums(ds[c("TA41_09","TA41_10","TA41_11")])/3)
b<-as.data.frame(rowSums(ds[c("TA01_01","TA01_02","TA01_03")])/3)
c<-as.data.frame(rowSums(ds[c("TA44_01","TA44_02","TA44_03")])/3)
d<-as.data.frame(rowSums(ds[c("TA20_01","TA20_02","TA20_03")])/3)
e<-as.data.frame(rowSums(ds[c("TA43_01","TA43_02","TA43_03")])/3)
f<-as.data.frame(rowSums(ds[c("TA18_01","TA18_02","TA18_03")])/3)
g<-as.data.frame(rowSums(ds[c("TA19_01","TA19_02","TA19_03")])/3)
colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
competence_mean<-rbind(a,b,c,d,e,f,g)
h<-as.data.frame(rep(c("Öko-Landwirt","konventioneller Landwirt","Öko-LEH", "konventioneller LEH","Öko-Lebensmittelproduzent","konventioneller Lebensmittelproduzent","Staat"), each = length(competence_mean[,1])/7))
colnames(h)<-"Akteur"
competence_tukey_test<-cbind(competence_mean,h)
library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Akteur, data=competence_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Akteur",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "Akteur",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
groupvalues_1<-outUn$groups[,2]
# Tukey care
a<-as.data.frame(rowSums(ds[c("TA41_12","TA41_13")])/2)
b<-as.data.frame(rowSums(ds[c("TA01_04","TA01_05")])/2)
c<-as.data.frame(rowSums(ds[c("TA44_04","TA44_05")])/2)
d<-as.data.frame(rowSums(ds[c("TA20_04","TA20_05")])/2)
e<-as.data.frame(rowSums(ds[c("TA43_04","TA43_05")])/2)
f<-as.data.frame(rowSums(ds[c("TA18_04","TA18_05")])/2)
g<-as.data.frame(rowSums(ds[c("TA19_04","TA19_05")])/2)
colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
competence_mean<-rbind(a,b,c,d,e,f,g)
h<-as.data.frame(rep(c("Öko-Landwirt","konventioneller Landwirt","Öko-LEH", "konventioneller LEH","Öko-Lebensmittelproduzent","konventioneller Lebensmittelproduzent","Staat"), each = length(competence_mean[,1])/7))
colnames(h)<-"Akteur"
competence_tukey_test<-cbind(competence_mean,h)
library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Akteur, data=competence_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Akteur",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "Akteur",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
groupvalues_2<-outUn$groups[,2]
#Tukey openess
a<-as.data.frame(rowSums(ds[c("TA41_14","TA41_15","TA41_16")])/3)
b<-as.data.frame(rowSums(ds[c("TA01_06","TA01_07","TA01_08")])/3)
c<-as.data.frame(rowSums(ds[c("TA44_06","TA44_07","TA44_08")])/3)
d<-as.data.frame(rowSums(ds[c("TA20_06","TA20_07","TA20_08")])/3)
e<-as.data.frame(rowSums(ds[c("TA43_06","TA43_07","TA43_08")])/3)
f<-as.data.frame(rowSums(ds[c("TA18_06","TA18_07","TA18_08")])/3)
g<-as.data.frame(rowSums(ds[c("TA19_06","TA19_07","TA19_08")])/3)
colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
competence_mean<-rbind(a,b,c,d,e,f,g)
h<-as.data.frame(rep(c("Öko-Landwirt","konventioneller Landwirt","Öko-LEH", "konventioneller LEH","Öko-Lebensmittelproduzent","konventioneller Lebensmittelproduzent","Staat"), each = length(competence_mean[,1])/7))
colnames(h)<-"Akteur"
competence_tukey_test<-cbind(competence_mean,h)
library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Akteur, data=competence_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Akteur",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "Akteur",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
groupvalues_3<-outUn$groups[,2]
####### competence
graph_competence<-as.data.frame(cbind(t(trustworthiness[2:3,]),colnames(trustworthiness)  ))   
colnames(graph_competence)<-c("Kompetenz","sds","Akteur")
graph_competence$Kompetenz<-as.numeric(graph_competence$Kompetenz)
graph_competence$sds<-as.numeric(graph_competence$sds)


fig_competence<-ggplot(graph_competence, aes(x=reorder(Akteur, Kompetenz),y=Kompetenz, fill=reorder(as.factor(Akteur),Kompetenz))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Kompetenz-sds, ymax=Kompetenz+sds), width=.2, position=position_dodge(.9))+
  geom_label(aes(label = round(Kompetenz,2)),
             position = position_dodge(.9),
             label.size = .1,
             color=c("green","blue","green","black","green","black","red"),
             label.padding = unit(0,"lines"),
             vjust = -.2,size=15,parse=TRUE, show.legend = FALSE,family = myFont3)+
  scale_y_continuous(breaks=c(1,2,3,4,5),labels = c("1=totally disagree", 2, 3, 4,"5=totally agree"))+
  coord_cartesian(ylim=c(1, 5))+
  labs( x="Lebensmittel", y = "Evaluation")+
  theme_classic(base_family="Palatino") +
  theme(  axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20, face="bold"),
          legend.title = element_blank(),
          axis.text.x = element_text(color = "black", size = 25),
          axis.text.y = element_text(color = "black", size = 20),
          legend.text = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")+ 
  ggtitle("Competence") + 
  theme(plot.title = element_text(size=30))  + 
  geom_text(
    aes(label = c("a","b","ab","d","ab","c","e"), y = Kompetenz+sds+ 0.05,family = myFont3),
    color=c("green","blue","green","black","green","black","red"),
    position = position_dodge(0.9),
    vjust = 0,size =15)+
  theme(plot.title = element_text(hjust = 0.5, margin=margin(t=40,b=-30)))

# 

##     



graph_competence<-as.data.frame(cbind(t(trustworthiness[5:6,]),colnames(trustworthiness)  ))   
colnames(graph_competence)<-c("Kompetenz","sds","Akteur")
graph_competence$Kompetenz<-as.numeric(graph_competence$Kompetenz)
graph_competence$sds<-as.numeric(graph_competence$sds)

fig_public<-ggplot(graph_competence, aes(x=reorder(Akteur, Kompetenz),y=Kompetenz, fill=reorder(as.factor(Akteur),Kompetenz))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Kompetenz-sds, ymax=Kompetenz+sds), width=.2, position=position_dodge(.9))+
  geom_label(aes(label = round(Kompetenz,2)),
             position = position_dodge(.9),
             label.size = .1,
             color=c("green","black","green","red","green","black","red"),
             label.padding = unit(0,"lines"),
             vjust = -.2,size=15,parse=TRUE, show.legend = FALSE,family = myFont3)+
  scale_y_continuous(breaks=c(1,2,3,4,5),labels = c("1=totally disagree", 2, 3, 4,"5=totally agree"))+
  coord_cartesian(ylim=c(1, 5))+
  labs( x="Lebensmittel", y = "Evaluation")+
  theme_classic(base_family="Palatino") +
  theme(  axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20, face="bold"),
          legend.title = element_blank(),
          axis.text.x = element_text(color = "black", size = 25),
          axis.text.y = element_text(color = "black", size = 20),
          legend.text = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")+ 
  ggtitle("Consideration of Public Interest") + 
  theme(plot.title = element_text(size=30))  + 
  geom_text(
    aes(label = c("a","b","a","c","a","b","c"), y = Kompetenz+sds+ 0.05),
    color=c("green","black","green","red","green","black","red"),
    position = position_dodge(0.9),
    vjust = 0,size =15,family = myFont3)+
  theme(plot.title = element_text(hjust = 0.5, margin=margin(t=40,b=-30)))
#
#openness
graph_competence<-as.data.frame(cbind(t(trustworthiness[8:9,]),colnames(trustworthiness)  ))   
colnames(graph_competence)<-c("Kompetenz","sds","Akteur")
graph_competence$Kompetenz<-as.numeric(graph_competence$Kompetenz)
graph_competence$sds<-as.numeric(graph_competence$sds)


fig_public2<-ggplot(graph_competence, aes(x=reorder(Akteur, Kompetenz),y=Kompetenz, fill=reorder(as.factor(Akteur),Kompetenz))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Kompetenz-sds, ymax=Kompetenz+sds), width=.2, position=position_dodge(.9))+
  geom_label(aes(label = round(Kompetenz,2)),
             position = position_dodge(.9),
             label.size = .1,
             color=c("green","black","green","red","green","black","black"),
             label.padding = unit(0,"lines"),
             vjust = -.2,size=15,parse=TRUE, show.legend = FALSE,family = myFont3)+
  scale_y_continuous(breaks=c(1,2,3,4,5),labels = c("1=totally disagree", 2, 3, 4,"5=totally agree"))+
  coord_cartesian(ylim=c(1, 5))+
  labs( x="Lebensmittel", y = "Evaluation")+
  theme_classic(base_family="Palatino") +
  theme(  axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20, face="bold"),
          legend.title = element_blank(),
          axis.text.x = element_text(color = "black", size = 25),
          axis.text.y = element_text(color = "black", size = 20),
          legend.text = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")+ 
  ggtitle("Openness") + 
  theme(plot.title = element_text(size=30))  + 
  geom_text(
    aes(label = c("a","c","b","d","ab","c","c"), y = Kompetenz+sds+ 0.05),
    color=c("green","black","green","red","green","black","black"),
    position = position_dodge(0.9),
    vjust = 0,size =15,family = myFont3)+
  theme(plot.title = element_text(hjust = 0.5, margin=margin(t=40,b=-30)))

png(file="Figure_5.png",width=1500, height=1550, family="Times New Roman")
gridExtra::grid.arrange(fig_competence,fig_public,fig_public2, nrow = 3)
dev.off()#
################
# End Figure 5 #
################

#################################################################################################################
# Start Figure 6: Consumers' confidence in the food integrity dimensions of safety, health, sustainability, ... #
#################################################################################################################

#### Tukey1
a<-as.data.frame(ds$TA48_02)
b<-as.data.frame(ds$TA07_02)
c<-as.data.frame(ds$TA08_02)
d<-as.data.frame(ds$TA09_02)
e<-as.data.frame(ds$TA49_02)
f<-as.data.frame(ds$TA12_02)
g<-as.data.frame(ds$TA13_02)
h<-as.data.frame(ds$TA14_02)
colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
colnames(h)<-"mittelwert"
safe_mean<-rbind(a,b,c,d,e,f,g,h)
h<-as.data.frame(rep(c("konventionell / regional","konventionell / deutsch", "konventionell / EU",
                       "konventionell / Nicht-EU","ökologisch / regional","ökologisch / deutsch","ökologisch / EU",
                       "ökologisch / Nicht-EU"), each = length(safe_mean[,1])/8))
colnames(h)<-"Herkunft_Betriebsform"
safe_tukey_test<-cbind(safe_mean,h)
library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Herkunft_Betriebsform, data=safe_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
groupvalues_1<-outUn$groups[,2]
#Tukey2 
a<-as.data.frame(ds$TA48_07)
b<-as.data.frame(ds$TA07_07)
c<-as.data.frame(ds$TA08_07)
d<-as.data.frame(ds$TA09_07)
e<-as.data.frame(ds$TA49_07)
f<-as.data.frame(ds$TA12_07)
g<-as.data.frame(ds$TA13_07)
h<-as.data.frame(ds$TA14_07)
colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
colnames(h)<-"mittelwert"
safe_mean<-rbind(a,b,c,d,e,f,g,h)
h<-as.data.frame(rep(c("konventionell / regional","konventionell / deutsch", "konventionell / EU",
                       "konventionell / Nicht-EU","ökologisch / regional","ökologisch / deutsch","ökologisch / EU",
                       "ökologisch / Nicht-EU"), each = length(safe_mean[,1])/8))
colnames(h)<-"Herkunft_Betriebsform"
safe_tukey_test<-cbind(safe_mean,h)
library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Herkunft_Betriebsform, data=safe_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
groupvalues_2<-outUn$groups[,2]

#Tukey3   
a<-as.data.frame(ds$TA48_08)
b<-as.data.frame(ds$TA07_08)
c<-as.data.frame(ds$TA08_08)
d<-as.data.frame(ds$TA09_08)
e<-as.data.frame(ds$TA49_08)
f<-as.data.frame(ds$TA12_08)
g<-as.data.frame(ds$TA13_08)
h<-as.data.frame(ds$TA14_08)
colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
colnames(h)<-"mittelwert"
safe_mean<-rbind(a,b,c,d,e,f,g,h)
h<-as.data.frame(rep(c("konventionell / regional","konventionell / deutsch", "konventionell / EU",
                       "konventionell / Nicht-EU","ökologisch / regional","ökologisch / deutsch","ökologisch / EU",
                       "ökologisch / Nicht-EU"), each = length(safe_mean[,1])/8))
colnames(h)<-"Herkunft_Betriebsform"
safe_tukey_test<-cbind(safe_mean,h)
library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Herkunft_Betriebsform, data=safe_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
groupvalues_3<-outUn$groups[,2]

#Tukey4  
a<-as.data.frame(ds$TA48_12)
b<-as.data.frame(ds$TA07_12)
c<-as.data.frame(ds$TA08_12)
d<-as.data.frame(ds$TA09_12)
e<-as.data.frame(ds$TA49_12)
f<-as.data.frame(ds$TA12_12)
g<-as.data.frame(ds$TA13_12)
h<-as.data.frame(ds$TA14_12)
colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
colnames(h)<-"mittelwert"
safe_mean<-rbind(a,b,c,d,e,f,g,h)
h<-as.data.frame(rep(c("konventionell / regional","konventionell / deutsch", "konventionell / EU",
                       "konventionell / Nicht-EU","ökologisch / regional","ökologisch / deutsch","ökologisch / EU",
                       "ökologisch / Nicht-EU"), each = length(safe_mean[,1])/8))
colnames(h)<-"Herkunft_Betriebsform"
safe_tukey_test<-cbind(safe_mean,h)
library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Herkunft_Betriebsform, data=safe_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
groupvalues_4<-outUn$groups[,2]

#Tukey5 
a<-as.data.frame(ds$TA48_13)
b<-as.data.frame(ds$TA07_13)
c<-as.data.frame(ds$TA08_13)
d<-as.data.frame(ds$TA09_13)
e<-as.data.frame(ds$TA49_13)
f<-as.data.frame(ds$TA12_13)
g<-as.data.frame(ds$TA13_13)
h<-as.data.frame(ds$TA14_13)
colnames(a)<-"mittelwert"
colnames(b)<-"mittelwert"
colnames(c)<-"mittelwert"
colnames(d)<-"mittelwert"
colnames(e)<-"mittelwert"
colnames(f)<-"mittelwert"
colnames(g)<-"mittelwert"
colnames(h)<-"mittelwert"
safe_mean<-rbind(a,b,c,d,e,f,g,h)
h<-as.data.frame(rep(c("konventionell / regional","konventionell / deutsch", "konventionell / EU",
                       "konventionell / Nicht-EU","ökologisch / regional","ökologisch / deutsch","ökologisch / EU",
                       "ökologisch / Nicht-EU"), each = length(safe_mean[,1])/8))
colnames(h)<-"Herkunft_Betriebsform"
safe_tukey_test<-cbind(safe_mean,h)
library(agricolae)
modelUnbalanced <- aov(mittelwert ~ Herkunft_Betriebsform, data=safe_tukey_test)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "Herkunft_Betriebsform",group=TRUE, unbalanced = TRUE)
print(outUn$groups)
groupvalues_5<-outUn$groups[,2]

##############################################konventionell nach Ländern
### sind sicher kon
data_safe_kon<-as.data.frame(cbind(ds$TA48_02,ds$TA07_02,ds$TA08_02,ds$TA09_02))
data_safe_kon<-pivot_longer(data_safe_kon, V1:V4)

data_safe_kon_final<- data_summary(data_safe_kon, varname="value", 
                                   groupnames=c("name"))

data_safe_kon_final$name<-revalue(data_safe_kon_final$name, c("V1"="conventional / local"))
data_safe_kon_final$name<-revalue(data_safe_kon_final$name, c("V2"="conventional / German"))
data_safe_kon_final$name<-revalue(data_safe_kon_final$name, c("V3"="conventional / EU"))
data_safe_kon_final$name<-revalue(data_safe_kon_final$name, c("V4"="conventional / Non-EU"))
colnames(data_safe_kon_final)<-c("Lebensmittel","Mittelwert","Standardabw.")
###
### sind gesund kon
data_health_kon<-as.data.frame(cbind(ds$TA48_07,ds$TA07_07,ds$TA08_07,ds$TA09_07))
data_health_kon<-pivot_longer(data_health_kon, V1:V4)

data_health_final<- data_summary(data_health_kon, varname="value", 
                                 groupnames=c("name"))

data_health_final$name<-revalue(data_health_final$name, c("V1"="conventional / local"))
data_health_final$name<-revalue(data_health_final$name, c("V2"="conventional / German"))
data_health_final$name<-revalue(data_health_final$name, c("V3"="conventional / EU"))
data_health_final$name<-revalue(data_health_final$name, c("V4"="conventional / Non-EU"))
colnames(data_health_final)<-c("Lebensmittel","Mittelwert","Standardabw.")
###
### sind nachhaltig kon
data_sus_kon<-as.data.frame(cbind(ds$TA48_08,ds$TA07_08,ds$TA08_08,ds$TA09_08))
data_sus_kon<-pivot_longer(data_sus_kon, V1:V4)

data_sus_final<- data_summary(data_sus_kon, varname="value", 
                              groupnames=c("name"))

data_sus_final$name<-revalue(data_sus_final$name, c("V1"="conventional / local"))
data_sus_final$name<-revalue(data_sus_final$name, c("V2"="conventional / German"))
data_sus_final$name<-revalue(data_sus_final$name, c("V3"="conventional / EU"))
data_sus_final$name<-revalue(data_sus_final$name, c("V4"="conventional / Non-EU"))
colnames(data_sus_final)<-c("Lebensmittel","Mittelwert","Standardabw.")
###
### sind authentisch kon
data_auth_kon<-as.data.frame(cbind(ds$TA48_12,ds$TA07_12,ds$TA08_12,ds$TA09_12))
data_auth_kon<-pivot_longer(data_auth_kon, V1:V4)

data_auth_final<- data_summary(data_auth_kon, varname="value", 
                               groupnames=c("name"))

data_auth_final$name<-revalue(data_auth_final$name, c("V1"="conventional / local"))
data_auth_final$name<-revalue(data_auth_final$name, c("V2"="conventional / German"))
data_auth_final$name<-revalue(data_auth_final$name, c("V3"="conventional / EU"))
data_auth_final$name<-revalue(data_auth_final$name, c("V4"="conventional / Non-EU"))
colnames(data_auth_final)<-c("Lebensmittel","Mittelwert","Standardabw.")
###
### schmecken kon
data_taste_kon<-as.data.frame(cbind(ds$TA48_13,ds$TA07_13,ds$TA08_13,ds$TA09_13))
data_taste_kon<-pivot_longer(data_taste_kon, V1:V4)

data_taste_final<- data_summary(data_taste_kon, varname="value", 
                                groupnames=c("name"))

data_taste_final$name<-revalue(data_taste_final$name, c("V1"="conventional / local"))
data_taste_final$name<-revalue(data_taste_final$name, c("V2"="conventional / German"))
data_taste_final$name<-revalue(data_taste_final$name, c("V3"="conventional / EU"))
data_taste_final$name<-revalue(data_taste_final$name, c("V4"="conventional / Non-EU"))
colnames(data_taste_final)<-c("Lebensmittel","Mittelwert","Standardabw.")
###

##############################################öko nach Ländern
### sind sicher öko
data_safe_eco<-as.data.frame(cbind(ds$TA49_02,ds$TA12_02,ds$TA13_02,ds$TA14_02))
data_safe_eco<-pivot_longer(data_safe_eco, V1:V4)

data_safe_final_eco<- data_summary(data_safe_eco, varname="value", 
                                   groupnames=c("name"))

data_safe_final_eco$name<-revalue(data_safe_final_eco$name, c("V1"="organic / local"))
data_safe_final_eco$name<-revalue(data_safe_final_eco$name, c("V2"="organic / German"))
data_safe_final_eco$name<-revalue(data_safe_final_eco$name, c("V3"="organic / EU"))
data_safe_final_eco$name<-revalue(data_safe_final_eco$name, c("V4"="organic / Non-EU"))
colnames(data_safe_final_eco)<-c("Lebensmittel","Mittelwert","Standardabw.")
###
### sind gesund eco
data_health_eco<-as.data.frame(cbind(ds$TA49_07,ds$TA12_07,ds$TA13_07,ds$TA14_07))
data_health_eco<-pivot_longer(data_health_eco, V1:V4)

data_health_final_eco<- data_summary(data_health_eco, varname="value", 
                                     groupnames=c("name"))

data_health_final_eco$name<-revalue(data_health_final_eco$name, c("V1"="organic / local"))
data_health_final_eco$name<-revalue(data_health_final_eco$name, c("V2"="organic / German"))
data_health_final_eco$name<-revalue(data_health_final_eco$name, c("V3"="organic / EU"))
data_health_final_eco$name<-revalue(data_health_final_eco$name, c("V4"="organic / Non-EU"))
colnames(data_health_final_eco)<-c("Lebensmittel","Mittelwert","Standardabw.")
###
### sind nachhaltig eco
data_sus_eco<-as.data.frame(cbind(ds$TA49_08,ds$TA12_08,ds$TA13_08,ds$TA14_08))
data_sus_eco<-pivot_longer(data_sus_eco, V1:V4)

data_sus_final_eco<- data_summary(data_sus_eco, varname="value", 
                                  groupnames=c("name"))

data_sus_final_eco$name<-revalue(data_sus_final_eco$name, c("V1"="organic / local"))
data_sus_final_eco$name<-revalue(data_sus_final_eco$name, c("V2"="organic / German"))
data_sus_final_eco$name<-revalue(data_sus_final_eco$name, c("V3"="organic / EU"))
data_sus_final_eco$name<-revalue(data_sus_final_eco$name, c("V4"="organic / Non-EU"))
colnames(data_sus_final_eco)<-c("Lebensmittel","Mittelwert","Standardabw.")
###
### sind authentisch eco
data_auth_eco<-as.data.frame(cbind(ds$TA49_12,ds$TA12_12,ds$TA13_12,ds$TA14_12))
data_auth_eco<-pivot_longer(data_auth_eco, V1:V4)

data_auth_final_eco<- data_summary(data_auth_eco, varname="value", 
                                   groupnames=c("name"))

data_auth_final_eco$name<-revalue(data_auth_final_eco$name, c("V1"="organic / local"))
data_auth_final_eco$name<-revalue(data_auth_final_eco$name, c("V2"="organic / German"))
data_auth_final_eco$name<-revalue(data_auth_final_eco$name, c("V3"="organic / EU"))
data_auth_final_eco$name<-revalue(data_auth_final_eco$name, c("V4"="organic / Non-EU"))
colnames(data_auth_final_eco)<-c("Lebensmittel","Mittelwert","Standardabw.")
###
### schmecken eco
data_taste_eco<-as.data.frame(cbind(ds$TA49_13,ds$TA12_13,ds$TA13_13,ds$TA14_13))
data_taste_eco<-pivot_longer(data_taste_eco, V1:V4)

data_taste_final_eco<- data_summary(data_taste_eco, varname="value", 
                                    groupnames=c("name"))

data_taste_final_eco$name<-revalue(data_taste_final_eco$name, c("V1"="organic / local"))
data_taste_final_eco$name<-revalue(data_taste_final_eco$name, c("V2"="organic / German"))
data_taste_final_eco$name<-revalue(data_taste_final_eco$name, c("V3"="organic / EU"))
data_taste_final_eco$name<-revalue(data_taste_final_eco$name, c("V4"="organic / Non-EU"))
colnames(data_taste_final_eco)<-c("Lebensmittel","Mittelwert","Standardabw.")
###

safety<-as.data.frame(rbind(data_safe_kon_final,data_safe_final_eco))
ordered_safety<-as.data.frame(safety[order(safety$Mittelwert, decreasing = TRUE),c(1:3),])

healthy<-as.data.frame(rbind(data_health_final,data_health_final_eco))
ordered_healthy<-healthy[order(healthy$Mittelwert, decreasing = TRUE),c(1:3),]

sustainable<-as.data.frame(rbind(data_sus_final,data_sus_final_eco))
ordered_sustainable<-sustainable[order(sustainable$Mittelwert, decreasing = TRUE),c(1:3),]

authentical<-as.data.frame(rbind(data_auth_final,data_auth_final_eco))
ordered_authentical<-authentical[order(authentical$Mittelwert, decreasing = TRUE),c(1:3),]

taste<-as.data.frame(rbind(data_taste_final,data_taste_final_eco))
ordered_taste<-taste[order(taste$Mittelwert, decreasing = TRUE),c(1:3),]



## figure 1: 
f1<-ggplot(ordered_safety, aes(x=reorder(Lebensmittel, Mittelwert),y=Mittelwert, fill=reorder(as.factor(Lebensmittel),Mittelwert))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mittelwert-Standardabw., ymax=Mittelwert+Standardabw.), width=.2,
                position=position_dodge(.9))+
  geom_label(aes(label = round(Mittelwert,2)),
             position = position_dodge(.9),
             label.size = .2,
             color=c("white","white","white","black","black","black","black","black"),
             label.padding = unit(.2, "lines"),
             vjust = -.2,size=5,parse=TRUE, show.legend = FALSE)+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8))+
  coord_cartesian(ylim=c(1, 5))+
  labs( x="Lebensmittel", y = "Evaluation")+
  theme_classic(base_family="Palatino") +
  theme(  axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20, face="bold"),
          legend.title = element_blank(),
          axis.text.x = element_text(color = "black", size = 15,angle = 80, hjust = 1),
          axis.text.y = element_text(color = "black", size = 20),
          legend.text = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")+ 
  ggtitle("... are safe")+
  geom_text(
    aes(label = groupvalues_1, y = Mittelwert+Standardabw.+ 0.05),
    color=c("darkgreen","green","black","darkblue","blue","red1","red1","red4"),
    position = position_dodge(0.9),
    vjust = 0,size =9, family = myFont3)

f2<-ggplot(ordered_healthy, aes(x=reorder(Lebensmittel, Mittelwert),y=Mittelwert, fill=reorder(as.factor(Lebensmittel),Mittelwert))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mittelwert-Standardabw., ymax=Mittelwert+Standardabw.), width=.2,
                position=position_dodge(.9))+
  geom_label(aes(label = round(Mittelwert,2)),
             position = position_dodge(.9),
             label.size = .2,
             color=c("white","white","white","black","black","black","black","black"),
             label.padding = unit(.2, "lines"),
             vjust = -.2,size=5,parse=TRUE, show.legend = FALSE)+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8))+
  coord_cartesian(ylim=c(1, 5))+
  labs( x="Lebensmittel", y = "Evaluation")+
  theme_classic(base_family="Palatino") +
  theme(  axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20, face="bold"),
          legend.title = element_blank(),
          axis.text.x = element_text(color = "black", size = 15,angle = 80, hjust = 1),
          axis.text.y = element_text(color = "black", size = 20),
          legend.text = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")+ 
  ggtitle("... are healthy")+
  geom_text(
    aes(label = groupvalues_2, y = Mittelwert+Standardabw.+ 0.05),
    color=c("darkgreen","darkgreen","green","black","black","red1","red1","red4"),
    position = position_dodge(0.9),
    vjust = 0,size =9, family = myFont3)

f3<-ggplot(ordered_sustainable, aes(x=reorder(Lebensmittel, Mittelwert),y=Mittelwert, fill=reorder(as.factor(Lebensmittel),Mittelwert))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mittelwert-Standardabw., ymax=Mittelwert+Standardabw.), width=.2,
                position=position_dodge(.9))+
  geom_label(aes(label = round(Mittelwert,2)),
             position = position_dodge(.9),
             label.size = .2,
             color=c("white","white","white","black","black","black","black","black"),
             label.padding = unit(.2, "lines"),
             vjust = -.2,size=5,parse=TRUE, show.legend = FALSE)+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8))+
  coord_cartesian(ylim=c(1, 5))+
  labs( x="Lebensmittel", y = "Evaluation")+
  theme_classic(base_family="Palatino") +
  theme(  axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20, face="bold"),
          legend.title = element_blank(),
          axis.text.x = element_text(color = "black", size = 15,angle = 80, hjust = 1),
          axis.text.y = element_text(color = "black", size = 20),
          legend.text = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")+ 
  ggtitle("... are sustainable")+
  geom_text(
    aes(label = groupvalues_3, y = Mittelwert+Standardabw.+ 0.05),
    color=c("darkgreen","green","black","darkblue","darkblue","red1","red1","red4"),
    position = position_dodge(0.9),
    vjust = 0,size =9, family = myFont3)

f4<-ggplot(ordered_authentical, aes(x=reorder(Lebensmittel, Mittelwert),y=Mittelwert, fill=reorder(as.factor(Lebensmittel),Mittelwert))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mittelwert-Standardabw., ymax=Mittelwert+Standardabw.), width=.2,
                position=position_dodge(.9))+
  geom_label(aes(label = round(Mittelwert,2)),
             position = position_dodge(.9),
             label.size = .2,
             color=c("white","white","white","black","black","black","black","black"),
             label.padding = unit(.2, "lines"),
             vjust = -.2,size=5,parse=TRUE, show.legend = FALSE)+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8))+
  coord_cartesian(ylim=c(1, 5))+
  labs( x="Lebensmittel", y = "Evaluation")+
  theme_classic(base_family="Palatino") +
  theme(  axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20, face="bold"),
          legend.title = element_blank(),
          axis.text.x = element_text(color = "black", size = 15,angle = 80, hjust = 1),
          axis.text.y = element_text(color = "black", size = 20),
          legend.text = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")+ 
  ggtitle("... are authentic")+
  geom_text(
    aes(label = groupvalues_4, y = Mittelwert+Standardabw.+ 0.05),
    color=c("darkgreen","green","green","black","darkblue","red1","red1","red4"),
    position = position_dodge(0.9),
    vjust = 0,size =9, family = myFont3)

f5<-ggplot(ordered_taste, aes(x=reorder(Lebensmittel, Mittelwert),y=Mittelwert, fill=reorder(as.factor(Lebensmittel),Mittelwert))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mittelwert-Standardabw., ymax=Mittelwert+Standardabw.), width=.2,
                position=position_dodge(.9))+
  geom_label(aes(label = round(Mittelwert,2)),
             position = position_dodge(.9),
             label.size = .1,
             color=c("white","white","white","black","black","black","black","black"),
             label.padding = unit(.2, "lines"),
             vjust = -.2,size=5,parse=TRUE, show.legend = FALSE)+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8))+
  coord_cartesian(ylim=c(1, 5))+
  labs( x="Lebensmittel", y = "Evaluation")+
  theme_classic(base_family="Palatino") +
  theme(  axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20, face="bold"),
          legend.title = element_blank(),
          axis.text.x = element_text(color = "black", size = 15,angle = 80, hjust = 1),
          axis.text.y = element_text(color = "black", size = 20),
          legend.text = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")+ 
  ggtitle("... taste good")+
  geom_text(
    aes(label = groupvalues_5, y = Mittelwert+Standardabw.+ 0.05),
    color=c("darkgreen","green","black","darkblue","blue","red1","red1","red4"),
    position = position_dodge(0.9),
    vjust = 0,size =9, family = myFont3)
setwd("/Users/adrianoprofeta/Nextcloud/DIL CONSUMER SCIENCE/6_SURVEY_DATA/2020/6_Vertrauensstudie/5_FIGURES/") 
png(file="Figure_6.png",width=1000, height=1550, family="Times New Roman")
f1
f2
f3
f4
f5
gridExtra::grid.arrange(f1,f2,f3,f4,f5, nrow = 5)
dev.off()




################
# End Figure 6 #
################

#################################################################################################################
# Start Figure 7 & 8: SEM #
#################################################################################################################


############################
# Data preparation for SEM #
############################
## The dataset is replicated 6 times. This is once for actor
ds1<-ds
ds2<-ds
ds3<-ds
ds4<-ds
ds5<-ds
ds6<-ds
ds_total<-rbind(ds1,ds2,ds3,ds4,ds5,ds6)
ds_total$groups<-c(rep("conventional farmer",896),
                   rep("conventinal food producer",896),
                   rep("conventional retailer",896),
                   rep("organic farmer",896),
                   rep("organic food producer",896),
                   rep("organic retailer",896))
# Creating variable general trust(gen_trust)
ds_total$gen_trust<-c(ds_total$TA02_01[1:896],ds_total$TA02_03[1:896],ds_total$TA02_02[1:896],ds_total$TA02_06[1:896],ds_total$TA02_07[1:896],ds_total$TA02_08[1:896])

#competence
ds_total$C1<-c(ds_total$TA01_01[1:896],ds_total$TA18_01[1:896],ds_total$TA20_01[1:896],ds_total$TA41_09[1:896],ds_total$TA43_01[1:896],ds_total$TA44_01[1:896])
ds_total$C2<-c(ds_total$TA01_02[1:896],ds_total$TA18_02[1:896],ds_total$TA20_02[1:896],ds_total$TA41_10[1:896],ds_total$TA43_02[1:896],ds_total$TA44_02[1:896])
ds_total$C3<-c(ds_total$TA01_03[1:896],ds_total$TA18_03[1:896],ds_total$TA20_03[1:896],ds_total$TA41_11[1:896],ds_total$TA43_03[1:896],ds_total$TA44_03[1:896])

#Public interest
ds_total$P1<-c(ds_total$TA01_04[1:896],ds_total$TA18_04[1:896],ds_total$TA20_04[1:896],ds_total$TA41_12[1:896],ds_total$TA43_04[1:896],ds_total$TA44_04[1:896])
ds_total$P2<-c(ds_total$TA01_05[1:896],ds_total$TA18_05[1:896],ds_total$TA20_05[1:896],ds_total$TA41_13[1:896],ds_total$TA43_05[1:896],ds_total$TA44_05[1:896])

#openess  
ds_total$O1<-c(ds_total$TA01_06[1:896],ds_total$TA18_06[1:896],ds_total$TA20_06[1:896],ds_total$TA41_14[1:896],ds_total$TA43_06[1:896],ds_total$TA44_06[1:896])
ds_total$O2<-c(ds_total$TA01_07[1:896],ds_total$TA18_07[1:896],ds_total$TA20_07[1:896],ds_total$TA41_15[1:896],ds_total$TA43_07[1:896],ds_total$TA44_07[1:896])
ds_total$O3<-c(ds_total$TA01_08[1:896],ds_total$TA18_08[1:896],ds_total$TA20_08[1:896],ds_total$TA41_16[1:896],ds_total$TA43_08[1:896],ds_total$TA44_08[1:896])

#confidence in food
## safe 
ds_total$safe<-c(ds_total$TA07_02[1:2688],ds_total$TA12_02[2689:5376])
## healthy 
ds_total$healthy<-c(ds_total$TA07_07[1:2688],ds_total$TA12_07[2689:5376])
## sustainable
ds_total$sustainable<-c(ds_total$TA07_08[1:2688],ds_total$TA12_08[2689:5376])
## authentic 
ds_total$authentic<-c(ds_total$TA07_12[1:2688],ds_total$TA12_12[2689:5376])
## tasty
ds_total$tasty<-c(ds_total$TA07_13[1:2688],ds_total$TA12_13[2689:5376])


# Items WFI-Oel (organic index)
ds_total$ORG1<-c(ds_total$TA21_02)
ds_total$ORG2<-c(ds_total$TA21_07)
ds_total$ORG3<-c(ds_total$TA21_08)
ds_total$ORG4<-c(ds_total$TA21_12)
ds_total$ORG5<-c(ds_total$TA21_13)

# Items Social trust scale
ds_total$S1<-c(ds_total$TA23_02)
ds_total$S2<-c(ds_total$TA23_07)
ds_total$S3<-c(ds_total$TA23_08)
ds_total$S4<-c(ds_total$TA23_12)
ds_total$S5<-c(ds_total$TA23_13)
ds_total$S6<-c(ds_total$TA23_14)

# Organic labels
ds_total$GER<-c(ds_total$SI01_01)
ds_total$QS<-c(ds_total$SI01_06)
ds_total$EU<-c(ds_total$SI01_07)
ds_total$BIOL<-c(ds_total$SI01_10)
ds_total$DEM<-c(ds_total$SI01_03)
ds_total$NS<-c(ds_total$SI01_15)
###

# selection of dataset for conventional or organic actors -> choose only one!
#ds_total<-ds_total[1:2688,] #conventional
ds_total<-ds_total[2689:5376,] #organic

###########################
# End preparation for SEM #
###########################


model <- '
  # measurement model
    # latent variables
    competence  =~   C1 + C2 + C3  
    pub_i  =~  P1 + P2  
    openess =~  O1 + O2 + O3 
    
      confidence_in_food =~ safe + healthy + sustainable + authentic + tasty
    
    organic_index =~ ORG1+ORG2+ORG3+ORG4+ORG5
    social_trust =~ S1+S2+S3+S4+S5+S6
    beliefs_in_trustworthiness  =~ competence + pub_i + openess  
    
      # Covariance
    organic_index~~social_trust
    
    
  
    
    
    # regressions
    gen_trust ~ beliefs_in_trustworthiness
    confidence_in_food ~ gen_trust
    beliefs_in_trustworthiness ~ social_trust
    beliefs_in_trustworthiness~ organic_index
    BIOL ~ gen_trust      
    DEM ~ gen_trust
    GER ~ gen_trust 
    EU ~ gen_trust 
    
  
 '

fit_new <- sem(model, data=ds_total,std.lv=TRUE)
standardised_values<-summary(fit_new, standardized=TRUE)
standardised_values<-cbind(standardised_values$pe$lhs,standardised_values$pe$op, standardised_values$pe$rhs, round(standardised_values$pe$std.all,3), round(standardised_values$pe$pvalue,3))
library(xtable)
output<-xtable(standardised_values)
print.xtable(output, type="html", file="output_standardized.html")

# Model result graphic
# 
labels<-c("C1","C2","C3","P1","P2","O1","O2","O3","safe","healthy","sustain- \n able","authen- \ntic","tasty","ORG1","ORG2","ORG3","ORG4","ORG5","S1","S2","S3","S4","S5","S6","BIOL","DEM","GER","EU","GEN. \nTRUST","COMP.","PUB.I.","OPEN- \n NESS","CON- \n FIDENCE","WFI- \n OeL","SOC. \n TRUST","TRUST- \n WORTHI- \n NESS")
png("SEM_conventional_actors.png", width = 15, height = 10, units = 'cm', res =  300)
semPaths(fit_new,"std",layout="tree2", nodeLabels=labels,mar = c(2, 2, 2, 2),style = "ram",sizeMan = 4,
         node.width = .9,
         edge.label.cex = .6,) #1000 600
dev.off()

labels<-c("C1","C2","C3","P1","P2","O1","O2","O3","safe","healthy","sustain- \n able","authen- \ntic","tasty","ORG1","ORG2","ORG3","ORG4","ORG5","S1","S2","S3","S4","S5","S6","BIOL","DEM","GER","EU","GEN. \nTRUST","COMP.","PUB.I.","OPEN- \n NESS","CON- \n FIDENCE","WFI- \n OeL","SOC. \n TRUST","TRUST- \n WORTHI- \n NESS")
png("SEM_organic_actors.png", width = 15, height = 10, units = 'cm', res =  300)
semPaths(fit_new,"std",layout="tree2", nodeLabels=labels,mar = c(2, 2, 2, 2),style = "ram",sizeMan = 4,
         node.width = .9,
         edge.label.cex = .6,) #1000 600
dev.off()


results_conventional<-semTable(fit_new, longtable = TRUE, file ="sem_conventional_not_standardized.html",type = "html", columns=c("estsestars",  "rsquare"))
results_organic<-semTable(fit_new, longtable = TRUE, file ="sem_organic_not_standardized",type = "html")
