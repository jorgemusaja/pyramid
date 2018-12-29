
rm(list=ls())

options(encoding="UTF-8")

setwd("C:/Users/jcutipa/Documents/R_Studio")

################################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)
library(gridExtra)
library(pyramid)

################################################################################

perH = read_excel("perH.xls")
perM = read_excel("perM.xls")

perH <- perH %>%
  gather(key="Edad", value="Población", 2:82)
perM <- perM %>%
  gather(key="Edad", value="Población", 2:82)

perH <- perH %>%
  mutate(Sexo = "Hombre")
perM <- perM %>%
  mutate(Sexo = "Mujer")

per <- bind_rows(perH,perM)
per <- mutate(per, Edad=as.numeric(Edad))
glimpse(per)

rm(perH, perM)

per <- per %>%
  mutate(RangoEdad=ifelse(Edad>=0&Edad<=4, "00-04",
                          ifelse(Edad>=5&Edad<=9, "05-09",
                                 ifelse(Edad>=10&Edad<=14, "10-14",
                                        ifelse(Edad>=15&Edad<=19, "15-19",
                                               ifelse(Edad>=20&Edad<=24, "20-24",
                                                      ifelse(Edad>=25&Edad<=29, "25-29",
                                                             ifelse(Edad>=30&Edad<=34, "30-34",
                                                                    ifelse(Edad>=35&Edad<=39, "35-39",
                                                                           ifelse(Edad>=40&Edad<=44, "40-44",
                                                                                  ifelse(Edad>=45&Edad<=49, "45-49",
                                                                                         ifelse(Edad>=50&Edad<=54, "50-54",
                                                                                                ifelse(Edad>=55&Edad<=59, "55-59",
                                                                                                       ifelse(Edad>=60&Edad<=64, "60-64",
                                                                                                              ifelse(Edad>=65&Edad<=69, "65-69",
                                                                                                                     ifelse(Edad>=70&Edad<=74, "70-74",
                                                                                                                            ifelse(Edad>=75&Edad<=79, "75-79",
                                                                                                                                   ifelse(Edad==80, "80-más", NA
                                                                                                                                          ))))))))))))))))))

per <- arrange(per, AÑOS,Sexo,Edad,RangoEdad)

per <- per %>%
  group_by(AÑOS,Sexo,RangoEdad) %>%
  summarise(Población=sum(Población))

per <- per %>%
  group_by(AÑOS,Sexo) %>%
  mutate(Total=sum(Población))

per <- per %>%
  mutate(Población_porcentaje=round(Población/Total*100,1))

glimpse(per)

################################################################################
########  PIRÁMIDE  ############################################################
################################################################################

per <- per %>%
  mutate(Población_porcentaje = ifelse(Sexo=="Hombre", 
                            -1*Población_porcentaje, 1*Población_porcentaje))
                            
per$RangoEdad <- factor(per$RangoEdad)
colnames(per) <- c("Año","Sexo","RangoEdad","Población","Total","Población_porcentaje")

#WriteXLS::WriteXLS(per, "piramide_rangoedad.xlsx")

glimpse(per)
años = unique(per$Año)

################################################################################

library(plyr)
for(i in 1950:2050) {
  piramide <- subset(per, Año==i)
  
  p <- ggplot(piramide, aes(x=RangoEdad, y=Población_porcentaje, fill=Sexo)) + 
    geom_bar(subset = .(Sexo=="Hombre"), stat = "identity") + 
    geom_bar(subset = .(Sexo=="Mujer"), stat = "identity") + 
    scale_y_continuous(breaks = seq(-20, 20, 2), 
                       labels = paste0(as.character(c(abs(seq(-20, 20, 2)))), "%")) + 
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") +
    theme_light() +
    ggtitle(paste0("Pirámide Poblacional: Año ", i, " (en porcentaje)"))
  
   png(filename = paste0("piramide_rangoedad_",i,".png"),
      width=3200, height=3600, units="px", pointsize=2,bg="white",res=400)
   plot(p)
   dev.off()
}
detach("package:plyr", unload=TRUE)


####################################################################################
####################################################################################

