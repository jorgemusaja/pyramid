
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

WriteXLS::WriteXLS(per, "piramide.xlsx")


################################################################################
########  PIRÁMIDE  ############################################################
################################################################################

per <- per %>%
  mutate(Población = ifelse(Sexo=="Hombre", 
                             round(-1*Población/1000,0), round(Población/1000,0)))
per$Edad <- factor(per$Edad)
colnames(per) <- c("Año","Edad","Población","Sexo")

glimpse(per)
años = unique(per$Año)

################################################################################

library(plyr)
for(i in 1950:2050) {
  piramide = filter(per, Año==i)
  
  lmin = min(piramide$Población)
  rmax = max(piramide$Población)
  val = max(abs(lmin),rmax)
  val = as.numeric(substr(val,1,1))
  
  p <- ggplot(piramide, aes(x=Edad, y=Población, fill=Sexo)) + 
    geom_bar(subset = .(Sexo=="Hombre"), stat = "identity") + 
    geom_bar(subset = .(Sexo=="Mujer"), stat = "identity") + 
    scale_y_continuous(breaks = seq(-val*100, val*100, 100), 
                       labels = paste0(as.character(c(val:0, 1:val)), "00m")) + 
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") +
    theme_light() +
    ggtitle(paste0("Pirámide Poblacional: Año ", i, " (en miles)"))
  
   png(filename = paste0("piramide_",i,".png"),
      width=3200, height=3600, units="px", pointsize=2,bg="white",res=400)
   plot(p)
   dev.off()
}
detach("package:plyr", unload=TRUE)
