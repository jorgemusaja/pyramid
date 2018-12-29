
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
########  PERÚ  ################################################################
################################################################################

arch <- list.files(pattern = "per", recursive = T, full.names = F)

library(plyr)
per <- adply(arch, 1, read_xls)
detach("package:plyr", unload=TRUE)

paraPER <- data.frame(rep(c("Rural", "Perú","Urbano"), c(56,101,56)), 
                      rep(c(2, 1, 3), c(56,101,56)))

per <- cbind(paraPER, per[-1])
colnames(per)[1] <- "Ámbito"
colnames(per)[2] <- "n"
colnames(per)[3] <- "Año"
colnames(per)

per <- per %>%
  gather(key="Edad", value="Población", 4:84)

per <- mutate(per, Edad=as.numeric(Edad))

glimpse(per)

perN <- per %>%
  filter((Edad>=0 & Edad<=14) | (Edad>=65 & Edad<=100)) %>%
  group_by(Ámbito, Año, n) %>%
  summarise(numerador = sum(Población))

perD <- per %>%
  filter(Edad>=15 & Edad<=64) %>%
  group_by(Ámbito, Año, n) %>%
  summarise(denominador = sum(Población))

datosPER <-full_join(perN, perD, by=c("Año", "Ámbito", "n"))

datosPER <- datosPER %>%
  mutate(ratio = numerador/denominador*100)

rm(perN, perD)

glimpse(datosPER)

per_name <- c("Perú", "Rural", "Urbano")

for(i in 1:length(per_name)){
  secc = ""
  secc <- filter(datosPER, n==i)
  png(filename = paste0(per_name[i],".png"),
      width=1200, height=800, units="px", pointsize=5,bg="white",res=400)
  plot(secc$Año, secc$ratio, col="blue", xlab="Años", ylab="Tasa de dependencia", 
       pch=20, cex=1.2, lwd=1, type="o", ylim=c(0,120),
       main=paste0("Tasa de dependencia ",per_name[i]," (",min(secc$Año),"-",max(secc$Año),")"))
  dev.off()  
}


################################################################################
########  PIRÁMIDE  ############################################################
################################################################################

perAMB = filter(per, Ámbito=="Urbano" | Ámbito=="Rural")
perAMB <- perAMB %>%
  mutate(Población = ifelse(Ámbito=="Urbano", 
                             round(-1*Población/1000,0), round(Población/1000,0)))
perAMB$Edad <- as.numeric(perAMB$Edad)
perAMB$Edad <- factor(perAMB$Edad)

años = unique(perAMB$Año)

library(plyr)
for(i in 1970:2025) {
  piramide = filter(perAMB, Año==i)
  
  lmin = min(piramide$Población)
  rmax = max(piramide$Población)
  val = max(abs(lmin),rmax)
  val = as.numeric(substr(val,1,1))
  
  p <- ggplot(piramide, aes(x=Edad, y=Población, fill=Ámbito)) + 
    geom_bar(subset = .(Ámbito=="Urbano"), stat = "identity") + 
    geom_bar(subset = .(Ámbito=="Rural"), stat = "identity") + 
    scale_y_continuous(breaks = seq(-val*100, val*100, 100), 
                       labels = paste0(as.character(c(val:0, 1:val)), "00m")) + 
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") +
    theme_light() +
    ggtitle(paste0("Pirámide Poblacional: Año ", i, " (en miles)"))
  
   png(filename = paste0("piramide_",i,".png"),
      width=2400, height=3600, units="px", pointsize=2,bg="white",res=400)
   plot(p)
   dev.off()
}
detach("package:plyr", unload=TRUE)




################################################################################
########  REGIONES  ############################################################
################################################################################

Listado <- read_excel("Listado.xls")

reg_name = Listado$Región

archivos <- list.files(pattern = "reg", recursive = T, full.names = F)

library(plyr)
datos <- adply(archivos, 1, read_xls)
detach("package:plyr", unload=TRUE)

regiones <- data.frame(sort(rep(Listado$CODREG, 81)))

datos <- cbind(regiones, datos[-1])

colnames(datos)[1] <- "CODREG"
colnames(datos)

datos <- datos %>%
  gather(key="Año", value="Población", 3:33)

datos <- left_join(datos, Listado, by="CODREG")
datos <- mutate(datos, n=as.numeric(UBI))

glimpse(datos)

datosN <- datos %>%
  filter((Edad>=0 & Edad<=14) | (Edad>=65 & Edad<=100)) %>%
  group_by(Región, Año, n) %>%
  summarise(numerador = sum(Población))

datosD <- datos %>%
  filter(Edad>=15 & Edad<=64) %>%
  group_by(Región, Año, n) %>%
  summarise(denominador = sum(Población))

datosREG <-full_join(datosN, datosD, by=c("Año", "Región", "n"))

datosREG <- datosREG %>%
  mutate(ratio = numerador/denominador*100)

rm(datos, datosD, datosN)

glimpse(datosREG)


for(i in 1:length(reg_name)){
  secc = ""
  secc <- filter(datosREG, n==i)
  png(filename = paste0(reg_name[i],".png"),
      width=1200, height=800, units="px", pointsize=5,bg="white",res=400)
  plot(secc$Año, secc$ratio, col="blue", xlab="Años", ylab="Tasa de dependencia", 
       pch=20, cex=1.2, lwd=1, type="o", ylim=c(0,120),
       main=paste0("Tasa de dependencia ",reg_name[i]," (",min(secc$Año),"-",max(secc$Año),")"))
       dev.off()  
}

write.csv(datosREG, "datosREG.csv", fileEncoding = "latin1")

