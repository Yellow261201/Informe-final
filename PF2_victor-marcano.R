library( readxl)
library(tidyverse)
library(ggrepel)
library(dslabs)
library(stringr)
library(zoo)
library(lubridate)
library(rvest)
library(tidyr)
library(gridExtra)
library(ggplot2)

options(warn = -1)
#1. Limpieza de la base de datos------------------------------------------------
#1.1 Pivote de la data para su limB4pieza----------------------------------------

datos_aire <- read_excel("Air_Quality.xlsx")

variables <- c( "Ozone (O3)",  
                "Nitrogen Dioxide (NO2)",
                "O3-Attributable Cardiac and Respiratory Deaths")
                
Filtro1 <- c( "Annual Average 2009", "Annual Average 2010", "Annual Average 2011",
              "Annual Average 2012", "Annual Average 2013", "Annual Average 2014", 
              "Annual Average 2015", "Annual Average 2016", "Annual Average 2017",
              "Annual Average 2018", "Annual Average 2019", "Annual Average 2021")


datos_aire_new <- datos_aire %>% 
  
  rename( "Period" = "Time Period",
          
          "GeoType" = "Geo Type Name") %>%
  
  filter(!Period %in% Filtro1) %>%
  
  filter(GeoType != "UHF34") %>%
  
  select(-(1:2), -(4:7), -9, -12) %>%
  
  filter(Name %in% variables) %>%
  
  distinct()


  datos_aire_new$Start_Date <- ydm(datos_aire_new$Start_Date)
  
  datos_aire_new <- arrange(datos_aire_new, Start_Date) %>% 
    
    na.omit()

  calidad_aire <- datos_aire_new %>%
   
   pivot_wider( names_from = "Name",
               
               values_from = "Data Value") %>%
    
    rename( "dioxido_nitrogeno" = "Nitrogen Dioxide (NO2)",
            
            "ozono" = "Ozone (O3)",
            
            "fecha" = "Start_Date",
            
            "geo_lugar" ="Geo Place Name",
            
            "problemas_cardiacos" ="O3-Attributable Cardiac and Respiratory Deaths")  %>%
    
    arrange(desc(fecha) ) %>%
    
    transform(dioxido_nitrogeno = as.numeric(dioxido_nitrogeno),
        
                ozono = as.numeric(ozono),
              
                problemas_cardiacos = as.numeric(problemas_cardiacos))
#1.2 OrganizaciC3n individual de las variables para concretar la base de datos---

#1.2.1 organizacion de la lista de variables a trabajar-------------------------
 
 
 lista_variables <- calidad_aire %>%
   
   replace_na(list(dioxido_nitrogeno = 0)) %>%
    
   replace_na(list(ozono =0)) 
    
 
 promedio_nitrogeno <- lista_variables %>%
   
   group_by(fecha,dioxido_nitrogeno) %>%
   
   summarise(dioxido_nitrogeno = mean(dioxido_nitrogeno, na.rm=TRUE)) %>%
   
   ungroup()
 
 
 promedio_ozono <- lista_variables  %>%
   
   group_by(fecha,ozono) %>%
   
   summarise(ozono = mean.default(ozono, na.rm=TRUE)) %>%
   
   ungroup()
 

lugar<- "Brooklyn"
#1.2.2 acomodo de los datos de forma anual--------------------------------------

datos_anuales <- lista_variables %>%
  
  separate(fecha, c("year" , "moth"), sep = "-" )%>%
  
  select(-ozono) %>%
  
  pivot_wider(names_from = moth, 
              
              values_from = dioxido_nitrogeno) 

#1.2.3 acomodo del dioxido de nitrogeno como variable---------------------------

datos_anuales$dioxido_nitrogeno_media <- rowMeans(datos_anuales[, c(4,5,6)],
                                                  
                                                  na.rm = TRUE)

dioxido_nitrogeno <- datos_anuales %>%
  
  select(-problemas_cardiacos, -06, -04, -05)

#1.2.4 acomodo del ozono como variable------------------------------------------

ozono <- lista_variables %>%
  
  separate(fecha, c("year" , "moth"), sep = "-" )%>%
  
  select(-dioxido_nitrogeno) %>%
  
  pivot_wider(names_from = moth, 
              
              values_from = ozono)

ozono$ozono_media <- rowMeans(ozono[, c(4,5,6)],
                                          
                                          na.rm = TRUE)

ozono <- ozono %>%
  
  select(-problemas_cardiacos, -06, -04, -05)

#1.2.5 Union de variables a trabajar con ozono y dioxido de nitrogeno-----------

calidad_aire <-  merge(dioxido_nitrogeno, ozono, all = TRUE) 


calidad_aire<- calidad_aire %>%

  
  rename(localizacion = geo_lugar,
         
         nitrogeno_media =dioxido_nitrogeno_media)
  
calidad_aire <- transform(calidad_aire, nitrogeno_media = as.numeric(nitrogeno_media), 
                          
                          localizacion = as.character(localizacion), 
                          
                          ozono_media = as.numeric(ozono_media))
  

#1.2.6 acomodo de los problemas cardidacos como variable------------------------

enfermedad <- lista_variables %>%
  
  separate(fecha, c("year" , "moth"), sep = "-" )%>%
  
  select(-ozono, -dioxido_nitrogeno) %>%
  
  pivot_wider(names_from = moth, 
              
              values_from = problemas_cardiacos)

problemas_cardiacos <- enfermedad %>%
  select(-3, -5) %>%
  na.omit() %>%
  rename(muertes_media = 3,
         localizacion = geo_lugar)

problemas_cardiacos <-  transform(problemas_cardiacos,  localizacion = as.character(localizacion), 
                                  
                                  muertes_media = as.numeric(muertes_media))

resultado <- left_join(calidad_aire, problemas_cardiacos)

remove(datos_aire_new,
       enfermedad,
       lista_variables,
       promedio_nitrogeno,
       promedio_ozono,
       datos_anuales,
       dioxido_nitrogeno,
       ozono)


#2. GRAFICAs-------------------------------------------------------------------

#2.1 Grafico 1, Dioxido de nitrogeno a lo largo de los aC1os-------------------


new_york <-c("East New York and Starrett City (CD5)")


grafico <- calidad_aire %>% filter(localizacion == new_york)

grafico <- transform(grafico, nitrogeno_media = as.numeric(nitrogeno_media), 
                     
                     localizacion = as.character(localizacion), 
                     
                     ozono_media = as.numeric(ozono_media))
summary(grafico)

grafico %>%
  
  ggplot(aes(year,nitrogeno_media, group = 1)) + 
  
  geom_point(aes(year, nitrogeno_media), color = "deepskyblue4", size = 3) +
  
  geom_line(color = "deepskyblue2") +
  
  xlab("Anos") +
  
  ylab("nitrogeno_media") +
  
  labs(title="Linea de tiempo del Dioxido de nitrogeno", x="Anos", y = "Dioxido de nitrogeno") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.position="none",
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold")) +
  
  geom_vline(xintercept = as.factor(2019), linetype = "dashed", color = "red", size = 1) +
  
  geom_label(aes(x = as.factor(2019), y = 15, label ="inicio de pandemia"),
             size = 3, color = "black", family = "Times New Roman") 
   
#2.2 grafico 2, Ozono a lo largo de los aC1os---------------------------------- 

grafico  %>% 
  
  ggplot(aes(year , ozono_media, group = 1)) +
  
  geom_point(aes( year, ozono_media), color = "green4", size = 3) +
  
  geom_line(color = "green2") +
  
  ylim(10,20) +
  
  labs(title="Linea de tiempo del Ozono", x="Anos", y = "Ozono") +
  
  geom_vline(xintercept = as.factor(2019), linetype = "dashed", color = "red", size = 1) +
  
  geom_label(aes(x = as.factor(2019), y = 13, label ="inicio de pandemia"),
             size = 3, color = "black", family = "Times New Roman") +
  
  xlab("Anos") +
  
  ylab("Ozono")  


#2.3 Grafico 3, problemas cardiacos de ciertos distritos para cada aC1o----------

years<- c(2005, 2009, 2012, 2015)
years<- as.factor(years)



distrito<- c("Queens",
             "Bronx",
             "Brooklyn",
             "Manhattan",
             "Staten Island",
             "East New York")


resultado %>% 
  
  filter(year %in% years  & localizacion %in% distrito ) %>% 
  
  ggplot(aes(year, muertes_media, group = localizacion)) + 
  
  geom_line(aes(color= localizacion)) +
  
  theme(plot.title = element_text(size = 17, face = "bold"),
        legend.position="bottom",
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold")) +
  
  labs(title="Linea de tiempo de distritos relevantes", x ="Anos", y = "Afecciones y problemas cardiacos") +
  
  geom_point(aes(color = localizacion)) 


#2.4 comparaciC3n de loa distritos con mayor muerte promedio ------------------

resultado %>% 
  
  filter(year > 2004 & muertes_media > 5) %>%
  
  ggplot(aes(as.factor(year), muertes_media, group = localizacion)) + 
  
  geom_point(aes(color = localizacion)) +
  
  geom_line(aes(color = localizacion)) +
  
  labs(title="Comparacion de los distritos con mayor muerte promedio", x="Anos", y = "Afecciones y  problemas cardiacos") +
  
  facet_wrap('localizacion') +
  theme(legend.position = 'none') 

#2.5 comparaciC3n de los distritos con menor muerte promedio------------------
resultado %>% 
  
  filter(year > 2004 & muertes_media < 5) %>%
  
  ggplot(aes(as.factor(year), muertes_media, group = localizacion)) + 
  
  geom_point(aes(color = localizacion)) +
  
  geom_line(aes(color = localizacion)) +
  
  labs(title="Comparacion de los distritos con menor muerte promedio", x = "Anos", y = "Afecciones y  problemas cardiacos") +
  facet_wrap('localizacion') + 
  
  theme(legend.position = 'none', axis.text.x = element_text(size = 8, angle = 45))

#2.6 comparacion entre distritos por muertes promedio en diferentes plazos de tiempo---------------------
 p3 <- problemas_cardiacos %>% 
  filter(year == 2005 & muertes_media) %>%
  mutate(localizacion = reorder(localizacion, muertes_media, FUN = median)) %>%
  ggplot(aes(localizacion, muertes_media)) +
  geom_bar(stat ="identity", color= "white", fill="Green4",   na.rm = TRUE, show.legend = none) +
  coord_flip() +
  labs(y ="Problemas car(2005)", x ="Localizacion") +
  theme(legend.position = 'none', axis.text.x = element_text(size = 8)) +
  theme(axis.title.x = element_text(face="bold", vjust=1.5, colour="black", size=rel(0.80)))


p4 <- problemas_cardiacos %>% 
  filter(year == 2015 & muertes_media) %>%
  mutate(localizacion = reorder(localizacion, muertes_media, FUN = median)) %>%
  ggplot(aes(localizacion, muertes_media)) +
  geom_bar(aes(localizacion, muertes_media),
           stat ="identity",color= "white", fill="Red4", na.rm = TRUE, show.legend = none) +
    labs(y ="Problemas car(2015)", x ="Localizacion") +
  coord_flip() +
  theme(legend.position = 'none', axis.text.x = element_text(size = 8)) +
  theme(axis.title.x = element_text(face="bold", vjust=1.5, colour="black", size=rel(0.80)))

grid.arrange(p3, p4, ncol = 2)


#2.7 LC-nea de regresiC3n entre las muertes por afecciones cardiacas y el Ozono-------------


resultado %>%
  ggplot(aes(nitrogeno_media, muertes_media)) +
  geom_point(alpha = 0.6) +
  geom_smooth( formula = y~x,
               method = "lm",
               color = "red") +
  xlim(10,55) +
  labs(y ="Afecciones y problemas cardiacos", x ="Dioxido de nitrogeno")

  
#2.8 LC-nea de regresiC3n entre las muertes por afecciones cardiacas y el Ozono-----------------------------------

resultado %>%
  ggplot(aes(ozono_media, muertes_media)) +
  geom_point(alpha = 0.6) +
  geom_smooth( formula = y~x,
               method = "lm",
               color = "red") +
  xlim(5, 25) +
  labs(y ="Afecciones y problemas cardiacos", x ="Ozono")


