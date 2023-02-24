#Librerias
{ 
library(RcmdrMisc)
library(ggdendro)
library(corrplot)
library(circlize)
library(FactoMineR)
library(ggcorrplot)
library(dplyr)
library(pracma)
library(abind)
library(tinytex)
library(ggplot2)
library(tidyverse)
library(factoextra)
library(fitdistrplus)
library(logspline)
library(broom)
library(MASS)
library(knitr)
library(kableExtra)
library(multcomp)
library(lubridate)
library(openxlsx)
library(ggpubr)
library(plotly)
library(ggdendro)
library(clustertend)
library(NbClust)
library(pvclust)
library(fpc)
library(RColorBrewer)
library(readxl)
library(hopkins)
library(cluster)
library(pbapply)
}

DatosRS <- read_excel("~/Proyectos y Convocatorias/Hackaton Rio Santiago/DatosClean.xlsx")
DatosRS$fecha <- ymd(DatosRS$fecha)
DatosRS$EST <- factor(DatosRS$EST)
DatosRS$Year <- year(DatosRS$fecha)
DatosRS$Month <- month(DatosRS$fecha)
DatosRS <- tibble::rownames_to_column(DatosRS , "ID")
DatosRS$PID <- paste(DatosRS$EST, DatosRS$ID, sep = "_")



#Analisis por punto

DatosClean <- select(DatosRS,-c(fecha, LAT, LON, Year, Month, ID, EST))
DatosCleanBASE <- select(DatosRS,-c(fecha, LAT, LON, Year, Month, ID, EST))
#DatosClean <- impute(DatosClean, mean)


row.names(DatosClean) <- DatosClean$PID

DatosClean <- scale(select(DatosClean, c(TEMPERATURA:NITROGENO_TOTAL)))
DatosClean <- as.data.frame(DatosClean)
DatosClean <- select(DatosClean,-c(MATERIA_FLOTANTE))

row.names(DatosClean) <- DatosCleanBASE$PID


q1 <- apply(DatosClean, 2, quantile, probs = 0.25)
q3 <- apply(DatosClean, 2, quantile, probs = 0.75)
iqr <- q3 - q1

lower_fence <- q1 - 10 * iqr
upper_fence <- q3 + 10 * iqr

outliers <- apply(DatosClean, 1, function(x) any(x < lower_fence | x > upper_fence))
DatosCleanB <- DatosClean[!outliers,]

#Metodos de determinacion de numero de clusters

RS_sil <- fviz_nbclust(DatosCleanB, hcut, method = "silhouette", k.max = 12)+
  labs(subtitle = "Silhouette method-hcut")
RS_sil

wss_plot <- fviz_nbclust(DatosCleanB, hcut, method = "wss", k.max = 12) +
  geom_vline(xintercept = 7, linetype = 2)+
  labs(subtitle = "Elbow method-hcut")
wss_plot

ap_plot <- fviz_nbclust(DatosCleanB, kmeans, nstart = 25, method = "gap_stat", nboot = 50, k.max = 12)+
  labs(subtitle = "Gap statistic method-kmeans")
ap_plot


set.seed(123)

RS.hc_res <- eclust(DatosCleanB, k=7, hc_metric = "euclidean", hc_method = "ward.D2", graph = FALSE)

km.res_CEA <- kmeans(DatosCleanB, 7, iter.max = 500)

fviz_cluster(list(data = DatosCleanB, cluster = km.res_CEA$cluster),
             ellipse.type = "norm", 
             geom = "point", 
             stand = FALSE,
             star.plot = TRUE,
             palette = "jco", ggtheme = theme_classic())


DatosRSC <- DatosCleanB
DatosRSC <- rownames_to_column(DatosRSC, "PID")
  
DatosRSC <- merge(DatosRSC, DatosRS[c("PID", "EST")], by = "PID")


DatosRSC$cluster <- km.res_CEA$cluster


clust_CEA <- hclust(dist(DatosCleanB, method="euclidean"), method="ward.D2")


dendo <- fviz_dend(clust_CEA, k=7, cex=1, lwd=1, rect = TRUE)
dendo



#Analisis por estacion


Agrupado <- group_by(DatosRS, DatosRS$EST)
Estaciones <- summarise_if(Agrupado, is.numeric, mean)
EstacionesB <- Estaciones
Estaciones <- select(Estaciones, -c(LAT, Year, Month))
Estaciones <- select(Estaciones, -c(`DatosRS$EST`, MATERIA_FLOTANTE))
row.names(Estaciones) <-  EstacionesB$`DatosRS$EST`

Estaciones <- scale(Estaciones)
Estaciones <- as.data.frame(Estaciones)
row.names(Estaciones) <-  EstacionesB$`DatosRS$EST`

Sil_est <- fviz_nbclust(Estaciones, hcut, method = "silhouette", k.max = 12)+
  labs(subtitle = "Silhouette method-hcut")
Sil_est

ggsave("OptCluster.png",path ="~/Proyectos y Convocatorias/Hackaton Rio Santiago", bg ='transparent', width = 20, height = 20 ,units = c("cm"), dpi=600 )

Elbow_est <- fviz_nbclust(Estaciones, hcut, method = "wss", k.max = 12) +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method-hcut")
Elbow_est

Ap_est <- fviz_nbclust(Estaciones, kmeans, nstart = 25, method = "gap_stat", nboot = 50, k.max = 12)+
  labs(subtitle = "Gap statistic method-kmeans")
Ap_est

set.seed(123)

hc_est <- eclust(Estaciones, k=6, hc_metric = "euclidean", hc_method = "ward.D2", graph = FALSE)

km_est <- kmeans(Estaciones, 6, iter.max = 500)

fviz_cluster(list(data = Estaciones, cluster = km_est$cluster),
             #ellipse.type = "norm", 
             #geom = "point", 
             stand = FALSE,
             star.plot = TRUE,
             palette = "jco", ggtheme = theme_classic())

clust_EST<- hclust(dist(Estaciones, method="euclidean"), method="ward.D2")


dendo <- fviz_dend(clust_EST, k=6, cex=1, lwd=1, rect = TRUE, k_colors = c('#0f509d','#077f75','#9aab21', '#fab513', '#f07f1b', '#a51f73'))
dendo

ggsave("Dendograma.png",path ="~/Proyectos y Convocatorias/Hackaton Rio Santiago", bg ='transparent', width = 20, height = 20 ,units = c("cm"), dpi=600 )



Paleta <- c('#0f509d','#077f75','#9aab21', '#fab513', '#f07f1b', '#a51f73')

