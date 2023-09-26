#install.packages("tidyverse")
library(readr)
library(psych)
library(readxl)
library("RcppDynProg")
library(dplyr)
library(ggpubr)
library("pROC")
library(ggplot2)
library(reshape2)
library(tidyverse)
require(maps)
require(viridis)
#theme_set(theme_void())

setwd("D:/thermal/thermal_R/silver_pompano")

silver_Data<- read_csv("silver_pompano.csv") #Suresh_Kumar_Data to silver_Data
silver_density<- read_csv("silver_pompano_kd.csv") #Suresh_Kumar_Data to silver_Data

str(silver_Data)
#df<-Suresh_Kumar_Data[,-c(3:5)]
#df$Fish_status<-as.factor(df$Fish_status)
#df$TankID<-as.factor(df$TankID)
#df<-select(df,TankID,Accliminate_Temp_oC,CT_max,CT_min,Length_mm,Weight_g,Fish_status)
silver_Data$TankID<-as.factor(silver_Data$TankID)
describe(silver_Data)
summary(silver_Data)
na.omit(silver_density)

#####Normality test##############
qq1<-ggqqplot(silver_Data$CT_max, main = "Normal Quantile Plot for CT_max", color='lightcoral')
qq2<-ggqqplot(silver_Data$CT_min, main = "Normal Quantile Plot for CT_min", color='steelblue1')
shapiro.test(silver_Data$CT_max) #Not nromally distributed

#jpeg("qqplot.jpg", res = 600,height = 5,width = 9,units = "in")
ggarrange(qq2, qq1, 
          labels = c("a", "b"),
          ncol = 2, nrow = 1)
dev.off()

###########density plot#########
#jpeg("densityplot.jpg", res = 600,height = 5,width = 9,units = "in")
par(mfrow=c(1,2))
#define kernel density
kd_ctmin <- density(silver_Data$CT_min)
kd_ctmax <- density(silver_Data$CT_max)

#create kernel density plot
plot(kd_ctmin,xlim=c(8,22), ylim=c(0,0.2), xlab="CTmin",main = "Density Plot for CTmin")
polygon(kd_ctmin,xlim=c(8,21), ylim=c(0,0.2),col='steelblue1', border='black',xlab="CTmin")

plot(kd_ctmax,xlim=c(32,44), ylim=c(0,0.2),xlab="CTmax",main = "Density Plot for CTmax")
polygon(kd_ctmax,xlim=c(32,44), ylim=c(0,0.2),col='lightcoral', border='black',xlab="CTmax")
dev.off()


########summarize data against acc temp########
#Ctmax
group_by(silver_Data, Acc_Temp ) %>%
  summarise(
    count = n(),
    mean = mean(CT_max, na.rm = TRUE),
    sd = sd(CT_max, na.rm = TRUE)
  )
#CTmin
group_by(silver_Data, Acc_Temp) %>%
  summarise(
    count = n(),
    mean = mean(CT_min, na.rm = TRUE),
    sd = sd(CT_min, na.rm = TRUE)
  )
#Length
group_by(silver_Data, Acc_Temp) %>%
  summarise(
    count = n(),
    mean = mean(Length_mm1 , na.rm = TRUE),
    sd = sd(Length_mm1 , na.rm = TRUE)
  )
#Weight
head(silver_Data)
group_by(silver_Data, Acc_Temp) %>%
  summarise(
    count = n(),
    mean = mean(Weight_g1, na.rm = TRUE),
    sd = sd(Weight_g1, na.rm = TRUE)
  )

#######Box plot for the Length and weight against acc temp###########
#Length
#jpeg("gg_length1.jpg", res = 600,height = 5,width = 7,units = "in")
gg_length<-ggboxplot(silver_Data, x = "Acc_Temp", y = "Length_mm1", 
          outlier.shape = NA,alpha = 0.5,bxp.errorbar = TRUE,bxp.errorbar.width = 0.4,
          color = "Acc_Temp", 
          fill = "Acc_Temp",
          #palette = c("#00AFBB", "#E7B800","brown","green","black","red"),
          order = c("18", "22","26","30","34","36"), 
          ylab = "Length (cm)", xlab = "Accliminate temperature (°C)",
          )
gg_length+theme_bw()
dev.off()


#Weight
#jpeg("gg_weight1.jpg", res = 600,height = 5,width = 7,units = "in")
gg_weight<-ggboxplot(silver_Data, x = "Acc_Temp", y = "Weight_g1", 
                     outlier.shape = NA,alpha = 0.5,
                     bxp.errorbar = TRUE,bxp.errorbar.width = 0.4,
                     color = "Acc_Temp", 
                     fill = "Acc_Temp",
                     #palette = c("#00AFBB", "#E7B800","brown","green","black","red"),
                     order = c("18", "22","26","30","34","36"), 
                     ylab = "Weight (g)", xlab = "Accliminate temperature (°C)",
)
gg_weight+theme_bw()
dev.off()

############Kruskal-Wallis test#######
head(silver_Data)
#CTmax
shapiro.test(silver_Data$CT_max) #Not nromally distributed
kruskal.test(CT_max ~ Acc_Temp, data = silver_Data)
pairwise.wilcox.test(silver_Data$CT_max, silver_Data$Acc_Temp,
                     p.adjust.method = "bonf")
#CTmin
shapiro.test(silver_Data$CT_min) #Not nromally distributed
kruskal.test(CT_min ~ Acc_Temp, data = silver_Data)
pairwise.wilcox.test(silver_Data$CT_min, silver_Data$Acc_Temp,
                     p.adjust.method = "bonf")
?shapiro.test
plot(silver_Data$Acc_Temp, silver_Data$CT_max,
     xlab = expression(Accliminate~Temperature~(degree~C)), ylab = "CTmax",
     main = "raw data\ncircles: observed values, dashed line: unobserved true values")
lines(silver_Data$Acc_Temp, silver_Data$CT_max,
      type = "l",
      lty = "dashed")

#######Thermal Polygon########

max(silver_Data$CT_max)
min(silver_Data$CT_max)

max(silver_Data$CT_min)
min(silver_Data$CT_min)
head(silver_Data)

polygon_area<-(Total<-(((max(silver_Data$CT_max)-max(silver_Data$CT_min)) + 
            (min(silver_Data$CT_max)-min(silver_Data$CT_min)))/2) *
    (min(silver_Data$CT_max)-max(silver_Data$CT_min)))


#Melt into long form
df.m<- melt(silver_Data, id.vars = "Acc_Temp", measure.vars = c("CT_max", "CT_min"))

#plot
#jpeg("thermal polygon.jpg", res = 600,height = 5,width = 7,units = "in")
ggplot(df.m, aes(Acc_Temp, value, group = variable, colour = variable, shape = variable)) +
  geom_point() + 
  geom_line(aes(y = c(rep(18.99,179),41.14)), color = "darkred", linetype="twodash") + 
  geom_point() + geom_line(aes(y = c(rep(34.84,179),18.99)), color = "darkred", linetype="dotted") +
  geom_line(aes(y = c(11.94,rep(34.84,179))), color = "darkred", linetype="dotdash") +
  annotate(geom="text", x=25, y=25, label="Total=357.02", color="black") +
  annotate(geom="text", x=18, y=37, label="34.84", color="blue") +
  annotate(geom="text", x=36, y=42.1, label="41.14", color="blue") +
  annotate(geom="text", x=18, y=11, label="11.94", color="blue") +
  annotate(geom="text", x=36, y=18, label="18.99", color="blue") +
  annotate(geom="text", x=27, y=42, label="R-square=0.99, p<0.001", color="blue") +
  ggtitle("Thermal Polygon") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="Thermal Limit", x = "Accliminate Temperature") +
  geom_smooth(method = "lm") +  theme_classic()
dev.off()

p<-ggplot(df.m, aes(Acc_Temp, value, group = variable,colour = variable))+
  geom_point()+
  geom_smooth(method = "lm") 
p+scale_color_manual(values=c("red", "green"))
p

#####corr##############
cor_data<- silver_Data %>% select(c(Acc_Temp,CT_max, CT_min))
cor_data<-na.omit(cor_data)
boxplot(cor_data, outline = F)
#install.packages("Hmisc")
library(Hmisc)
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
library(ggcorrplot)

corr1<-rcorr(as.matrix (cor_data))
corr1
a<-flattenCorrMatrix(corr1$r,corr1$P)
#write.csv(summer.correlation$P, 'summer.correlation_p.csv')

corr2 <- round(cor(corr))
ggcorrplot(corr,tl.cex = 15,lab_size = 5, lab = T, digits = 2,legend.title = "Correlation",
           type="lower",outline.col = "white")

##########GAM#############
#install.packages("mgcv")
library(mgcv)
library(cowplot)
#install.packages("visreg")
library(visreg)
fit_lm<-lm(Acc_Temp ~CT_max+CT_min, data=silver_Data)
summary(fit_lm)
plot(fit_lm)

df2<-select(df,Accliminate_Temp_oC,CT_max,CT_min,Length_mm,Weight_g )

fit<-lm(Weight_g~.,data=df2)

summary(fit)

fit2<-lm(Weight_g~Length_mm ,data=df2)

summary(fit2)
gam1<-gam(Acc_Temp ~s(CT_max)+s(CT_min), data=silver_Data)
summary(gam3)
gam4<-gam(Acc_Temp ~s(CT_min)+s(CT_max), data=silver_Data)

#jpeg("GAM_plot.jpg", res = 600,height = 5,width = 7,units = "in")
plot(gam4, shade = T, shade.col = "slategray1", pages = 1)
dev.off()
#ctmin_gg<-visreg(gam1, "CT_min", gg=TRUE)+theme_bw()
#ctmax_gg<-visreg(gam1, "CT_max", gg=TRUE)+theme_bw()
#plot_grid(ctmin_gg,ctmax_gg,
          #levels= c("(a)", "(b)"),
          #label_size = 12,
          #ncol = 2,
          #hjust = 0,
          #label_x = 0,
          #align = "hv")

gam2<-gam(Acc_Temp ~s(CT_max), data=silver_Data)
gam3<-gam(Acc_Temp ~s(CT_min), data=silver_Data)

jpeg("gam3d_1.jpg", res = 600,height = 7,width = 7,units = "in")
vis.gam(gam1,view=c("CT_min","CT_max"), color = "topo", type = 'response',plot.type = 'persp',ticktype="detailed",
        xlab = "", ylab = "", zlab = "", cex.lab=1.5, cex.axis=1.5, 
        phi=30, theta=25, n.grid = 40, border="darkgray")
dev.off()

#install.packages("ggExtra")

library(ggExtra)
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(df, aes(Accliminate_Temp_oC, CT_max)) + 
  geom_count() + 
  geom_smooth(method="lm", se=T) +  
  labs(y="CTmax", x = expression(Accliminate~Temperature~(degree~C)))

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")

citation("ggplot2")

############temp map#######
str(world_map)
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")+
  theme_bw()
thermal<-read.csv("silver_pompano_thermal.csv")
head(thermal.map)
colnames(thermal)<-c("region","summer","winter")
thermal.map <- left_join(thermal, world_map, by = "region")

ggplot(thermal.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = summer ), color = "white")+
  scale_fill_viridis_c(option = "C")+
  geom_sf(world_map,aes(x = long, y = lat))
#########
library(sf)
