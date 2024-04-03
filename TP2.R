setwd("L:/BUT/SD/Promo 2023/acordier/Progra statistiques/datasets/fao.csv")
df=read.csv("fao.csv",header=TRUE,dec=",", sep=";")
View(df)
nrow(df)
summary(df)
mean(df$Dispo_alim)
sum(df$Population,na.rm=TRUE)
sd(df$Export_viande,na.rm=TRUE)
sd(df$Import_viande,na.rm=TRUE)
median(df$Prod_viande,na.rm=TRUE)
quantile(df$Dispo_alim,probs=c(0.25,0.5,0.75))
quantile(df$Import_viande,probs=seq(0,1,0.01))
Pop<-order(df$Population)
head(df[order(df$Population),],n=5)
head(df[order(df$Population,decreasing=TRUE),],n=5)
head(df[order(df$Prod_viande,decreasing=TRUE),],n=5)
head(df[order(df$Import_viande,decreasing=TRUE),],n=5)
nrow(subset(df,df$Dispo_alim>2300))
nrow(subset(df,df$Dispo_alim>3500 & df$Import_viande>1000))
subset(df,df$Nom %in% c("France","Belgique"))
df$part_export=df$Export_viande/df$Prod_viande
View(df)       
df$dispo_alim_pays=df$Dispo_alim*df$Population
write.table(df,file="ExportTp2.csv",sep=",", row.names=FALSE)
dispo_mondiale=sum(df$dispo_alim_pays,na.rm=TRUE)
nb_personne=dispo_mondiale/2300
print(nb_personne)
plot(df$Prod_viande,df$Export_viande)
cor( x = df$Prod_viande , y = df$Export_viande , use="complete.obs" )
install.packages("corrplot")
library(corrplot)
matriceCor=cor(df[,-1],use="complete.obs")
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         diag=FALSE )