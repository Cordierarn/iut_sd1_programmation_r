donnees <- read.csv("L://BUT/SD/Promo 2023/acordier/Progra statistiques/datasets/bodies_karts.csv", header = TRUE, dec = ",", sep = ";")
donnees1 <- read.csv("L://BUT/SD/Promo 2023/acordier/Progra statistiques/datasets/drivers.csv", header = TRUE, dec = ",", sep = ";")
donnees2 <- read.csv("L://BUT/SD/Promo 2023/acordier/Progra statistiques/datasets/gliders.csv", header = TRUE, dec = ",", sep = ";")
donnees3 <- read.csv("L://BUT/SD/Promo 2023/acordier/Progra statistiques/datasets/tires.csv", header = TRUE, dec = ",", sep = ";")
setwd(dir = "L://BUT/SD/Promo 2023/acordier/Progra statistiques/datasets")
getwd()
bodies_karts = read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")
tires = read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")
gliders = read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")
drivers = read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")
dim(bodies_karts)
dim(tires)
dim(gliders)
dim(drivers)
summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)
plot(x = drivers$Weight,
     y = drivers$Acceleration, 
     main = "Drivers : Poids / Accélération")
cor(x = drivers$Weight,
    y = drivers$Acceleration)
#5/Corrélation
coefCorr = cor(x = drivers$Weight,
               y = drivers$Acceleration)
coefDeter = coefCorr^2
print(coefDeter)




#6/Matrice
matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
View(matriceCor)
#7/Installation package
install.packages("corrplot")
#8/Coréllogramme
library(corrplot)
corrplot(matriceCor, method = "circle")
#9/autres coréllogramme
matriceCor = round(cor(tires[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE )
matriceCor = round(cor(gliders[ , -1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE )
matriceCor = round(cor(bodies_karts[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)
#Exercice3
resultat = drivers[ , c("Driver" , "Weight")]
View(resultat)
