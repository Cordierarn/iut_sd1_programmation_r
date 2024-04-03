#Exercice1
#1
# Créer une toile de fond vide pour le graphique
par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 1), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité \n de lois normales")
#2
moy = c(0,0,0,-2)
sig = c(0.45,1,2.25,0.7)
colors = c("red","green","blue","yellow")
legend_labels = c()
for ( i in 1:length(moy)){
  serie = rnorm(100, mean = moy[i], sd = sig[i])
  lines(density(serie), col = colors[i])
  legend_labels <- c(legend_labels, paste("m =", moy[i], ",", "s =", sig[i]))
}
legend("topright", legend=legend_labels, col=colors, lwd=2, cex=0.8)
#3
serie = rnorm(n = 10000, mean = 0, sd = 1)
#4
hist(serie, main = "loi normale centrée réduite", probability = TRUE)
line(density(serie))
#5
median(serie)
#6
quantile(serie)
#7
quantile(serie, 
         probs = seq(from = 0, 
                     to = 1, by = 0.01))

quantile(serie, 
         probs = 0.95)
#8
qnorm(0.95, mean = 0, sd =  1)
pnorm(1.644854, mean = 0, sd = 1)
#9
qnorm(0.975, mean = 0, sd = 1)
#10
pnorm(1.96, mean = 0, sd = 1)
#Exercice2
#1
ind = seq(from = 0, to = 3.9, by = 0.1)
all_probas = c()
for ( i in ind){
   prob = pnorm(i, mean = 0, sd = 0)
   all_probas = c(all_probas, prob)
   all_probas = round(all_probas, 4)
}
#2
ind = seq(from = 0, to = 3.9, by = 0.1)
lig = seq(from = 0, to = 0.09, by = 0.01)
resultat = NULL
for (j in lig){
  all_probas = c()
for ( i in ind){
  quantile = i + j
  prob = pnorm(quantile, mean = 0, sd = 0)
  all_probas = c(all_probas, prob)
  all_probas = round(all_probas, 4)
}
  resultat = cbind(resultat,all_probas)
}
#3
class(resultat)
table = data.frame(resultat)
colnames(table) = ind
rownames(table) = lig
View(table)
#Exercice3
#1
population = rnorm(1e7,mean = 171, sd = 9)
#2
mean(population)
sd(population)
#3
hist(population)
#4
resultat = 0
for ( i in 1:length(population)){
  if(population[i]<190){
    resultat = resultat + 1
  }
}
print(resultat)
#4bis
#observé
pop190 = population[population < 190]
length(pop190)
length(pop190) / length(population)
moyenne_pop=mean(population)
sd_pop=sd(population)
#en théorie
pnorm(q = 190, mean=moyenne_pop, sd=sd_pop)*1e7
#5
resultat = 0
for ( i in 1:length(population)){
  if(population[i]>200){
    resultat = resultat + 1
  }
}
print(resultat)
1e7-pnorm(200, mean = moyenne_pop,sd = sd_pop)*1e7
#Exercice4
#1
