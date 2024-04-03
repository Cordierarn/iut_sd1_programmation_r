#1
salaire_net_cadre = function(salaire_brut){
  calc = salaire_brut*(1-25/100)
  return(calc)
}
salaire_net_cadre(1000)
#2
salaire_net_cadre = function(salaire_brut=2500){
  calc = salaire_brut*(1-25/100)
  return(calc)
}
salaire_net_cadre()
#3
salaire_net_cadre = function(salaire_brut=2500,temps_travail=1){
  calc = salaire_brut*(1-25/100)*temps_travail
  return(calc)
}
salaire_net_cadre()
#4
salaire_net_cadre = function(salaire_brut=2500,temps_travail=1){
  if (!is.numeric(salaire_net_cadre)){
    return("Erreur : Le salaire rentré n'est pas numérique !")
  }
  
  
  calc = salaire_brut*(1-25/100)*temps_travail
  return(calc)
}
salaire_net_cadre("2000$")
#5
salaire_net_cadre = function(salaire_brut=2500,temps_travail=1){
  if (!is.numeric(salaire_brut)){
    return("Erreur : Le salaire rentré n'est pas numérique !")
  }
  
  if (!is.numeric(temps_travail)){
    return("Erreur : le temps de travail n'est pas numérique !")
  }
  
  if ((temps_travail>1) | (temps_travail<0)){
    return("Le temps de travail doit être compris entre 0 et 1.")
  }
  
  calc = salaire_brut*(1-25/100)*temps_travail
  return(calc)
}
salaire_net_cadre(salaire_brut = 2000, temps_travail = "100%")
salaire_net_cadre(salaire_brut = 2000, temps_travail = 0.8)
salaire_net_cadre(salaire_brut = 2000, temps_travail = 100)
#6
salaire_net = function(salaire_brut,temps_travail=1,statut){
  if (!is.numeric(salaire_brut)){
    return("Erreur : Le salaire rentré n'est pas numérique !")
  }
  
  if (!is.numeric(temps_travail)){
    return("Erreur : le temps de travail n'est pas numérique !")
  }
  
  if ((temps_travail>1) | (temps_travail<0)){
    return("Le temps de travail doit être compris entre 0 et 1.")
  }
  if (!statut %in% c("cadre","non cadre")){
    return("Le statut doit être cadre ou non cadre")
  }
  if (statut == "cadre"){
    calc = salaire_brut * 0.75 * temps_travail
  }else{
    calc = salaire_brut * 0.78 * temps_travail
  }
  return(calc)
}
salaire_net(salaire_brut = 2000, statut = "cadre")
salaire_net(salaire_brut = 2000, statut = "non cadre")
salaire_net(salaire_brut = 2000, statut = "technicien")
#7
salaire_net = function(salaire_brut,temps_travail=1,statut){
  if (!is.numeric(salaire_brut)){
    return("Erreur : Le salaire rentré n'est pas numérique !")
  }
  
  if (!is.numeric(temps_travail)){
    return("Erreur : le temps de travail n'est pas numérique !")
  }
  
  if ((temps_travail>1) | (temps_travail<0)){
    return("Le temps de travail doit être compris entre 0 et 1.")
  }
  if (!statut %in% c("cadre","non cadre")){
    return("Le statut doit être cadre ou non cadre")
  }
  if (statut == "cadre"){
    calc = salaire_brut * 0.75 * temps_travail
  }else{
    calc = salaire_brut * 0.78 * temps_travail
  }

  if (calc <= 1591){
    salaire_net_ap_impo = calc
  }else if (calc <=2006){
    salaire_net_ap_impo = calc * (1-0.029)
  }else if (calc <= 3476){
    salaire_net_ap_impo = calc * (1-0.099)
  }else if (calc<=8557){
    salaire_net_ap_impo = calc * (1-0.2)
  }else{
    salaire_net_ap_impo = calc * (1-0.43)
  }
  return(salaire_net_ap_impo)
}
salaire_net(salaire_brut = 2000, statut = "cadre")
salaire_net(salaire_brut = 4000, statut = "non cadre")
salaire_net(salaire_brut = 10500, statut = "cadre")
#8

#Exercice2
#1
resultat = 0
for( i in c(1,2,3,4,5)){
  i = i + 1
  resultat = resultat + i 
}
print(resultat)
#2
resultat = 0
x = 1
while (resultat<=50) {
  i = i + 1
  resultat = resultat + x 
  print(paste("le resultat est : ",resultat))
  print(paste("le programme s'est arrêté à la valeur : ", i))
}
#3
for (colonne in colnames(iris)) {
  type_colonne = class(iris[ , colonne])
  print(paste("la colonne ", colonne, " est de type : ", type_colonne))
}
#4
i = 1
while(i <= ncol(iris)){
  nom_colonne = colnames(iris)[i]
  type_colonne = class(iris[,nom_colonne])
  print(paste("la colonne ", nom_colonne, " est de type : ", type_colonne))
  i = i + 1

}
#Exercice 3
#1
# Boucle pour demander 5 fois un nombre à l'utilisateur
for (i in 1:5) {
  
  # Demander à l'utilisateur d'entrer un nombre
  nombre <- readline(prompt = "Entrez le nombre :")
  nombre <- as.numeric(nombre)
  
  # Calculer le carré du nombre
  carre <- nombre^2
  
  # Afficher le carré du nombre
  print(paste("Le carré de", nombre, "est", carre))
}


