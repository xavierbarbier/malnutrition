# Installation et chargement des packages necessaires
install.packages("dplyr")

install.packages("reshape2")

install.packages("RMySQL")

library(RMySQL)
library(reshape2)
library(readr)
library(dplyr)

#QUESTION 1
## Import données

population <- read_csv("data/FAO/FAOSTAT_data_population_11-18-2019.csv")


##calcul population mondiale en milliard
Total<-((sum(population$Valeur))*1000) #7.2 milliards en 2013
pctaugmantation20132018<-((7.534*100000000000)/Total)

#QUESTION 3

##Import des données
animal <- read_csv("data/FAO/FAOSTAT_data_animal_11-18-2019.csv")
vegetal <- read_csv("data/FAO/FAOSTAT_data_vegetal_11-18-2019.csv")

##Ajout variable Origine
animal$origin<-c("Animal")
vegetal$origin<-c("Vegetal")

## Création data frame commun
animal_vegetal<-rbind(animal,vegetal)

## Pivot
bilan = dcast(animal_vegetal, animal_vegetal$`Code Pays` + animal_vegetal$Pays + animal_vegetal$`Code Produit` + animal_vegetal$Produit + animal_vegetal$Année + animal_vegetal$origin ~ animal_vegetal$`Élément`, value.var='Valeur', fun.aggregate=sum)

##Renomage pour merge
names(bilan)[2]<-"Pays"
names(population)[4]<-"Pays"
names(population)[12]<-"Population"

## merge bilan et population
bilan<-merge(bilan,population,by="Pays")

## Renomage colonnes
names(bilan)[2]<-"Code Pays"
names(bilan)[3]<-"Code Produit"
names(bilan)[4]<-"Produit"
names(bilan)[5]<-"Année"
names(bilan)[6]<-"Origine"

##Suppression varaibles en double
bilan <- bilan[-27]
bilan <- bilan[-27]

##Conversion unité population
bilan$Population<-bilan$Population*1000

##calcul disponibilité alimentaire en kcal par an
bilan["Dispo alimentaire kcal par an"]<-bilan$`Disponibilité alimentaire (Kcal/personne/jour)`*365*bilan$Population

##Calcul dispo proteine par an en kg
bilan["Dispo prot kg par an"]<-(bilan$`Disponibilité de protéines en quantité (g/personne/jour)`*365*bilan$Population)/1000  

#QUESTION 4

## Ratio énergie/poids
bilan["Ratio énergie/poids"]<-(bilan$`Disponibilité alimentaire (Kcal/personne/jour)`*365)/bilan$`Disponibilité alimentaire en quantité (kg/personne/an)`

## calcul moyenne pour vérifier + commentaire comparaison source
mean(bilan$`Ratio énergie/poids`[bilan$Produit == "Oeufs"], na.rm=TRUE) 
###-> 1363.863 kcal/kg - Wikipedia = 1470kcal/kg

## calcul Ratio protéines/poids total 
bilan["Ratio protéines/poids total"]<- bilan$`Dispo prot kg par an`/(bilan$`Disponibilité alimentaire en quantité (kg/personne/an)`*bilan$Population)

##moyenne pour vérifier + commentaire comparaison source  
mean(bilan$`Ratio protéines/poids total`[bilan$Produit == "Avoine"], na.rm=TRUE) 
###-> 0.07686093 - Wikipedia = 0.01070

#QUESTION 5
  
##Calories
caloriques<-bilan[bilan$`Disponibilité alimentaire (Kcal/personne/jour)`> 0 & bilan$`Disponibilité alimentaire en quantité (kg/personne/an)`> 0,]
caloriques<-tapply(caloriques$`Ratio énergie/poids`,caloriques$Produit,mean)
caloriques<-as.data.frame(caloriques)  
caloriques

##Proteines
proteines<-bilan[bilan$`Disponibilité de protéines en quantité (g/personne/jour)`> 0 & bilan$`Disponibilité alimentaire en quantité (kg/personne/an)`> 0,]
proteines<-tapply(proteines$`Ratio protéines/poids total`,proteines$Produit,mean)
proteines<-as.data.frame(proteines) 
proteines

#QUESTION 6

##Selection origine Vegetal pour création bilan vegetal 
bilan_vegetal<-bilan[bilan$Origine == "Vegetal",]

## Selection valeurs > pour éviter inf & NA
bilan_vegetal<-bilan_vegetal[bilan_vegetal$`Disponibilité alimentaire (Kcal/personne/jour)`> 0,]
bilan_vegetal<-bilan_vegetal[bilan_vegetal$`Disponibilité alimentaire en quantité (kg/personne/an)`> 0,]
bilan_vegetal<-bilan_vegetal[bilan_vegetal$`Disponibilité intérieure`> 0,]

## Création variable Dispo int Kcal
bilan_vegetal["Dispo int kcal"]<-bilan_vegetal$`Ratio énergie/poids`*bilan_vegetal$`Disponibilité intérieure`*1000*1000

#Calcul disponibilité intérieure mondiale
dispo_int_mond_vege<-sum(bilan_vegetal$`Dispo int kcal`, na.rm = TRUE)
dispo_int_mond_vege #-> 1.216221e+16

#QUESTION 7     
## Nombre humain apport minimum kcal via vegetal
(dispo_int_mond_vege/(2800*365))/1000000000 #2800 kcal/jours
((dispo_int_mond_vege/(2800*365))/Total)*100 #-> 165%

##Dispo intérieure proteine en kg
bilan_vegetal$Dispo_int_prot_kg<-(bilan_vegetal$`Disponibilité intérieure`*1000*1000)*bilan_vegetal$`Ratio protéines/poids total` 
dispo_int_mond_prot<-sum(bilan_vegetal$Dispo_int_prot_kg[bilan_vegetal$`Disponibilité alimentaire en quantité (kg/personne/an)`> 0], na.rm = TRUE)

(dispo_int_mond_prot/(0.066*365))/1000000000 #66 g/jours
((dispo_int_mond_prot/(0.066*365))/Total)*100 #-> 156%

#Question 8

## Calcul disponibilité végétale (nourriture + feed + pertes)
bilan_vegetal["Dispo vegetale"]<-bilan_vegetal$Nourriture + bilan_vegetal$`Aliments pour animaux`+ bilan_vegetal$Pertes

## Calcul disponibilité végétale Kcal
bilan_vegetal["Dispo vegetale kcal"]<-bilan_vegetal$`Ratio énergie/poids`*((bilan_vegetal$`Dispo vegetale`)*1000*1000)

## Calcul disponibilité végétale kcal totale
dispo_vege<-sum(bilan_vegetal$`Dispo vegetale kcal`, na.rm = TRUE)
dispo_vege #-> 9.168226e+15  kcal soit 9168226 milliards kcal

##Nombre de personnes + ratio
(dispo_vege/(2800*365))/1000000000 #2800 kcal/jours -> 8,9 milliards
((dispo_vege/(2800*365))/Total)*100 #-> 124%

## disponibilité végétale totale proteine
bilan_vegetal$Dispo_vege_prot_kg<-(bilan_vegetal$`Dispo vegetale`*1000*1000)*bilan_vegetal$`Ratio protéines/poids total` 
dispo_vegetal_prot<-sum(bilan_vegetal$Dispo_vege_prot_kg[bilan_vegetal$`Disponibilité alimentaire en quantité (kg/personne/an)`> 0], na.rm = TRUE)

##Nombre de personnes + ratio
(dispo_vegetal_prot/(0.066*365))/1000000000 #66 g/jours -> 8,4 milliards
((dispo_vegetal_prot/(0.066*365))/Total)*100 #-> 116 %

#Question 9

## Calcul disponibilité totale (int + végétale) + ratio
pop_vege<-(sum(bilan$`Dispo alimentaire kcal par an`)/(2800*365)) #7.251051 Milliards
pop_vege/Total*100 #-> 100%

## disponibilité totale proteine + ratio
prot_vege<-(sum(bilan$`Dispo prot kg par an`)/(0.066*365)) #8,6 Milliards
prot_vege/Total*100 #-> 120%
 
#Question 10
##Import des données
malnutrition <- read_csv("data/FAO/FAOSTAT_data_malnutrition_11-18-2019.csv", 
                         col_types = cols(Valeur = col_number()))

## Pivot
sousnutrition = dcast(malnutrition, malnutrition$`Code zone` + malnutrition$`Zone`  +
                malnutrition$Unité + malnutrition$Symbole + 
                malnutrition$Note + malnutrition$Élément + malnutrition$`Description du Symbole` ~ malnutrition$Année   , value.var='Valeur')

## Renommage des variables
names(sousnutrition)[1]<-"Code zone"
names(sousnutrition)[2]<-"Zone"
names(sousnutrition)[3]<-"Unité"
names(sousnutrition)[4]<-"Symbole"
names(sousnutrition)[5]<-"Note"
names(sousnutrition)[6]<-"Element"
names(sousnutrition)[7]<-"Description du Symbole"

##Nombre personnes sous nutrition en 2013-2015
nb_pers_sousnutrition2013<-sum(sousnutrition$`2013-2015`, na.rm = TRUE)
nb_pers_sousnutrition2013 #737.4 millions

## ratio
((nb_pers_sousnutrition2013*1000000)/Total)*100 # = 10,22%

#Question 11

crl <- read_csv("data/FAO/FAOSTAT_data_crl_11-18-2019.csv")

##création variable is_cereal
bilan$is_cereal<-c("")

## Liste des codes produits crl
list_crl<-unique(crl$`Code Produit`)

##boucle pour booleen is_cereal
for (i in 1:dim(bilan)[1]) {
  if (bilan$`Code Produit`[i] %in% list_crl) {
    bilan$is_cereal[i] <- TRUE
  } else {bilan$is_cereal[i] <- FALSE}
  
}

## total 
bilan_crl<-bilan[bilan$is_cereal == "TRUE",]
sum(bilan_crl$`Aliments pour animaux`)/((sum(bilan_crl$`Aliments pour animaux`) + sum(bilan_crl$Nourriture)))*100
### -> 46%

# QUESTION 12

## liste pays sous nutrition
list_sousnutrition<-unique(sousnutrition$Zone[sousnutrition$`2013-2015` !=0])

## création variable sous nutrition
bilan$sous_nutrition<-c("")

##boucle pour booleen sous_nutrition
for (i in 1:dim(bilan)[1]) {
  if (bilan$Pays[i] %in% list_sousnutrition) {
    bilan$sous_nutrition[i] <- TRUE
  } else {bilan$sous_nutrition[i] <- FALSE}
}

## regroupement pays sous nutrition dans un data frame
bilan_pays_sousnutrition<-bilan[bilan$sous_nutrition == TRUE,]

## liste exportation
export<-tapply(bilan_pays_sousnutrition$`Exportations - Quantité`,bilan_pays_sousnutrition$Produit,sum)
export<-as.data.frame(export) 
export

## creation variable produit
export$Produit <- rownames(export)

## ordonnage variable export puis selection top15
top<-export[order(export$export,decreasing = TRUE),]

top[1:15,]

## liste produit export des pays sous nutrition
list_export<-top[1:15,]$Produit
list_export

## création variable sous nutrition
bilan$import_sous_nutrition<-c("")

##boucle pour booleen sous_nutrition
for (i in 1:dim(bilan)[1]) {
  if (bilan$Produit[i] %in% list_export) {
    bilan$import_sous_nutrition[i] <- TRUE
  } else {bilan$import_sous_nutrition[i] <- FALSE}
  
}

## regroupement pays sous nutrition dans un data frame
bilan_import_pays_sousnutrition<-bilan[bilan$import_sous_nutrition == TRUE,]

## classement descendant + sélection 200
bilan_import_pays_sousnutrition<-bilan_import_pays_sousnutrition %>% arrange(desc(bilan_import_pays_sousnutrition$`Importations - Quantité`))

## selection des 200 importations top
bilan_import_pays_sousnutrition<-bilan_import_pays_sousnutrition[1:200,]

## Conservation variables utiles
bilan_import_pays_sousnutrition<-bilan_import_pays_sousnutrition%>%select(Produit,`Aliments pour animaux`,`Autres Utilisations`,`Disponibilité intérieure`,`Exportations - Quantité`)

## Aggregation par produit
importations<-bilan_import_pays_sousnutrition%>%
  group_by(Produit)%>%
  summarise_all(funs(sum))

## RATIO
importations$Ratio_autres_int<- importations$`Autres Utilisations`/importations$`Disponibilité intérieure`
importations$Ratio_animal_tot<- importations$`Aliments pour animaux`/(importations$`Aliments pour animaux`+importations$`Disponibilité intérieure`)

## Top 3 ratio
top_Ratio_autres_int<-importations[order(importations$Ratio_autres_int,decreasing = TRUE),]
top_Ratio_autres_int[1:3,]

Ratio_animal_tot<-importations[order(importations$Ratio_animal_tot,decreasing = TRUE),]
Ratio_animal_tot[1:3,]

#QUESTION 13

usa_crl<-bilan_crl[bilan_crl$Pays == "États-Unis d'Amérique",]
sum(usa_crl$`Aliments pour animaux`)*0.1 # -> 14009.6 milliers de tonnes



#QUESTION 14
##ratio manioc
bilan$`Exportations - Quantité`[bilan$Pays == "Thaïlande" & bilan$Produit == "Manioc"]/bilan$Production[bilan$Pays == "Thaïlande" & bilan$Produit == "Manioc"]
##-> 0.8341273

##ratio sous nutrition
(sousnutrition$`2013-2015`[sousnutrition$Zone == "Thaïlande"]*1000000)/(population$Population[population$Pays == "Thaïlande"]*1000)
###-> 0.08071082

#QUESTION 15

#Importation données FAO 2012-2019
population1219 <- read_csv("data/FAO/FAOSTAT_data_12-20-2019.csv")

## sélection des variables a conserver
population1219<-select(population1219,Zone,`Code zone`,Année,Valeur)

## Renommage des variables
names(population1219)[1]<-"pays"
names(population1219)[2]<-"code_pays"
names(population1219)[3]<-"annee"
names(population1219)[4]<-"population"

## Création table dans BDD
mydb <- dbConnect(RSQLite::SQLite(), ":openclassrooms:")

dbSendQuery(mydb,"CREATE TABLE population(pays VARCHAR(255),code_pays INTEGER,annee INTEGER,population INTEGER, PRIMARY KEY (code_pays, annee))")

dbWriteTable(mydb, name="population", population1219, overwrite = TRUE, row.names = FALSE)

dbGetQuery(mydb, "SELECT * FROM population")

#QUESTION 16
dispo_alim_vegetal1219 <- read_csv("data/FAO/dispo_alim_vegetal1219.csv")
dispo_alim_animal1219 <- read_csv("data/FAO/dispo_alim_animal1219.csv")

dispo_alim_animal1219$origin<-c("Animal")
dispo_alim_vegetal1219$origin<-c("Vegetal")

dispo_alim_animal_vegetal1219<-rbind(dispo_alim_animal1219,dispo_alim_vegetal1219)

bilan1219 = dcast(dispo_alim_animal_vegetal1219, dispo_alim_animal_vegetal1219$`Code zone` + dispo_alim_animal_vegetal1219$Zone + dispo_alim_animal_vegetal1219$`Code Produit` + dispo_alim_animal_vegetal1219$Produit + dispo_alim_animal_vegetal1219$Année + dispo_alim_animal_vegetal1219$origin ~ dispo_alim_animal_vegetal1219$`Élément`, value.var='Valeur', fun.aggregate=sum)

#Renomage pour merge
names(bilan1219)[2]<-"Pays"
names(population1219)[1]<-"Pays"
names(population1219)[4]<-"Population"

### merge bilan et population
bilan1219<-merge(bilan1219,population1219,by="Pays")

bilan1219$dispo_alim_tonnes<-(bilan1219$`Disponibilité alimentaire en quantité (kg/personne/an)`*bilan1219$Population*1000)/1000

# Renomage colonnes
names(bilan1219)[1]<-"pays"
names(bilan1219)[2]<-"code_pays"
names(bilan1219)[3]<-"code_produit"
names(bilan1219)[4]<-"produit"
names(bilan1219)[6]<-"origin"
names(bilan1219)[7]<-"dispo_alim_kcal_p_j"
names(bilan1219)[9]<-"dispo_mat_gr"
names(bilan1219)[10]<-"dispo_prot"

bilan1219 <- bilan1219[-5]

dispo_alim<-select(bilan1219,dispo_alim_tonnes,pays,code_pays,code_produit,produit,annee,origin,dispo_alim_kcal_p_j,dispo_mat_gr,dispo_prot)

### BDD SQL
mydb <- dbConnect(RSQLite::SQLite(), ":openclassrooms:")
dbSendQuery(mydb,"CREATE TABLE dispo_alim (pays VARCHAR(255),code_pays INTEGER,code_produit INTEGER, produit VARCHAR(255) , origin VARCHAR(255), annee INTEGER,dispo_alim_kcal_p_j NUMERIC ,dispo_mat_gr NUMERIC,dispo_prot NUMERIC,PRIMARY KEY (code_pays, annee, code_produit))")

dbWriteTable(mydb, name="dispo_alim", value=dispo_alim, overwrite = TRUE, row.names = FALSE)

dbGetQuery(mydb, "SELECT * FROM dispo_alim")

#QUESTION 17
equilibre_prod1219 <- read_csv("data/FAO/equilibre_prod1219.csv")

equilibre_prod <- dcast(equilibre_prod1219,equilibre_prod1219$`Code zone`+ equilibre_prod1219$Zone + equilibre_prod1219$`Code Produit`+equilibre_prod1219$Produit+equilibre_prod1219$Année ~ equilibre_prod1219$`Élément`, value.var='Valeur', fun.aggregate=sum)

names(equilibre_prod)[2]<-"pays"
names(equilibre_prod)[1]<-"code_pays"
names(equilibre_prod)[3]<-"code_produit"
names(equilibre_prod)[4]<-"produit"
names(equilibre_prod)[5]<-"annee"
names(equilibre_prod)[8]<-"dispo_int"
names(equilibre_prod)[6]<-"alim_ani"
names(equilibre_prod)[11]<-"semences"
names(equilibre_prod)[10]<-"pertes"
names(equilibre_prod)[12]<-"transfo"
names(equilibre_prod)[9]<-"nourriture"
names(equilibre_prod)[7]<-"autres_utilisations"

### BDD SQL
mydb <- dbConnect(RSQLite::SQLite(), ":openclassrooms:")
dbSendQuery(mydb,"CREATE TABLE equilibre_prod (pays VARCHAR(255),code_pays INTEGER,code_produit INTEGER, produit VARCHAR(255) , origin VARCHAR(255), annee INTEGER,dispo_alim_kcal_p_j NUMERIC ,dispo_mat_gr NUMERIC,dispo_prot NUMERIC,PRIMARY KEY (code_pays, annee, code_produit))")

dbWriteTable(mydb, name="equilibre_prod", equilibre_prod, overwrite = TRUE, row.names = FALSE)

dbGetQuery(mydb, "SELECT * FROM equilibre_prod")

#QUESTION 18

names(malnutrition)[4]<-"pays"
names(malnutrition)[3]<-"code_pays"
names(malnutrition)[10]<-"annee"
names(malnutrition)[12]<-"nb_personnes"

sous_nutrition<-select(malnutrition,nb_personnes,annee,code_pays,pays)

#boucle pour booleen pour années
for (i in 1:dim(sous_nutrition)[1]) {
  if (sous_nutrition$annee[i] %in% c("2012-2014")) {
    sous_nutrition$annee[i] <- 2013  } 
  if (sous_nutrition$annee[i] %in% c("2013-2015")) {
    sous_nutrition$annee[i] <- 2014  }
  if (sous_nutrition$annee[i] %in% c("2014-2016")) {
    sous_nutrition$annee[i] <- 2015  }
  if (sous_nutrition$annee[i] %in% c("2015-2017")) {
    sous_nutrition$annee[i] <- 2016 }
  if (sous_nutrition$annee[i] %in% c("2016-2018")) {
    sous_nutrition$annee[i] <- 2017}
}


### BDD SQL
mydb <- dbConnect(RSQLite::SQLite(), ":openclassrooms:")

dbSendQuery(mydb,"CREATE TABLE sous_nutrition (pays VARCHAR(255),code_pays INTEGER, annee INTEGER,nb_personnes NUMERIC ,PRIMARY KEY (code_pays, annee))")

dbWriteTable(mydb, name="sous_nutrition", sous_nutrition, overwrite = TRUE, row.names = FALSE)

dbGetQuery(mydb, "SELECT * FROM sous_nutrition")


#QUESTION 19
mydb <- dbConnect(RSQLite::SQLite(), ":openclassrooms:")

## top 10 ratio disponibilité alimentaire/habitant prot

dbGetQuery(mydb, "SELECT pays,ROUND(sum(dispo_prot)/1000,2) AS Ratio_dispo_prot_kg FROM `dispo_alim` 
GROUP BY pays
ORDER BY dispo_prot DESC
LIMIT 10")

## top 10 ratio disponibilité alimentaire/habitant kcal

dbGetQuery(mydb, "SELECT pays,ROUND(sum(dispo_alim_kcal_p_j)/1000,2) AS Ratio_dispo_alim_kcal  FROM `dispo_alim` 
GROUP BY pays 
ORDER BY dispo_alim_kcal_p_j DESC
LIMIT 10")

## top 10 ratio disponibilité alimentaire/habitant prot par année

dbGetQuery(mydb, "SELECT * FROM (SELECT pays,annee,sum(dispo_prot/1000) AS dispo_prot_kg, ROW_NUMBER() OVER (PARTITION BY annee ORDER BY sum(dispo_prot/1000) ASC) AS rank
FROM dispo_alim 
GROUP BY pays
) WHERE rank <= 10
           ")

##La quantité totale (en kg) de produits perdus par pays et par année.
dbGetQuery(mydb, "SELECT `pays`, annee, SUM(pertes) 
FROM equilibre_prod  
GROUP BY pays, annee
           ORDER BY annee")

##Les 10 pays pour lesquels la proportion de personnes sous-alimentées est la plus forte
dbGetQuery(mydb,"SELECT population.Pays, sous_nutrition.pays,population.annee, sous_nutrition.annee, sous_nutrition.nb_personnes/((population.Population*1000)/100000) AS ratio 
           FROM sous_nutrition, population 
           WHERE (population.Pays = sous_nutrition.pays AND population.annee = sous_nutrition.annee ) 
           ORDER BY ratio DESC 
           LIMIT 10")

##Les 10 produits pour lesquels le ratio Autres utilisations/Disponibilité intérieure
dbGetQuery(mydb, "SELECT produit, autres_utilisations/dispo_int AS Ratio   
FROM equilibre_prod  
GROUP BY produit
ORDER BY Ratio DESC 
LIMIT 10
")


