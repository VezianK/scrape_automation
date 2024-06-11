library(RSelenium)
library(rvest)
library(tidyverse)

extraire_tableaux_orias <- function(numero_orias, browser = "firefox") {
  # Générer un port aléatoire
  port = sample(4567:49151, 1)
  
  # Démarrage du serveur Selenium (avec message d'attente)
  cat("Démarrage du serveur Selenium sur le port", port, "...\n")
  rD = rsDriver(browser = browser, port = port, check = TRUE)
  remDr = rD$client
  
  # Navigation vers la page de recherche ORIAS
  cat("Ouverture de la page ORIAS...\n")
  remDr$navigate("https://www.orias.fr/home/resultSearch")
  
  # Localisation de la barre de recherche
  cat("Remplissage de la barre de recherche...\n")
  search_bar = remDr$findElement(using = "id", value = "searchInput")
  search_bar$clearElement()
  search_bar$sendKeysToElement(list(numero_orias))
  
  # Appuyer sur la touche Entrée
  search_bar$sendKeysToElement(list(key = "enter"))
  
  # Attendre le chargement de la page de résultats
  cat("Attente du chargement des résultats...\n")
  Sys.sleep(3)
  
  # Extraire le code HTML de la page
  page_source = remDr$getPageSource()[[1]]
  
  # Parser le code HTML avec rvest
  page = read_html(page_source)
  
  # Extraire tous les tableaux
  tableaux = page %>% html_elements("table")
  
  # Vérifier si des tableaux ont été trouvés
  if (length(tableaux) == 0) {
    tableaux <- NULL # Retourner NULL si aucun tableau n'a été trouvé
  }
  
  # Fermer la session Selenium
  remDr$close()
  rD$server$stop()
  cat("Fermeture du serveur Selenium.\n")
  
  # Retourner les tableaux extraits
  return(tableaux)
}

Retour=function(b,browser="firefox"){
  resultats=extraire_tableaux_orias(b,browser="firefox")
  # Afficher les tableaux (optionnel)
  if (length(resultats) > 0) {
    for (i in seq_along(resultats)) {
      cat("\nTableau", i, ":\n")
      return(html_table(resultats[[i]]))
    }
  } else {
    return(c(0))
  }
}


f=Retour("08041535","firefox")

print(f)


id1="Emeric SCHUMAN"

# Mesure du temps
start_time <- Sys.time()

c <- NULL
for(i in 1:length(id1)) {
  c <- cbind(c, list(Retour(id1[i],"firefox")))
}

end_time <- Sys.time()



# Calcul du temps écoulé
elapsed_time <- end_time - start_time

print(elapsed_time)
## Time difference of 14.59268 secs



df <- data.frame(
  "Immatriculation Orias" = character(),
  Statut = character(),
  SIREN = character(),
  Dinomination = character(),
  "Catigorie(s)" = character(),
  CP = character(),
  Ville = character(),
  Pays = character(),
  Ditails = character()
)


Extraire=function(id,browser="firefox"){ 
  n=1
  if(n%%3==0){ 
    Sys.sleep(200*rnorm(1,mean=1.5))
  }
  if(n%%10==0){ 
    Sys.sleep(600*rnorm(1,mean = 2))}
  if(n%%20==0){ 
    Sys.sleep(1200*rnrom(1,mean=1.2))}
  for(i in 1:length(id)){ 
    n=n+1
    data_transition=as.data.frame(Retour(id[i],browser)) ## datame sortie
    if(length(data_transition)==1){ ## On verifie s'il est null ou pas 
      df[nrow(df) + 1, ] = c(id[i],rep("Non_inscrit",8))
    }
    else{ df[nrow(df) + 1, ] = data_transition
    }
  }
  
  return(df)
}



id=c("07031790", "07008257", "14005258", "21001063", "21008808", "19001130",
      "Carole CHAVANCE", "AUTOMATISME ET TECHNIQUES AVANCEES", " AUTOMATISME ET TECHNIQUES AVANCEES", "10010101010",
      "07008257")
length(id)



start_time <- Sys.time()

data=Extraire(id,"firefox")

end_time <- Sys.time()

elapsed_time <- end_time - start_time

print(elapsed_time)

print(data)


id1=c(id,"21005548","21005545","19004944","19004944","19006276","20007703","19006276")

id1=rep(id1,2)
length(id1) ## 36


start_time <- Sys.time()

data=Extraire(id1)

end_time <- Sys.time()

elapsed_time <- end_time - start_time

print(elapsed_time)

print(data)

id3=rep(id1,2)
length(id3)

start_time <- Sys.time()

data=Extraire(id3)

end_time <- Sys.time()

elapsed_time <- end_time - start_time

print(elapsed_time)


print(data)

write_csv(data)

