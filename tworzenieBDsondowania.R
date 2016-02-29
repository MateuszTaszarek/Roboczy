library(parallel)
baza <- readRDS(file="data/baza.rds")


mojafunkcja <- function(lista){
  c(lista, lista[['res']]$HGHT-lista[['staty']][3])
}

mojafunkcja <- function(lista){
  nowa_zmienna <- lista[['res']]$HGHT-lista[['staty']][3]
  
  res <- lista[['res']]
  res$nowa_zmienna
  nowa_lista <-list(lista, list(nowa_zmienna))
  return(nowa_lista)
}

a <- mclapply(baza, mojafunkcja,mc.cores = 10)


#nauka_funkcji





baza1 <- baza[[1]]

dodajdolisty <- function(baza1){
  a1 <- baza1[['res']]$HGHT-baza1[['staty']][3]
  baza1[['res']] <- cbind(baza1[['res']], data.frame(HGHT2=a1))
  return(baza1)
}

dodajdolisty(baza1)

ala <- lapply(baza, dodajdolisty)





staty_giver_lista <- function(listewka){
  staty_giver <- function(podlista){
    podlista[['staty']][10]
  }
  lapply(listewka, staty_giver)
}

stacja_giver_lista <- function(listewka){
  date_giver <- function(podlista){
    podlista[['res']][,2]
  }
  lapply(listewka, date_giver)
}


moje_staty <- stacja_giver_lista(baza)
wysokosc <- staty_giver_lista(baza)
nowy <- mapply("-", moje_staty,wysokosc)


table(unlist(lapply(moje_staty, function(x) x[3])))
hist(unlist(lapply(moje_staty, function(x) x[6])))
table(round(unlist(lapply(moje_staty, function(x) x[12]))/10)) # w cape'y rzedu 23390 nie wierze

#=================================================================

#otwieranie bazy danych
baza <- readRDS(file="data/baza.rds")

#definiowanie konkretnego elemetnu w liscie 
staty_giver_lista <- function(listewka){
  staty_giver <- function(podlista){
    podlista[['staty']][9]
  }
  lapply(listewka, staty_giver)
}

#odpalanie funkcji 
staty_giver_lista(baza)

#konwersja z listy do wektora 
unlist(staty_giver_lista(baza))

#tworzenie nowego obiektu 
t3 <- unlist(staty_giver_lista(baza))

#łączenie
N1 <- cbind(t1,t3)
