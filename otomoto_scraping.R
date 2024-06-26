library(rvest)
library(dplyr)
library(readr)

#parametry samochodów, które będziemy analizować
id <- ""
Marka_pojazdu <- ""
Model_pojazdu <- ""
Rok_produkcji <- ""
Przebieg <- ""
Pojemność_skokowa <- ""
Rodzaj_paliwa <- ""
Moc <- ""
Kolor <- ""
Typ_nadwozia <- ""
Cena <- ""

#każdy wiersz dostanie swój indywidualny identyfikator
id_i <- 1

#j - liczba stron samochodów które będą przeszukiwane przez skrypy
for(j in 1:110){
  Sys.sleep(3)  #pauza w działaniu systemu, aby nie obciążyć strony internetowej
  
  #zapisanie linku i-tej strony
link <- paste("https://www.otomoto.pl/osobowe/miasto_lublin?search%5Border%5D=created_at_first%3Adesc&search%5Bdist%5D=&page=",j,"&search%5Badvanced_search_expanded%5D=true",
              sep="")
page = read_html(link)

#wczytanie nazw wszytkich samochodów z j-tej strony
name = page |>html_nodes(".e1b25f6f0 .er34gjf0 a") |> html_text2()
length(name)

#wczytanie linków do samochodów ze strony (tylko podstrony otomoto.pl)
links = page |> html_nodes(".e1b25f6f0 .er34gjf0 a") |> html_attr("href")
length(links)

#wczytywanie parametrów każdego samochodu z j-tej strony
for(i in 1:length(links)){
  
  page <- read_html(links[i])
  
  #aby pobrać "cena" potrzebujemy innego TAGa
  #pobieramy tylko i wyłącznie cene podaną w zł, a wieć usuwamy dodatkowe znaki białe oraz litery
  cena = page |> html_nodes(".offer-price__number") |> html_text2()
  cena <- parse_number(gsub(" ","",cena[1]))
  
#lista nazw wszytkich pobieranych parametrów
  parametry_nazwy <- page |>html_nodes(".offer-params__label") |> html_text()
  parametry_nazwy <- c(parametry_nazwy,"Cena") 
  
  #pobieramy wszystkie dostępne parametry dla i-tego samochodu z j-tej strony
  parametry <- page |>html_nodes(".offer-params__value") |> html_text2()
  parametry <- c(parametry,cena)

  #ramka danych dla pojedynczego samochodu
  samochod <- data.frame(parametry_nazwy,parametry)
  
  #10 parametrów interesujących nas parametrów    
  samochod <- samochod[
      parametry_nazwy=="Marka pojazdu" |
      parametry_nazwy=="Model pojazdu" |
      parametry_nazwy=="Rok produkcji" |
      parametry_nazwy=="Przebieg" |
      parametry_nazwy=="Pojemność skokowa" |
      parametry_nazwy=="Rodzaj paliwa" |
      parametry_nazwy=="Moc" |
      parametry_nazwy=="Typ nadwozia"|
      parametry_nazwy=="Kolor"|
      parametry_nazwy=="Cena"
    ,]

  #tworzymy wektory  
  id <- c(id,id_i)
  Marka_pojazdu <- c(Marka_pojazdu,samochod$parametry[1])
  Model_pojazdu <- c(Model_pojazdu,samochod$parametry[2])
  Rok_produkcji <- c(Rok_produkcji,samochod$parametry[3])
  Przebieg <- c(Przebieg,samochod$parametry[4])
  Pojemność_skokowa <- c(Pojemność_skokowa,samochod$parametry[5])
  Rodzaj_paliwa <- c(Rodzaj_paliwa,samochod$parametry[6])
  Moc <- c(Moc,samochod$parametry[7])
  Typ_nadwozia <- c(Typ_nadwozia,samochod$parametry[8])
  Kolor <- c(Kolor,samochod$parametry[9])
  Cena <- c(Cena,samochod$parametry[10])
  
  #zwiększamy indeks
  id_i <- id_i+1
}
}

#Ramka danych dla wszystkich samochodów, przy okazji usuwamy pierwsy element z każdego wektora, który jest pusty
samochody <- data.frame(
  id <- id[-1],
  Marka_pojazdu <- Marka_pojazdu[-1],
  Model_pojazdu <- Model_pojazdu[-1],
  Rok_produkcji <- Rok_produkcji[-1],
  Przebieg<- Przebieg[-1],
  Pojemność_skokowa<- Pojemność_skokowa[-1],
  Rodzaj_paliwa<- Rodzaj_paliwa[-1],
  Moc<- Moc[-1],
  Typ_nadwozia<- Typ_nadwozia[-1],
  Kolor<- Kolor[-1],
  Cena<- Cena[-1],
  stringsAsFactors = FALSE
)  
#zmiana nazw kolumn
colnames(samochody) <- c("id",
                         "Marka_pojazdu",
                         "Model_pojazdu",
                         "Rok_produkcji",
                         "Przebieg",
                         "Pojemność_skokowa",
                         "Rodzaj_paliwa",
                         "Moc",
                         "Typ_nadwozia",
                         "Kolor",
                         "Cena")

#zapis utworzonej ramki danych do pliku CSV
write.csv(samochody,
          file = "ścieżka",
          row.names = FALSE)

