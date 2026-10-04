options(stringsAsFactors = FALSE)

library(dplyr)
library(lubridate)
library(stringr)
library(tibble)

set.seed(123)

# ==========================================
# DATY
# ==========================================

daty <- seq.Date(
  from = as.Date("2022-01-01"),
  to   = as.Date("2025-06-30"),
  by   = "day"
)

# ==========================================
# INFLACJA
# ==========================================

ceny_rok <- tibble(
  rok = 2022:2025,
  mnoznik = c(1.00, 1.08, 1.15, 1.22)
)

# ==========================================
# PRODUKTY
# ==========================================

produkty <- tibble(
  produkt = c(
    "Róża Czerwona","Róża Biała",
    "Goździk Czerwony","Goździk Biały",
    "Tulipan","Lilia","Gerbera","Słonecznik","Frezja",
    "Storczyk","Monstera","Fikus","Skrzydłokwiat",
    "Bukiet Ślubny","Bukiet Romantyczny",
    "Wianek","Doniczka","Kosz Wiklinowy","Gąbka",
    "Wstążka",
    "Chryzantema Biała","Chryzantema Żółta"
  )
) %>%
  mutate(
    
    grupa = case_when(
      grepl("Róża|Goździk|Tulipan|Lilia|Gerbera|Słonecznik|Frezja|Chryzantema", produkt) ~ "Kwiaty Cięte",
      grepl("Bukiet", produkt) ~ "Bukiety",
      grepl("Monstera|Fikus|Skrzydłokwiat|Storczyk", produkt) ~ "Rośliny",
      TRUE ~ "Dekoracje"
    ),
    
    gatunek = case_when(
      grepl("Róża", produkt) ~ "Róża",
      grepl("Goździk", produkt) ~ "Goździk",
      grepl("Tulipan", produkt) ~ "Tulipan",
      grepl("Lilia", produkt) ~ "Lilia",
      grepl("Gerbera", produkt) ~ "Gerbera",
      grepl("Słonecznik", produkt) ~ "Słonecznik",
      grepl("Frezja", produkt) ~ "Frezja",
      grepl("Storczyk", produkt) ~ "Storczyk",
      grepl("Monstera", produkt) ~ "Monstera",
      grepl("Fikus", produkt) ~ "Fikus",
      grepl("Skrzydłokwiat", produkt) ~ "Skrzydłokwiat",
      grepl("Chryzantema", produkt) ~ "Chryzantema",
      grepl("Bukiet", produkt) ~ "Bukiet",
      TRUE ~ "Dekoracja"
    ),
    
    sezonowosc = ifelse(
      gatunek %in% c("Róża","Goździk"),
      "caloroczny",
      "sezonowy"
    ),
    
    kraj_pochodzenia = case_when(
      gatunek %in% c("Róża","Goździk","Tulipan","Chryzantema","Gerbera","Frezja") ~ "Holandia",
      TRUE ~ "Polska"
    ),
    
    jednostka = "szt",
    
    cena_zakupu = case_when(
      gatunek == "Róża" ~ 2.5,
      gatunek == "Goździk" ~ 1.5,
      gatunek == "Chryzantema" ~ 1.2,
      grupa == "Bukiety" ~ 8,
      grupa == "Dekoracje" ~ 0.8,
      TRUE ~ 1.8
    ),
    
    cena_sprzedazy = round(cena_zakupu * runif(n(), 1.3, 1.9), 2)
  )

# ==========================================
# DOSTAWCY
# ==========================================

dostawcy <- tibble(
  nazwa_firmy = c(
    "Dutch Flowers BV","Polskie Tulipany","Flora Holland",
    "Green Garden Polska","Tropical Flowers Ltd",
    "Rose Export Group","Flower Market Polska",
    "Premium Floral Import"
  ),
  kraj = c("Holandia","Polska","Holandia","Polska","Tajlandia","Kolumbia","Polska","Holandia")
)

# ==========================================
# KLIENCI
# ==========================================

klienci <- tibble(
  nazwa_klienta = c(
    "Róża i Fiołek Warszawa","Kwiaciarnia Stokrotka Kraków","Flora Art Gdańsk",
    "Orchidea Wrocław","Zielony Zakątek Poznań","Bukiet Marzeń Warszawa",
    "Kwiatowa Przystań Kraków","La Fleur Gdańsk","Studio Florystyczne Wrocław",
    "Kwiaty u Ani Poznań","Kwiaciarnia Magnolia Warszawa","Flower House Kraków",
    "Zielona Lilia Gdańsk","Rose Studio Wrocław","Kwiatowa Galeria Poznań",
    "Ogród Kwiatów Warszawa","Flora Boutique Kraków","Kwiaty & Styl Gdańsk",
    "Floristic Design Wrocław","Bloom Studio Poznań","Kwiatowy Świat Warszawa",
    "Gardenia Kraków","Studio Róża Gdańsk","Kwiaty Premium Wrocław",
    "Flower Express Poznań","Kwiaciarnia Zielona Warszawa","Kwiatowy Zakątek Kraków",
    "Florystyczny Raj Gdańsk","Royal Flowers Wrocław","Kwiatowa Pasja Poznań"
  ),
  miasto = rep(c("Warszawa","Kraków","Gdańsk","Wrocław","Poznań"),6)[1:30],
  wojewodztwo = rep(c("Mazowieckie","Małopolskie","Pomorskie","Dolnośląskie","Wielkopolskie"),6)[1:30]
)

# ==========================================
# PRACOWNICY (NAPRAWIONE)
# ==========================================

pracownicy <- tibble(
  imie = c("Anna","Piotr","Maria","Jan","Michał","Kasia"),
  nazwisko = c("Nowak","Kowalski","Wiśniewski","Wójcik","Kamiński","Zielińska"),
  wiek = sample(25:55, 6, replace = TRUE)
) %>%
  mutate(
    pracownik = paste(imie, nazwisko)
  ) %>%
  select(pracownik, imie, nazwisko, wiek)

# ==========================================
# WAGI PRODUKTÓW
# ==========================================

produkty$waga <- ifelse(
  produkty$gatunek %in% c("Róża","Goździk"), 6,
  ifelse(produkty$gatunek == "Chryzantema", 3, 1)
)

produkty$waga <- produkty$waga / sum(produkty$waga)

losuj_produkt <- function(){
  idx <- sample(seq_len(nrow(produkty)), 1, prob = produkty$waga)
  produkty[idx, ]
}

# ==========================================
# SEZONOWOŚĆ
# ==========================================

waga_dnia <- function(dzien){
  
  m <- month(dzien)
  d <- day(dzien)
  
  base <- case_when(
    m == 2 ~ 2.5,
    m == 3 ~ 1.5,
    m == 5 ~ 1.3,
    m == 9 ~ 1.4,
    m == 10 ~ 1.6,
    m == 11 ~ 1.7,
    m == 12 ~ 2.0,
    TRUE ~ 1
  )
  
  wal <- ifelse(m == 2 & d >= 7 & d <= 14, 3, 1)
  
  base * wal
}

# ==========================================
# GENEROWANIE DANYCH
# ==========================================

gen_year <- function(year, daty, n){
  
  daty_rok <- daty[year(daty) == year]
  
  w <- sapply(daty_rok, waga_dnia)
  w <- w / sum(w)
  
  wybrane <- sample(daty_rok, n, replace = TRUE, prob = w)
  wybrane <- sort(wybrane)
  
  mult <- ceny_rok$mnoznik[ceny_rok$rok == year]
  
  out <- vector("list", n)
  i <- 1
  
  while(i <= n){
    
    dzien <- wybrane[i]
    m <- month(dzien)
    jesien <- m %in% c(9,10,11)
    
    prod <- losuj_produkt()
    
    if(prod$gatunek == "Chryzantema" && !jesien){
      next
    }
    
    if(prod$gatunek == "Chryzantema"){
      ilosc_num <- sample(200:900, 1)
    } else if(prod$sezonowosc == "caloroczny"){
      ilosc_num <- sample(120:700, 1)
    } else {
      ilosc_num <- sample(50:400, 1)
    }
    
    if(ilosc_num <= 0) next
    
    ilosc <- paste(ilosc_num, prod$jednostka)
    
    cena_rok <- prod$cena_sprzedazy * mult
    
    rabat <- sample(c(0,0,5,10,15),1)
    
    kwota <- round(ilosc_num * cena_rok * (1 - rabat/100), 2)
    
    out[[i]] <- tibble(
      nr_faktury = paste0("FV/", year, "/", str_pad(i, 6, side = "left", pad = "0")),
      data_sprzedazy = format(dzien,"%d-%m-%Y"),
      
      produkt = prod$produkt,
      gatunek = prod$gatunek,
      grupa = prod$grupa,
      sezonowosc = prod$sezonowosc,
      
      ilosc = ilosc,
      
      cena_zakupu = prod$cena_zakupu,
      cena_sprzedazy = prod$cena_sprzedazy,
      cena_sprzedazy_rok = round(cena_rok,2),
      
      rabat = rabat,
      kwota = kwota,
      
      waluta = "PLN",
      
      klient = sample(klienci$nazwa_klienta,1),
      miasto = sample(klienci$miasto,1),
      wojewodztwo = sample(klienci$wojewodztwo,1),
      
      pracownik = sample(pracownicy$pracownik,1),
      
      dostawca = sample(dostawcy$nazwa_firmy,1)
    )
    
    i <- i + 1
  }
  
  bind_rows(out)
}

# ==========================================
# LOSOWA LICZBA FAKTUR (TAKTOWNIE)
# ==========================================

sprzedaz <- bind_rows(
  gen_year(2022, daty, sample(3000:4500, 1)),
  gen_year(2023, daty, sample(3500:4750, 1)),
  gen_year(2024, daty, sample(4000:4750, 1)),
  gen_year(2025, daty, sample(2050:2500, 1))
)

nrow(sprzedaz)


# ==========================================
# ZAPIS DO EXCEL (WIELE ARKUSZY)
# ==========================================

library(openxlsx)

# jeśli nie masz:
# install.packages("openxlsx")

# ==========================================
# TWORZENIE WORKBOOKA
# ==========================================

wb <- createWorkbook()

# ==========================================
# ARKUSZ 1 - SPRZEDAŻ
# ==========================================

addWorksheet(wb, "sprzedaz")
writeData(wb, "sprzedaz", sprzedaz)

# ==========================================
# ARKUSZ 2 - PRODUKTY
# ==========================================

addWorksheet(wb, "produkty")
writeData(wb, "produkty", produkty)

# ==========================================
# ARKUSZ 3 - DOSTAWCY
# ==========================================

addWorksheet(wb, "dostawcy")
writeData(wb, "dostawcy", dostawcy)

# ==========================================
# ARKUSZ 4 - KLIENCI
# ==========================================

addWorksheet(wb, "klienci")
writeData(wb, "klienci", klienci)

# ==========================================
# ARKUSZ 5 - PRACOWNICY
# ==========================================

addWorksheet(wb, "pracownicy")
writeData(wb, "pracownicy", pracownicy)

# ==========================================
# ZAPIS PLIKU
# ==========================================

saveWorkbook(
  wb,
  file = "hurtownia_kwiatow.xlsx",
  overwrite = TRUE
)






