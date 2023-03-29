library(tidyverse)
library(openxlsx)

### stap 0. inlezen regiocodes ---

mra_gebieden<- read.xlsx("formats/gebieden in de MRA 2022.xlsx")

# gebiednamen
gebied_gemeente    <- unique(mra_gebieden$gebied_naam_peiljaar)
gebied_deelregio   <- unique(mra_gebieden$Deelregio_naam_peiljaar)
gebied_corop <- c("Agglomeratie Haarlem",   
                  "Alkmaar en omgeving",    
                  "Flevoland",              
                  "Groot-Amsterdam",        
                  "Het Gooi en Vechtstreek",
                  "IJmond",                 
                  "Kop van Noord-Holland",  
                  "Zaanstreek")

gebied_totaal      <- c("Nederland","MRA", gebied_gemeente, gebied_deelregio, gebied_corop)


# gebiedcodes
gebied_gem_code    <- str_trim(unique(mra_gebieden$gebied_code_peiljaar), side = "both")
gebied_dr_code     <- c( "AL","AM","AS","GV","IJ","ZW","ZK","MRA","NL")
gebied_cor_code    <- c('CR21','CR19','CR40','CR23','CR24','CR20','CR18','CR22')

gebied_code_totaal <- c(gebied_gem_code, gebied_dr_code, gebied_cor_code)

#### stap 1. voeg landencodes en mracodes toe ---

functie_mra_codes <- function(x) {
  
  x%>%mutate(
    gebied_code=
      case_when(gebied_naam=='Almere-Lelystad'~"AL",
                gebied_naam=='Amstelland-Meerlanden'~"AM",
                gebied_naam=='Amsterdam' & gebied_niveau %in% c('deelregio', 'deelregios')  ~"AS",
                gebied_naam=='Gooi en Vechtstreek'~"GV",
                gebied_naam=='IJmond'& gebied_niveau %in% c('deelregio', 'deelregios') ~"IJ",
                gebied_naam=='Zaanstreek-Waterland'~"ZW",
                gebied_naam=='Zuid-Kennemerland'~"ZK",
                gebied_naam=='MRA'~"MRA",
                gebied_naam=='Nederland'~"NL",
                TRUE ~gebied_code)
  )
  
}

### stap 2. check op dubbelen, gebiednaam en gebiedcode en variabelen ---

functie_duplicates <- function (x) {
  
   y<- x %>%
  select( variabele,
          gebied_code,
          gebied_naam,
          gebied_niveau,
          peiljaar_gebiedsindeling,
          jaar )%>%
  filter(duplicated(.))
  
  if (nrow(y) == 0){
    
    print ("er zijn geen duplicates")
    
  } else {
    
    print ("waarschuwing, er zijn duplicates")
    
    return(y)
  }
  
  
}

functie_gebiednaam <- function(x) {
  
  y<- x %>%
    filter(!gebied_naam  %in% gebied_totaal)
  
  if (nrow(y) == 0){
    
    print ("alle gebiedsnamen kloppen")
    
  } else {
    
    print ("waarschuwing, er staan onbekende gebiednamen in!")
    
    return(y) } 
  
  
  }

functie_gebiedcode <- function(x) {
  
  y<- x %>%
    filter(!gebied_code  %in% gebied_code_totaal)
  
  if (nrow(y) == 0){
    
    print ("alle gebiedscodes kloppen")
    
  } else {
    
    print ("waarschuwing, er staan onbekende gebiedcodes in!")
    
    return(y) 
    } 
  
  
}


# deze functie checkt of Amsterdam DR AS heeft als code en Amsterdam gem GM0363 
functie_ams <- function(x) {
  
  y<- x %>%
     filter(gebied_naam=='Amsterdam', 
            gebied_niveau =='deelregio',
            gebied_code == 'GM0363')
    
  z <-x %>%
    filter(gebied_naam=='Amsterdam', 
           gebied_niveau =='gemeenten',
           gebied_code == 'AS')
    
    fout<- bind_rows(y,z)
  
  
  if (nrow(y) == 0 & nrow(z) == 0 ){
    
    print ("codering Amsterdam klopt")
    
  } else {
    
    print ("waarschuwing, codering Amsterdam klopt niet") 
    
    return(fout)
    } 
  
  
}

functie_varcheck   <- function(x, meta) {
  
  
  vars_meta <- meta[["variabele"]]
  
  vars_data <- unique(x[["variabele"]])
  
  y<-setdiff(vars_data, vars_meta )

  
  
  if (length(y) == 0 ){
    
    print ("variabelnamen komen overeen")
    
  } else  {
    
    print ("waarschuwing, niet alle variabelen zitten in de metafile")
    
    } 
  
  return(y)

}

functie_check      <- function(data, meta) {
  

   a <- functie_duplicates(data)
   b <- functie_gebiednaam(data)
   c <- functie_gebiedcode(data)
   d <- functie_varcheck(data, meta)
   e <- functie_ams(data)
   
   list <- list(a, b,c, d, e )
   names(list) <- c("duplicates", "gebiednamen", "gebiedcodes", "variabelen", "amsterdam")
   
   return(list)
  
}

