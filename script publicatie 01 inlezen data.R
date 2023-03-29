

library(tidyverse)
library(openxlsx)

## stap 0. inlezen functies

source("scripts/script publicatie 00 functies om fouten te checken.R")

### stap 1. inlezen data ---


temp_data  = list.files(pattern="*.xlsx", path= "data/",  full.names = T )

files_data = map(temp_data, read.xlsx)

# toevoegen codes voor deelregio
files_data<- map(files_data, functie_mra_codes)  

temp_data2 <-str_remove(temp_data, " dataset.xlsx")
temp_data2 <-str_remove(temp_data2, "data/")

# toekennen namen aan dataframes
names(files_data)<- temp_data2

# verwijderen spatie bij code
functie_trim<- function(x, var) {
  
  x[[var]]<- str_trim(x[[var]], side="both")
  
  return(x)
}
files_data<-map(files_data, ~ functie_trim(., "gebied_code")  )

# variabelnamen naar kleine letters
functie_to_lower2<- function(x, var) {
  
  x[[var]]<- str_to_lower(x[[var]])
  
  return(x)


}
files_data<-map(files_data,~ functie_to_lower2 (., "variabele"))




set_dtypes <- function(x, var){

  x[[var]]<- as.character(x[[var]])
  
  return(x)
}
files_data<- map(files_data,~ set_dtypes(., "jaar"))  
files_data<- map(files_data,~ set_dtypes(., "peiljaar_gebiedsindeling"))  


# 'gemeente' in gemeenteniveau omzetten naar 'gemeenten'

set_gem <- function(x){
  
  x %>% mutate(gebied_niveau=case_when(gebied_niveau=="gemeente"~ "gemeenten",
                                       gebied_niveau=="Gemeente"~ "gemeenten",
                                       
                                       gebied_niveau=="deelregio"~"deelregios",
                                       gebied_niveau=="Deelregio"~"deelregios",
                                       
                                       gebied_niveau=="COROP"~"corops",
                                       gebied_niveau=="wijk"~"wijken",
                                       gebied_niveau=="Wijk"~"wijken",
                                       
                                       TRUE ~  gebied_niveau))
  

}
files_data<- map(files_data, set_gem)  

save(files_data, file= "output/mra_data_kerncijfers.rds")


### ---
### ---

### filteren: alleen NL, MRA, deelregio's en gemeenten: Deze data voor longitudinale tijdreeks (script 2)---

filter_functie <- function(df) {
  df%>%
    filter(gebied_naam   != 'MRDH',
           gebied_niveau != 'provincie',
           gebied_niveau != 'wijk',
           gebied_niveau != 'buurt'
      
    )
  
}
files_data_def<- map(files_data, filter_functie)  

### stap 2. inlezen metadata ---

temp_meta  = list.files(pattern="*.xlsx", path= "metadata/",  full.names = T )
files_meta = map(temp_meta, read.xlsx)


temp_meta2 <-str_remove(temp_meta,  " metadata.xlsx")
temp_meta2 <-str_remove(temp_meta2, "metadata/")

names(files_meta)<- temp_meta2

# kolomnamen in kleine letters
functie_to_lower <- function(df){

    names(df) <- tolower(names(df))
    
  return(df)

}

files_meta<- map(files_meta,   functie_to_lower)
files_meta<- map(files_meta, ~ functie_to_lower2 (., "variabele"))
files_meta<- map(files_meta, ~ set_dtypes(.,"peildatum"))
files_meta<- map(files_meta, ~ functie_trim(., "variabele"))

### data check ---

foutjes<- map2(files_data_def, files_meta, ~ functie_check(.x, .y)  )

for (i in seq(foutjes))
  write.xlsx(foutjes[[i]],
             paste("fouten/foutjes_",
                   names(foutjes[i]),  ".xlsx", sep= "_"),
             overwrite = T)

save(files_meta, files_data_def, file = "output/mra_data_gemcijfers.RData")

