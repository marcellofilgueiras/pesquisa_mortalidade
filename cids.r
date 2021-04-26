# Análise mortalidade por uns trem gástrico - Artigo Rafael
# Autor: Marcello Filgueiras 
# META É anos = (2017201820192020)


library(tidyverse)

#Importação -------------------------------------------------

#https://bvsms.saude.gov.br/bvs/publicacoes/sis_mortalidade.pdf


library(readr)
sistema_mortalidade_ods_2020_raw <- read_csv2("data_raw/dobr2020_opendata.csv")

  
  sistema_mortalidade_ods_2020_raw %>%
  count(CAUSABAS) %>%
  arrange(desc(n)) %>%
  view()

#Tidying ----------------------------------------------------


#Filtros e Classificações -----------------------------------

cids_trens_gastricos <- str_c("K351", "K350", "K359", "K37", "K381", "K382",
                          "K389", "K383", "K380", "Q206", "D121", "D373",
                          "C181", "K388", "K36", sep = "|")%>%
  regex()

cids_trens_gastricos2 <- str_c("K35\\.1", "K35\\.0", "K35\\.9", "K37", "K38\\.1", "K38\\.2",
                              "K38\\.9", "K38\\.3", "K38\\.0", "Q20\\.6", "D12\\.1", "D37\\.3",
                              "C18\\.1", "K38\\.8", "K36", sep = "|")%>%
  regex()

#Cid K80 é a de doenças das vias biliares


criadora_de_cids<- function (x) {
  
   paste(x, c("1","2","3", "4", "5", "6","7","8","9"),
         sep = "", collapse = "|" )
}

doenças = c("K80", "K81", "K82", "K83")

cid_rafael <- purrr::map_chr(doenças, criadora_de_cids)%>%
  str_c(sep = "|", collapse = "|")%>%
  regex()

filtradora_doenças <- function (x) {
  dplyr::filter(str_detect(LINHAA, x)|
           str_detect(LINHAB, x)|
           str_detect(LINHAC, x)|
           str_detect(LINHAD, x)|
           str_detect(LINHAII, x)|
           str_detect(CAUSABAS, x)|
           str_detect(CAUSAMAT, x)|
           str_detect(CAUSABAS_O, x))
}

doenças_rafael2 <- sistema_mortalidade_ods_2020_raw %>%
  filtradora_doenças(x= cid_rafael)


doenças_rafael <- sistema_mortalidade_ods_2020_raw %>%
  filter( str_detect(LINHAA, cid_rafael)|
           str_detect(LINHAB, cid_rafael)|
           str_detect(LINHAC, cid_rafael)|
           str_detect(LINHAD, cid_rafael)|
           str_detect(LINHAII, cid_rafael)|
           str_detect(CAUSABAS, cid_rafael)|
           str_detect(CAUSAMAT, cid_rafael)|
           str_detect(CAUSABAS_O, cid_rafael))

      

trens_gastricos<- sistema_mortalidade_ods_2020_raw%>%
  filter(str_detect(LINHAA, cids_trens_gastricos)|
           str_detect(LINHAB, cids_trens_gastricos)|
           str_detect(LINHAC, cids_trens_gastricos)|
           str_detect(LINHAD, cids_trens_gastricos)|
           str_detect(LINHAII, cids_trens_gastricos)|
         str_detect(CAUSABAS, cids_trens_gastricos)|
           str_detect(CAUSAMAT, cids_trens_gastricos)|
           str_detect(CAUSABAS_O, cids_trens_gastricos)
         )
         
       

#Modeling ---------------------------------------------------


#Visualização -----------------------------------------------


#Exportação -------------------------------------------------