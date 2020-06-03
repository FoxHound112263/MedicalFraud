#### Funciones base para el preprocesamiento de texto.

##########
ldInsPk <- function(pk){ # Descargar paquete si no está instalado y luego cargarlo
  if (!pk %in% installed.packages()) install.packages(pk)
  require(pk,character.only = TRUE)
}

##########
preproctext <- function(x){
  ldInsPk("magrittr")
  x[which(is.na(x))] <- ""
  y <- x %>% 
    iconv(.,from="utf-8",to="ASCII//TRANSLIT") %>%
    gsub("[^[:print:]]", " ", .) %>%
    tolower %>% 
    gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    gsub("[[:space:]]{1,}", " ", .) %>%
    trimws
  return(y)
}

##########
RemoveStopwordsFromText <- function(texto, # texto
                                    swords # términos a remover
){
  sapply(texto, function(x){
    y <- strsplit(x," ",T)[[1]]
    paste(y[!(y%in%swords)],collapse=" ")
  }, USE.NAMES = FALSE
  )
}



