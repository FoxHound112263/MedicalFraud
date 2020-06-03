#### Preámbulo
cat("\f")
sourceDir <- getSrcDirectory(function(x){x})
setwd(sourceDir)
rm(list=ls())
gc()
graphics.off()

ldInsPk <- function(pk){ # Descargar paquete si no está instalado y luego cargarlo
  if (!pk %in% installed.packages()) install.packages(pk)
  require(pk,character.only = TRUE)
}
ldInsPk("data.table")
ldInsPk("tictoc")
ldInsPk("magrittr")


#### Funciones base
source("funciones_base.R")

sw <- readLines("stop_words_spanish.txt")


#### Carga
load("indices.RData")
rm(ind.DXS)

pp <- "PREGABALINA"
ipp <- which(names(ind.PPS)==pp)
ppio <- readline(prompt = "Introduzca Principio Activo (por defecto \"PREGABALINA\"): ")
ppio <- toupper(ppio)
if(any(names(ind.PPS)==ppio)){
  pp <- ppio
  ipp <- which(names(ind.PPS)==pp)[1]
}
rm(ppio)

cat("Principio Activo Seleccionado: ",pp,"\nÍndice en la Lista: ",ipp,"\n",sep="")

fil1 <- paste("PorPA/",sprintf("%04d",ipp),".RData",sep="")
if(!file.exists(fil1)){
  cat("No existe archivo \"",fil1, "\". Generándolo ... \n", sep="")
  
  tic()
  cat("Leyendo base de datos completa. Este proceso puede tomar 2m ... \n")
  load("PRSCRPS.RData")
  load("PpiosXPres.RData")
  cat("Terminado\n")
  toc()
  cat("\n")
  
  ### Base de datos reducida a prescripciones del principio activo en cuestión
  tic()
  cat("Filtrando la base de datos ... \n")
  prescrps <- prescrps[ind.PPS[[ipp]], ]
  PpiosActivPres <- PpiosActivPres[ind.PPS[[ipp]]]
  cat("Terminado\n")
  toc()
  cat("\n")
  
  tic()
  cat("Llevando las justificaciónes a un formato estándar ... \n")
  JustificacionNoPBSLimpia <- preproctext(prescrps$JustificacionNoPBS)
  cat("Terminado\n")
  toc()
  cat("\n")
  
  tic()
  cat("Eliminando \'stopwords\' de las justificaciones ... \n")
  JustificacionNoPBSNoSW <- RemoveStopwordsFromText(JustificacionNoPBSLimpia, sw)
  cat("Terminado\n")
  toc()
  cat("\n")
  
  tic()
  cat("Eliminando \'stopwords\' de la descripción de DCI PBS usados y rechazados... \n")
  DescrRazonesPBSNoSW <- cbind(RemoveStopwordsFromText(preproctext(prescrps$DescripcionRazonCausaS31),sw),
                               RemoveStopwordsFromText(preproctext(prescrps$DescripcionRazonCausaS32),sw),
                               RemoveStopwordsFromText(preproctext(prescrps$DescripcionRazonCausaS41),sw),
                               RemoveStopwordsFromText(preproctext(prescrps$DescripcionRazonCausaS42),sw),
                               RemoveStopwordsFromText(preproctext(prescrps$DescripcionRazonCausaS43),sw),
                               RemoveStopwordsFromText(preproctext(prescrps$DescripcionRazonCausaS44),sw))
  cat("Terminado\n")
  toc()
  cat("\n")
  
  save(prescrps,JustificacionNoPBSLimpia,JustificacionNoPBSNoSW,DescrRazonesPBSNoSW,
       file=fil1)
} else {
  cat("Existe archivo \"",fil1, "\". Cargándolo ... \n", sep="")
  tic()
  load(file=fil1)
  cat("Terminado\n")
  toc()
  cat("\n")
}
rm(sw, fil1, ind.PPS)

####


fechasPr <- format(as.Date(prescrps$FechaPrescripcion,format="%Y-%m-%d"),format="%Y-%m")
todosMeses <- sort(unique(fechasPr))

iFCH <- 1:nrow(prescrps)

porFecha <- readline(prompt = "¿Desea seleccionar las fechas? (s/n): ")
porFecha <- tolower(porFecha)

a <- which(todosMeses == min(todosMeses))
b <- which(todosMeses == max(todosMeses))
sf <- FALSE

if(porFecha=="s"){
  cat("Rango de fechas de prescripciones: \"", min(todosMeses),"\" - \"", max(todosMeses), "\".\n", sep="")
  fIni <- readline(prompt = "Introduzca mes inicial: ")
  fFin <- readline(prompt = "Introduzca mes final: ")
  a <- which(todosMeses==fIni)
  b <- which(todosMeses==fFin)
  if(isTRUE(b>=a)){
    meses <- a:b
    iFCH <- which(fechasPr %in% todosMeses[meses])
    sf <- TRUE
    cat("Filtrando la base ...\n")
  } else {
    cat("Selección incorrecta. Se analizarán todas las prescripciones.\n")
  }
} else {
  cat("Se analizarán todas las prescripciones.\n")
}


####
save(pp,ipp,iFCH,sf,a,b,todosMeses, file="preKnit.RData")

cat("Generando reporte en html ... \n")
if(sf){
  nfilout <- paste(pp," ",todosMeses[a]," - ", todosMeses[b],".html",sep="")
} else {
  nfilout <- paste(pp,".html",sep="")
}

rmarkdown::render("RMDPA.Rmd",encoding = "UTF-8",output_file = nfilout,output_dir = "Reportes Principios Activos",clean = T)

unlink("preKnit.RData")