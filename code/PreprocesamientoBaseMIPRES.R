#### Preámbulo
sourceDir <- getSrcDirectory(function(x){x})
setwd(sourceDir)
rm(list=ls())

ldInsPk <- function(pk){ # Descargar paquete si no está instalado y luego cargarlo
  if (!pk %in% installed.packages()) install.packages(pk)
  require(pk,character.only = TRUE)
}

ldInsPk("data.table")
ldInsPk("tictoc")
ldInsPk("gsubfn")
ldInsPk("stringr")
ldInsPk("readr")
cat("\f")
tic()

cat("¿Desea preprocesar la base de datos MIPRES completa?\n",
    "Este proceso puede tomar varias horas.\n",
    "Indique \"s\" si la base de datos ha sido actualizada.\n",
    "Se borrarán todos los archivos intermedios y reportes generados previamente.\n",
    "El archivo con la base debe ser \'Medicamentos.txt\' en la carpeta actual.\n\n")
borrarTodo <- readline(prompt="Borrar archivos y preprocesar la base completa (s/n): ")
borrarTodo <- tolower(borrarTodo)

if(borrarTodo=="s"){
  cat("Borrando toda la estructura de archivos... \n\n")
  unlink(list.files("PorPA"))
  unlink(list.files("PorDX"))
  unlink(list.files("Reportes DX/TablasDCI"))
  unlink(list.files("Reportes DX"))
  unlink(list.files("Reportes Principios Activos/TablasDCIxDX"))
  unlink(list.files("Reportes Principios Activos"))
  unlink("indices.RData")
  unlink("PpiosXPres.RData")
  unlink("PRSCRPS.RData")
  unlink("resumenDepEPS.RData")
} else {
  cat("Usando la estructura de archivos existente.\n\n")
}

browser()

#### Carga Base de Datos Original
cat("Cargando Base de Datos ...\n")
if(!file.exists("PRSCRPS.RData")){
  tic()
  cat("No existe archivo pre-procesado. Cargando. Este proceso puede tomar ~5m ... \n")
  suppressWarnings(prescrps <- read_delim("Medicamentos.txt",delim = "|"))
  prescrps <- as.data.table(prescrps)
  cat("Terminado.\n")
  toc()
  cat("\n")
  
  #### Filtro, únicamente prescripciones en estado "Activo"
  tic()
  cat("Filtrando prescripciones activas ... \n")
  cat("Este proceso tomará unos segundos ... \n")
  prescrps <- prescrps[EstadoPrescripcion == "Activo"]
  cat("Terminado.\n")
  toc()
  cat("\n")
  
  tic()
  cat("Comprimiendo y guardando archivo ... \n")
  cat("Este proceso puede tomar ~6m ... \n")
  save(prescrps,file="PRSCRPS.RData")
  cat("Terminado.\n")
  toc()
  cat("\n")
} else {
  tic()
  cat("Cargando archivo ... \n")
  cat("Este proceso puede tomar ~2m ... \n")
  load("PRSCRPS.RData")
  cat("Terminado.\n")
  toc()
  cat("\n")
}


#### Carga de índices de prescripciones por DX Principal y Principio Activo
if(!file.exists("indices.RData")){
  cat("No existe archivo de índices por DX y Principio Activo. Generándolo ... \n")
  
  tic()
  cat("Identificando códigos CIE10 diagnósticos principales ... \n")
  DXS <- sort(unique(prescrps$CodDXPrincipal))
  cat("Terminado.\n")
  toc()
  cat("\n")
  
  tic()
  cat("Listando prescripciones por diagnóstico principal ...\n")
  cat("Este proceso puede tomar ~8m ... \n")
  ind.DXS <- sapply(DXS,
                    function(x){
                      if(runif(1)<0.1) cat("\r",x,sep="")
                      which(prescrps$CodDXPrincipal==x)
                    })
  
  cat("\nTerminado.\n")
  toc()
  cat("\n")
  
  tic()
  cat("Recuperando principios activos por prescripción ... \n")
  if(!file.exists("PpiosXPres.RData")){
    cat("No existe archivo. Calculando. Este proceso puede tomar ~2h ... \n")
    indx <- c(seq(0,nrow(prescrps),by=5000),nrow(prescrps))
    nindx <- length(indx)-1
    
    PpiosActivPres <- vector("list",nrow(prescrps))
    for(i in 1:nindx){
      cat("\r",i," / ",nindx,sep="")
      PpiosActivPres[(indx[i]+1):(indx[i+1])] <- 
        strapply(prescrps$DescripcionMedicamentoNoPBS[(indx[i]+1):(indx[i+1])],
                 "\\[([^]]*)\\]", backref = -1)
    }
    save(PpiosActivPres,file="PpiosXPres.RData")
    cat("\n")
  } else {
    cat("Cargando archivo ...")
    load("PpiosXPres.RData")
  }
  cat("Terminado.\n")
  toc()
  
  tic()
  cat("Listando prescripciones por principio activo ... \n")
  cat("Este proceso puede tomar ~30m ... \n")
  PPS <- sort(unique(unlist(PpiosActivPres)))
  ind.PPS <- sapply(PPS,
                    function(x){
                      cat("\r",x,rep(" ", 250),sep="")
                      which(str_detect(prescrps$DescripcionMedicamentoNoPBS,fixed(x)))
                    })
  cat("\nTerminado.\n")
  toc()
  save(ind.PPS,ind.DXS,file="indices.RData")
  
} else {
  load("indices.RData")
}

if(!file.exists("resumenDepEPS.RData")){
  tic()
  cat("Calculando estadísticas de la base y distribuciónes globales de prescripciones por departamento y EPS ... \n")
  rawDept <- table(prescrps$DepartamentoIPS)/nrow(prescrps)
  iDept <- which(rawDept>=0.01)
  Depts <- names(rawDept)[iDept]
  totalXdept <- c(rawDept[iDept],sum(rawDept[-iDept]))
  names(totalXdept)[length(iDept)+1] <- "OTROS"
  
  rawEPS <- table(prescrps$NombreEPS)/nrow(prescrps)
  iEPS <- which(rawEPS>=0.01)
  EPSs <- names(rawEPS)[iEPS]
  totalXeps <- c(rawEPS[iEPS],sum(rawEPS[-iEPS]))
  names(totalXeps)[length(iEPS)+1] <- "OTRAS"
  
  totalPrescrps <- nrow(prescrps)
  totalPacientes <- length(unique(prescrps$ID_Paciente))
  totalMedicos <- length(unique(prescrps$ID_Medico))
  
  save(Depts, totalXdept, EPSs, totalXeps,
       totalPrescrps, totalPacientes, totalMedicos,
       file="resumenDepEPS.RData")
  cat("Terminado.\n")
  toc()
} else {
  load("resumenDepEPS.RData")
}


####
toc()