library(dplyr)
library(reshape2)
library(tidyr)

?dashboardHeader

install.packages("rpivotTable")

# variables de configuracion
directorio.de.trabajo <- "D:/BDAvanzadas/Semana7"
nombre.archivo.denue <- "INEGI_DENUE_ALTOS_SUR.csv"
region.actual <- "ALTOS SUR"
ventana.prediccion <- 15.5
anio.prediccion <- 2030
carrera.especifica <- ""
archivo.mapeo.carreras.l4 <- "mapeocarreral4scian.csv"
mario.molina.relevante <- c("TECMM.Arandas")  # pueden ser varios. 
limite.superior.relevancia <- 2

# datos denue
getwd()
setwd(directorio.de.trabajo)
# load("region.lookup.table.R")
# region.lookup.table
datos.primarios.denue <- read.csv(nombre.archivo.denue,stringsAsFactors = F) # fileEncoding='latin1')
nrow(datos.primarios.denue)
str(datos.primarios.denue)
dim(datos.primarios.denue)
# limpiar los headers.
nombre.headers <- c("id","nombre","razon.social","codigo.scian","nombre.actividad","descripcion.estrato",
                    "tipo.vialidad","nombre.vialidad","tipo.entre.vialidad.1","nombre.entre.vialidad.1",
                    "tipo.entre.vialidad.2","nombre.entre.vialidad.2","tipo.entre.vialidad.3","nombre.entre.vialidad.3",
                    "numero.exterior","letra.exterior","edificio","edificio.piso","numero.interior","letra.interior",
                    "tipo.asentamiento","nombre.asentamiento","tipo.centro.comercial","corredor.industrial.centro.comercial.o.mercado.publico",
                    "numero.local","codigo.postal","clave.entidad","entidad.federativa","clave.municipio","nombre.municipio", "clave.localidad",
                    "nombre.localidad","ageb","manzana","numero.telefono","email","sitio.web","tipo.establecimiento","lat","long", "fecha.incorporacion")
length(nombre.headers)
nombre.headers
head(datos.primarios.denue)
colnames(datos.primarios.denue) <- nombre.headers
str(datos.primarios.denue)

unique(datos.primarios.denue$tipo.establecimiento)

columnas.elegidas <- c("id","nombre","codigo.scian","nombre.actividad","descripcion.estrato", "tipo.asentamiento",
                       "nombre.asentamiento","codigo.postal","clave.municipio","nombre.municipio","clave.localidad",
                       "nombre.localidad", "tipo.establecimiento", "lat", "long", "fecha.incorporacion")
denue.columnas.seleccion <- datos.primarios.denue[,columnas.elegidas]
str(denue.columnas.seleccion)

# asignar potencial demanda por estrato
unique(denue.columnas.seleccion.demanda$descripcion.estrato)
denue.columnas.seleccion.demanda <- denue.columnas.seleccion
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="0 a 5 personas","demanda"] <- 1
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="6 a 10 personas","demanda"] <- 2
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="11 a 30 personas","demanda"] <- 4
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="31 a 50 personas","demanda"] <- 7
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="51 a 100 personas","demanda"] <- 14
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="101 a 250 personas","demanda"] <- 30
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="251 y mas personas","demanda"] <- 50

denue.columnas.seleccion.demanda$subscian <- substr(as.character(denue.columnas.seleccion.demanda$codigo.scian),1,3)
# esta es la columna para hacer el mapeo
denue.columnas.seleccion.demanda$l4 <- substr(as.character(denue.columnas.seleccion.demanda$codigo.scian),1,4)
str(denue.columnas.seleccion.demanda)
head(denue.columnas.seleccion.demanda)
colnames(denue.columnas.seleccion.demanda)
datos.pivote <- denue.columnas.seleccion.demanda[,c("codigo.scian","descripcion.estrato","nombre.municipio",
                                                    "fecha.incorporacion","demanda","subscian","l4")]
datos.pivote$sector <- substr(as.character(datos.pivote$codigo.scian),1,2)
head(datos.pivote)
# Mapeo de nombres a nivel 2 
l2.lookup.table <- read.csv("scianl2.csv", stringsAsFactors = F)
head(l2.lookup.table)
colnames(l2.lookup.table) <- c("codigo.l2", "sector")
save(l2.lookup.table, file="l2.lookup.table.R")
load("l2.lookup.table.R")
l2.lookup.table # dos columnas

l2.lookup <- setNames(as.character(l2.lookup.table$sector), as.character(l2.lookup.table$codigo.l2))
l2.lookup[1]
l2.lookup["43"]
# l2 lookup
resultado.look.up.l2 <- sapply(datos.pivote$sector, function(i) l2.lookup[i])
datos.pivote$nombre.sector <- resultado.look.up.l2
head(datos.pivote)
colnames(datos.pivote)
colnames(datos.pivote) <- c("codigo.scian","estrato","municipio","incorporacion","existencia",
                            "subsector.scian","rama.scian","sector.scian","nombre.sector.scian")
# generar tabla pivote
datos.pivote[1,]

#l3
# Mapeo de nombres a nivel 2 
l3.lookup.table <- read.csv("scianl3.csv", stringsAsFactors = F)
head(l3.lookup.table)
colnames(l3.lookup.table) <- c("codigo.l3", "subsector")
save(l3.lookup.table, file="l3.lookup.table.R")
load("l3.lookup.table.R")
l3.lookup.table # dos columnas

l3.lookup <- setNames(as.character(l3.lookup.table$subsector), as.character(l3.lookup.table$codigo.l3))
l3.lookup[1]
l3.lookup["111"]
# l2 lookup
resultado.look.up.l3 <- sapply(datos.pivote$subsector.scian, function(i) l3.lookup[i])
resultado.look.up.l3[1]
datos.pivote$nombre.subsector <- resultado.look.up.l3
head(datos.pivote)
colnames(datos.pivote)
colnames(datos.pivote) <- c("codigo.scian","estrato","municipio","incorporacion","existencia",
                            "subsector.scian","rama.scian","sector.scian","nombre.sector.scian",
                            "nombre.subsector")
# generar tabla pivote
datos.pivote[1,]

#l4
load("l4.lookup.table.R")
l4.lookup.table # dos columnas
l4.lookup <- setNames(as.character(l4.lookup.table$rama), as.character(l4.lookup.table$codigo.l4))
l4.lookup[1]
l4.lookup["9311"] <- "Organos legislativos"
names(l4.lookup["9311"])
l4.lookup
resultado.look.up.l4 <- sapply(datos.pivote$rama.scian, function(i) l4.lookup[i])
resultado.look.up.l4[1]
datos.pivote$nombre.rama <- resultado.look.up.l4
head(datos.pivote)
colnames(datos.pivote)
colnames(datos.pivote) <- c("codigo.scian","estrato","municipio","incorporacion","existencia",
                            "subsector.scian","rama.scian","sector.scian","nombre.sector.scian",
                            "nombre.subsector","nombre.rama")







rpivotTable(datos.pivote)

## generate function to create the sector area

generando.datos.primarios.pivote <- function(datos.fuente.archivo) {
  datos.primarios.denue <- read.csv(datos.fuente.archivo,stringsAsFactors = F) # fileEncoding='latin1')
  # limpiar los headers.
  nombre.headers <- c("id","nombre","razon.social","codigo.scian","nombre.actividad","descripcion.estrato",
                      "tipo.vialidad","nombre.vialidad","tipo.entre.vialidad.1","nombre.entre.vialidad.1",
                      "tipo.entre.vialidad.2","nombre.entre.vialidad.2","tipo.entre.vialidad.3","nombre.entre.vialidad.3",
                      "numero.exterior","letra.exterior","edificio","edificio.piso","numero.interior","letra.interior",
                      "tipo.asentamiento","nombre.asentamiento","tipo.centro.comercial","corredor.industrial.centro.comercial.o.mercado.publico",
                      "numero.local","codigo.postal","clave.entidad","entidad.federativa","clave.municipio","nombre.municipio", "clave.localidad",
                      "nombre.localidad","ageb","manzana","numero.telefono","email","sitio.web","tipo.establecimiento","lat","long", "fecha.incorporacion")
  colnames(datos.primarios.denue) <- nombre.headers
  columnas.elegidas <- c("id","nombre","codigo.scian","nombre.actividad","descripcion.estrato", "tipo.asentamiento",
                         "nombre.asentamiento","codigo.postal","clave.municipio","nombre.municipio","clave.localidad",
                         "nombre.localidad", "tipo.establecimiento", "lat", "long", "fecha.incorporacion")
  denue.columnas.seleccion <- datos.primarios.denue[,columnas.elegidas]
  # asignar potencial demanda por estrato
  denue.columnas.seleccion.demanda <- denue.columnas.seleccion
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="0 a 5 personas","demanda"] <- 1
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="6 a 10 personas","demanda"] <- 2
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="11 a 30 personas","demanda"] <- 4
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="31 a 50 personas","demanda"] <- 7
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="51 a 100 personas","demanda"] <- 14
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="101 a 250 personas","demanda"] <- 30
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="251 y mas personas","demanda"] <- 50
  denue.columnas.seleccion.demanda$subscian <- substr(as.character(denue.columnas.seleccion.demanda$codigo.scian),1,3)
  # esta es la columna para hacer el mapeo
  denue.columnas.seleccion.demanda$l4 <- substr(as.character(denue.columnas.seleccion.demanda$codigo.scian),1,4)
  datos.pivote <- denue.columnas.seleccion.demanda[,c("codigo.scian","descripcion.estrato","nombre.municipio",
                                                      "fecha.incorporacion","demanda","subscian","l4")]
  datos.pivote$sector <- substr(as.character(datos.pivote$codigo.scian),1,2)
  load("l2.lookup.table.R")
  l2.lookup <- setNames(as.character(l2.lookup.table$sector), as.character(l2.lookup.table$codigo.l2))
  resultado.look.up.l2 <- sapply(datos.pivote$sector, function(i) l2.lookup[i])
  datos.pivote$nombre.sector <- resultado.look.up.l2
  colnames(datos.pivote) <- c("codigo.scian","estrato","municipio","incorporacion","existencia",
                              "subsector.scian","rama.scian","sector.scian","nombre.sector.scian")
  load("l3.lookup.table.R")
  l3.lookup <- setNames(as.character(l3.lookup.table$subsector), as.character(l3.lookup.table$codigo.l3))
  resultado.look.up.l3 <- sapply(datos.pivote$subsector.scian, function(i) l3.lookup[i])
  datos.pivote$nombre.subsector <- resultado.look.up.l3
  colnames(datos.pivote) <- c("codigo.scian","estrato","municipio","incorporacion","existencia",
                              "subsector.scian","rama.scian","sector.scian","nombre.sector.scian",
                              "nombre.subsector")
  load("l4.lookup.table.R")
  l4.lookup <- setNames(as.character(l4.lookup.table$rama), as.character(l4.lookup.table$codigo.l4))
  #l4.lookup["9311"] <- "Organos legislativos"
  resultado.look.up.l4 <- sapply(datos.pivote$rama.scian, function(i) l4.lookup[i])
  datos.pivote$nombre.rama <- resultado.look.up.l4
  head(datos.pivote)
  colnames(datos.pivote) <- c("codigo.scian","estrato","municipio","incorporacion","existencia",
                              "subsector.scian","rama.scian","sector.scian","nombre.sector.scian",
                              "nombre.subsector.scian","nombre.rama.scian")
  
  datos.pivote <- datos.pivote[,c("municipio","incorporacion","nombre.sector.scian",
                               "nombre.subsector.scian","nombre.rama.scian","existencia")]
  return(datos.pivote)
}
save(generando.datos.primarios.pivote,file="generando.datos.primarios.pivote.R")
load("generando.datos.primarios.pivote.R")

datos.primarios.pivote <- generando.datos.primarios.pivote(denue.relevante("ALTOS SUR"))
rpivotTable(datos.primarios.pivote)

# MAPEO :: CARRERA :: SCIAN 4 :: archivo mapeo
getwd()
carreras.l4.data <- read.csv(archivo.mapeo.carreras.l4,stringsAsFactors = F)
colnames(carreras.l4.data)
carreras.l4.data[1,]

# TODO: crear un vector con las carreras relevantes a esa region. 
carreras.relevantes.a.la.region <- carreras.l4.data[,mario.molina.relevante]
nombre.carreras <- carreras.l4.data[,1]
# para la version 1 usaremos I. Electromecanica
indice.carrera <- 7
vector.de.trabajo <- carreras.l4.data[indice.carrera,]

names(vector.de.trabajo[19])
## definción de la carrera. 
carrera.actual.nombre <- carreras.l4.data[indice.carrera,"Carrera"]
carreras.l4.data$Carrera
colnames(carreras.l4.data)

codigos.relevantes.a.la.carrera <- function(vec.trab) {
  cod.rel.vec <- vector()
  for (i in 19:length(vec.trab)) {
    # print(vec.trab[i])
    if (vec.trab[i] >= limite.superior.relevancia) {
      cod.rel.vec <- c(cod.rel.vec,substring(names(vec.trab[i]),2))
    }
  }
  return(cod.rel.vec)
}

save(codigos.relevantes.a.la.carrera, file="codigos.relevantes.a.la.carrera.R")
load("codigos.relevantes.a.la.carrera.R")

cod.rel.vec.tra <- codigos.relevantes.a.la.carrera(vector.de.trabajo)
cod.rel.vec.tra

# vector.de.trabajo nombrado
getwd()
#l4.lookup.table <- read.csv("l4lookuptable.csv", stringsAsFactors = F)
#head(l4.lookup.table)
#save(l4.lookup.table, file="l4.lookup.table.R")
load("l4.lookup.table.R")
l4.lookup.table # dos columnas

l4.lookup <- setNames(as.character(l4.lookup.table$rama), as.character(l4.lookup.table$codigo.l4))
l4.lookup[1]
l4.lookup["1111"]

resultado.look.up <- data.frame(sapply(cod.rel.vec.tra, function(i) l4.lookup[i]))
class(resultado.look.up)
colnames(resultado.look.up) <- c("rama")
nrow(resultado.look.up)

# seleccion de datos relevantes de denue para prediccion.
denue.para.l4 <- denue.columnas.seleccion.demanda
nrow(denue.para.l4)
denue.14.sel <- denue.para.l4[denue.para.l4$l4 %in% cod.rel.vec.tra,]
nrow(denue.14.sel)
getwd()
## AQUI
save(denue.14.sel,file="denue.14.sel.R")

denue.14.sel$year <- substr(as.character(denue.14.sel$fecha.incorporacion),1,4)
unique(denue.14.sel$year)
# parece haber datos desde el 2010, en altos sur, prospectiva, 2010-2019
head(denue.14.sel)
denue.14.sel$region <- region.actual
denue.14.sel$carrera <- carrera.actual.nombre

prospectiva.l4.t <- data.frame()
prospectiva.l4.t <- rbind(prospectiva.l4.t, denue.14.sel)
save(prospectiva.l4.t,file="prospectiva.l4.t.R")
prospectiva.l4.t$carrera

# Funcion auxiliar para preparar la interpolacion
prepare.interp <- function(data) {
  final.vector <- vector()
  for (i in 2010:2019) {
    if (nrow(data[data$year==as.character(i),])==0)
      final.vector[i-2009] <- NA
    else
      final.vector[i-2009] <- data[data$year==as.character(i),]$total.prospectiva
  }
  return(final.vector)
}

## prediction function
genera.datos.prediccion.l4 <- function(datos.melt, filtro, subsector, region) {
  # filtrado inicial
  # filtrado de datos.melt
  prospectiva.vocacionamiento <- datos.melt[datos.melt$region==region,]
  if (subsector == F) {
    prospectiva.vocacionamiento <- prospectiva.vocacionamiento[prospectiva.vocacionamiento$carrera==filtro,]
  } else {
    prospectiva.vocacionamiento <- prospectiva.vocacionamiento[prospectiva.vocacionamiento$subsector.scian==filtro,]
  }
  prospectiva.sector <- prospectiva.vocacionamiento %>%
    group_by(year) %>%
    summarise(total.prospectiva = sum(demanda))
  # 2020 tiene datos incompletos
  prospectiva.sector <- data.frame(prospectiva.sector)
  prospectiva.sector <- prospectiva.sector[prospectiva.sector$year!=2020,] 
  # TODO: create lookup table
  prospectiva.sector$region <- region
  prospectiva.sel <- prospectiva.sector[order(prospectiva.sector$year),c("year","total.prospectiva")]
  prospectiva.aux <- prepare.interp(prospectiva.sel)
  # interpolar
  prospectiva.int <- approx(prospectiva.aux)
  prospectiva.int <- data.frame(prospectiva.int)
  # modelo lineal sector/subsector
  lm.prospectiva <- lm(y ~ x, prospectiva.int)
  # gr??fica, N data points en el futuro
  nrow(prospectiva.int)
  tiempo.nuevo <- seq(10.1, ventana.prediccion, by=.1)
  tiempo.prediccion <- data.frame(x=tiempo.nuevo)
  demanda.prediccion <- predict(lm.prospectiva, newdata = tiempo.prediccion)
  #auxiliar para identificar los valores reales y los pronosticados
  style <- c(rep(1,nrow(prospectiva.int)), rep(4, length(demanda.prediccion)))
  prospectiva.full.data <- c(prospectiva.int$y,demanda.prediccion)
  # empaca los datos en un data.frame
  prediccion.ready.df <- data.frame(x=rep(2010:anio.prediccion,1,each=5),y=prospectiva.full.data,
                                    style=style)
  return(prediccion.ready.df)
}
pred.data <- genera.datos.prediccion.l4(prospectiva.l4.t,carrera.actual.nombre,F,"ALTOS SUR")

pred.data <- genera.datos.prediccion.l4(prospectiva.l4.t,"Ingenieria Electromecanica",F,"ALTOS SUR")
# plot
plot(pred.data$y, xaxt="n", ylab="Demanda", xlab="", pch = pred.data$style, col = pred.data$style)
axis(1, labels=pred.data$x, at=1:nrow(pred.data), las=3)

### auxiliar para tablero
regiones.master
getwd()
save(regiones.master, file="regiones.master.R")
load("regiones.master.R")

seleccion.region <- regiones.master[3]
# extraer carreras relevantes por region

mario.molina.relevante <- function(region) {
  case_when(
    region == "ALTOS SUR" ~ c("TECMM.Arandas"),
    region == "COSTA SUR" ~ c("TECMM.La.Huerta"),
    region == "COSTA SIERRA OCCIDENTAL" ~ c("TECMM.Puerto.Vallarta","TECMM.Mascota"),
    region == "NORTE" ~ c("TECMM.Tala"),
    region == "VALLES" ~ c("TECMM.Tala"),
    region=="SUR"~c("TECMM.Tamazula"),
    region=="SURESTE"~c("TECMM.Chapala"),
    region == "ALTOS NORTE" ~ c("TECMM.Lagos.de.Moreno"),
    region == "CENTRO" ~ c("TECMM.Zapopan", "TECMM.Zapotlanejo"),
    TRUE ~ c("INDEFINIDO")
  )
}

mario.molina.relevante("CENTRO")

#FALTAN dos de la región centro: "UTZMG","ITTJ.TECNM.Tlajomulco"

denue.relevante <- function(region) {
  case_when(
    region == "ALTOS SUR" ~ "INEGI_DENUE_ALTOS_SUR.csv",
    region == "COSTA SUR" ~ c("INEGI_DENUE_COSTA_SUR.csv"),
    region == "COSTA SIERRA OCCIDENTAL" ~ "DENUE_REGION_COSTA_SIERRA_OCCIDENTAL.csv",
    region == "NORTE" ~ "denue.norte.csv",
    region=="SUR"~"INEGI_DENUE_SUR.csv",
    region=="VALLES"~"INEGI_DENUE_VALLES.csv",
    region=="SURESTE"~"INEGI_DENUE_SURESTE.csv",
    region == "ALTOS NORTE" ~ "INEGI_DENUE_ALTOS_NORTE.csv",
    region == "CENTRO" ~ "INEGI_DENUE_CENTRO.csv",
    TRUE ~ c("INDEFINIDO")
  )
}






region.objetivo 


mario.molina.relevante("ALTOS SUR")
denue.relevante("ALTOS SUR")
save(mario.molina.relevante,file="mario.molina.relevante.R")
save(denue.relevante,file="denue.relevante.R")

# NOW: get the carrer names to populate the combobox. 
listado.carreras.por.centros <- function(tecnologicos) {
  vector.resultado <- vector()
  if (tecnologicos[1] == "INDEFINIDO") return(vector.resultado)
  for (i in 1:length(tecnologicos)) {
    tecnologico.actual <- tecnologicos[i]
    carreras.relevantes.a.la.region <- carreras.l4.data[,tecnologico.actual]
    for(i in 1:length(carreras.relevantes.a.la.region)) {
      if (carreras.relevantes.a.la.region[i]=="P")
        vector.resultado <- c(vector.resultado, carreras.l4.data[i,1])
    }
  }
  return(vector.resultado)
}

listado.carreras.por.centros(mario.molina.relevante("ALTOS SUR"))
listado.carreras.por.centros(mario.molina.relevante("COSTA SIERRA OCCIDENTAL"))
carreras.l4.data[,mario.molina.relevante("COSTA SIERRA OCCIDENTAL")]
carreras.l4.data[,mario.molina.relevante("ALTOS SUR")]


save(listado.carreras.por.centros, file="listado.carreras.por.centros.R")


load("mario.molina.relevante.R")
load("denue.relevante.R")
load("listado.carreras.por.centros.R")

carreras.relevantes.a.la.region <- carreras.l4.data[,mario.molina.relevante]



#### AUX :: mostrar cómo se hacer la predicción. 
datos.melt <- prospectiva.l4.t
prospectiva.vocacionamiento <- datos.melt[datos.melt$region=="ALTOS SUR",]
if (subsector == F) {
  prospectiva.vocacionamiento <- prospectiva.vocacionamiento[prospectiva.vocacionamiento$carrera=="Ingenier\303\255a Electromec\303\241nica",]
} else {
  prospectiva.vocacionamiento <- prospectiva.vocacionamiento[prospectiva.vocacionamiento$subsector.scian==filtro,]
}
nrow(datos.melt)
nrow(prospectiva.vocacionamiento)
prospectiva.sector <- prospectiva.vocacionamiento %>%
  group_by(year) %>%
  summarise(total.prospectiva = sum(demanda))
# 2020 tiene datos incompletos
prospectiva.sector <- data.frame(prospectiva.sector)
prospectiva.sector <- prospectiva.sector[prospectiva.sector$year!=2020,] 
# TODO: create lookup table
prospectiva.sector$region <- "ALTOS SUR"
prospectiva.sel <- prospectiva.sector[order(prospectiva.sector$year),c("year","total.prospectiva")]
prospectiva.aux <- prepare.interp(prospectiva.sel)
# interpolar
prospectiva.int <- approx(prospectiva.aux)
prospectiva.int <- data.frame(prospectiva.int)
# modelo lineal sector/subsector
lm.prospectiva <- lm(y ~ x, prospectiva.int)
# gr??fica, N data points en el futuro
nrow(prospectiva.int)
tiempo.nuevo <- seq(10.1, ventana.prediccion, by=.1)
tiempo.prediccion <- data.frame(x=tiempo.nuevo)
demanda.prediccion <- predict(lm.prospectiva, newdata = tiempo.prediccion)
#auxiliar para identificar los valores reales y los pronosticados
style <- c(rep(1,nrow(prospectiva.int)), rep(4, length(demanda.prediccion)))
prospectiva.full.data <- c(prospectiva.int$y,demanda.prediccion)
# empaca los datos en un data.frame
prediccion.ready.df <- data.frame(x=rep(2010:anio.prediccion,1,each=5),y=prospectiva.full.data,
                                  style=style)




vector.resultado <- vector()
tecnologico.actual <- mario.molina.relevante("ALTOS SUR")[1]
carreras.relevantes.a.la.region <- carreras.l4.data[,tecnologico.actual]
for(i in 1:length(carreras.relevantes.a.la.region)) {
  if (carreras.relevantes.a.la.region[i]=="P")
    vector.resultado <- c(vector.resultado, carreras.l4.data[i,1])
}

# Dashboard functions

ramas.relevantes.a.la.carrera <- function(carrera) {
  carreras.l4.data <- read.csv(archivo.mapeo.carreras.l4,stringsAsFactors = F)
  vector.de.trabajo <- carreras.l4.data[carreras.l4.data$Carrera==carrera,]
  cod.rel.vec.tra <- codigos.relevantes.a.la.carrera(vector.de.trabajo)
  load("l4.lookup.table.R")
  l4.lookup <- setNames(as.character(l4.lookup.table$rama), as.character(l4.lookup.table$codigo.l4))
  resultado.look.up <- data.frame(sapply(cod.rel.vec.tra, function(i) l4.lookup[i]))
  colnames(resultado.look.up) <- c("rama")
  return(resultado.look.up)
}
save(ramas.relevantes.a.la.carrera, file="ramas.relevantes.a.la.carrera.R")
load("ramas.relevantes.a.la.carrera.R")
ramas.relevantes.a.la.carrera("Ingenieria Electromecanica")

### NOW: DATOS PARA LA PREDICCIÓN
datos.para.proyeccion.carrera.region <- function(datos.fuente.archivo, carrera, region) {
  ventana.prediccion <- 15.5
  anio.prediccion <- 2030
  carrera.especifica <- ""
  archivo.mapeo.carreras.l4 <- "mapeocarreral4scian.csv"
  limite.superior.relevancia <- 2
  
  datos.primarios.denue <- read.csv(datos.fuente.archivo,stringsAsFactors = F) # fileEncoding='latin1')
  # limpiar los headers.
  nombre.headers <- c("id","nombre","razon.social","codigo.scian","nombre.actividad","descripcion.estrato",
                      "tipo.vialidad","nombre.vialidad","tipo.entre.vialidad.1","nombre.entre.vialidad.1",
                      "tipo.entre.vialidad.2","nombre.entre.vialidad.2","tipo.entre.vialidad.3","nombre.entre.vialidad.3",
                      "numero.exterior","letra.exterior","edificio","edificio.piso","numero.interior","letra.interior",
                      "tipo.asentamiento","nombre.asentamiento","tipo.centro.comercial","corredor.industrial.centro.comercial.o.mercado.publico",
                      "numero.local","codigo.postal","clave.entidad","entidad.federativa","clave.municipio","nombre.municipio", "clave.localidad",
                      "nombre.localidad","ageb","manzana","numero.telefono","email","sitio.web","tipo.establecimiento","lat","long", "fecha.incorporacion")
  colnames(datos.primarios.denue) <- nombre.headers
  columnas.elegidas <- c("id","nombre","codigo.scian","nombre.actividad","descripcion.estrato", "tipo.asentamiento",
                         "nombre.asentamiento","codigo.postal","clave.municipio","nombre.municipio","clave.localidad",
                         "nombre.localidad", "tipo.establecimiento", "lat", "long", "fecha.incorporacion")
  denue.columnas.seleccion <- datos.primarios.denue[,columnas.elegidas]
  # asignar potencial demanda por estrato
  denue.columnas.seleccion.demanda <- denue.columnas.seleccion
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="0 a 5 personas","demanda"] <- 1
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="6 a 10 personas","demanda"] <- 2
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="11 a 30 personas","demanda"] <- 4
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="31 a 50 personas","demanda"] <- 7
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="51 a 100 personas","demanda"] <- 14
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="101 a 250 personas","demanda"] <- 30
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="251 y mas personas","demanda"] <- 50
  denue.columnas.seleccion.demanda$subscian <- substr(as.character(denue.columnas.seleccion.demanda$codigo.scian),1,3)
  # esta es la columna para hacer el mapeo
  denue.columnas.seleccion.demanda$l4 <- substr(as.character(denue.columnas.seleccion.demanda$codigo.scian),1,4)
  
  carreras.l4.data <- read.csv(archivo.mapeo.carreras.l4,stringsAsFactors = F)
  colnames(carreras.l4.data)
  
  vector.de.trabajo <- carreras.l4.data[carreras.l4.data$Carrera==carrera,]
  cod.rel.vec.tra <- codigos.relevantes.a.la.carrera(vector.de.trabajo)
  denue.para.l4 <- denue.columnas.seleccion.demanda
  denue.14.sel <- denue.para.l4[denue.para.l4$l4 %in% cod.rel.vec.tra,]
  denue.14.sel$year <- substr(as.character(denue.14.sel$fecha.incorporacion),1,4)
  # parece haber datos desde el 2010, en altos sur, prospectiva, 2010-2019
  denue.14.sel$region <- region
  denue.14.sel$carrera <- carrera
  pred.data <- genera.datos.prediccion.l4(denue.14.sel,carrera,F,region)
  return(pred.data)
}
getwd()
save(datos.para.proyeccion.carrera.region,file="datos.para.proyeccion.carrera.region.R")
load("datos.para.proyeccion.carrera.region.R")

## en uso
datos.para.prediccion <- datos.para.proyeccion.carrera.region(denue.relevante("ALTOS SUR"),"Ingenieria Electromecanica","ALTOS SUR")

plot(datos.para.prediccion$y, xaxt="n", ylab="Existencia", xlab="", pch = datos.para.prediccion$style, col = datos.para.prediccion$style)
axis(1, labels=datos.para.prediccion$x, at=1:nrow(datos.para.prediccion), las=3)

### tree maps del nivel 4
install.packages("rpivotTable")
library(rpivotTable)
rpivotTable(Titanic)
Titanic

datos.para.proyeccion.carrera.region <- function(datos.fuente.archivo, carrera, region) {
  ventana.prediccion <- 15.5
  anio.prediccion <- 2030
  carrera.especifica <- ""
  archivo.mapeo.carreras.l4 <- "mapeocarreral4scian.csv"
  limite.superior.relevancia <- 2
  
  datos.primarios.denue <- read.csv(datos.fuente.archivo,stringsAsFactors = F) # fileEncoding='latin1')
  # limpiar los headers.
  nombre.headers <- c("id","nombre","razon.social","codigo.scian","nombre.actividad","descripcion.estrato",
                      "tipo.vialidad","nombre.vialidad","tipo.entre.vialidad.1","nombre.entre.vialidad.1",
                      "tipo.entre.vialidad.2","nombre.entre.vialidad.2","tipo.entre.vialidad.3","nombre.entre.vialidad.3",
                      "numero.exterior","letra.exterior","edificio","edificio.piso","numero.interior","letra.interior",
                      "tipo.asentamiento","nombre.asentamiento","tipo.centro.comercial","corredor.industrial.centro.comercial.o.mercado.publico",
                      "numero.local","codigo.postal","clave.entidad","entidad.federativa","clave.municipio","nombre.municipio", "clave.localidad",
                      "nombre.localidad","ageb","manzana","numero.telefono","email","sitio.web","tipo.establecimiento","lat","long", "fecha.incorporacion")
  colnames(datos.primarios.denue) <- nombre.headers
  columnas.elegidas <- c("id","nombre","codigo.scian","nombre.actividad","descripcion.estrato", "tipo.asentamiento",
                         "nombre.asentamiento","codigo.postal","clave.municipio","nombre.municipio","clave.localidad",
                         "nombre.localidad", "tipo.establecimiento", "lat", "long", "fecha.incorporacion")
  denue.columnas.seleccion <- datos.primarios.denue[,columnas.elegidas]
  # asignar potencial demanda por estrato
  denue.columnas.seleccion.demanda <- denue.columnas.seleccion
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="0 a 5 personas","demanda"] <- 1
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="6 a 10 personas","demanda"] <- 2
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="11 a 30 personas","demanda"] <- 4
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="31 a 50 personas","demanda"] <- 7
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="51 a 100 personas","demanda"] <- 14
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="101 a 250 personas","demanda"] <- 30
  denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$descripcion.estrato=="251 y mas personas","demanda"] <- 50
  denue.columnas.seleccion.demanda$subscian <- substr(as.character(denue.columnas.seleccion.demanda$codigo.scian),1,3)
  # esta es la columna para hacer el mapeo
  denue.columnas.seleccion.demanda$l4 <- substr(as.character(denue.columnas.seleccion.demanda$codigo.scian),1,4)
  
  carreras.l4.data <- read.csv(archivo.mapeo.carreras.l4,stringsAsFactors = F)
  vector.de.trabajo <- carreras.l4.data[carreras.l4.data$Carrera==carrera,]
  cod.rel.vec.tra <- codigos.relevantes.a.la.carrera(vector.de.trabajo)
  denue.para.l4 <- denue.columnas.seleccion.demanda
  denue.14.sel <- denue.para.l4[denue.para.l4$l4 %in% cod.rel.vec.tra,]
  denue.14.sel$year <- substr(as.character(denue.14.sel$fecha.incorporacion),1,4)
  # parece haber datos desde el 2010, en altos sur, prospectiva, 2010-2019
  denue.14.sel$region <- region
  denue.14.sel$carrera <- carrera
  # pred.data <- genera.datos.prediccion.l4(denue.14.sel,carrera,F,region)
  return(denue.14.sel)
}

datos.para.prediccion <- datos.para.proyeccion.carrera.region(denue.relevante("ALTOS SUR"),"Ingenieria Electromecanica","ALTOS SUR")

str(datos.para.prediccion)

rpivotTable(datos.para.prediccion)

# generate the dataset, provide a pivot table. 
# TODO: mapeo a nivel sector, para tener los nombres completos


