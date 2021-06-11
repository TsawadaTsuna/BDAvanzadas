## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(treemap)
library(reshape2)
library(ggplot2)
library(shinythemes)
library(rpivotTable)

# data
# datos.perfil.carreras.t -> perfilcarrera
# total.por.sectores -> vocacionamiento region
# total.subsector -> vocacionamiento subsectores
#?icon
#?dropdownMenu
ventana.prediccion <- 15.5
anio.prediccion <- 2030
setwd("D:/BDAvanzadas/Semana7")
load("datos.perfil.carreras.t.R")
load("total.por.sectores.t.R")
load("total.subsector.t.R")
# prospectiva 1
load("datos.vocacionamiento.melt.t.R")
load("genera.datos.prediccion.R")

load("pertinencia.graph.data.R")

# L4 prospectiva
load("regiones.master.R")
load("l4.lookup.table.R")
load("mario.molina.relevante.R")
load("denue.relevante.R")
load("listado.carreras.por.centros.R")
load("ramas.relevantes.a.la.carrera.R")
load("codigos.relevantes.a.la.carrera.R")
load("datos.para.proyeccion.carrera.region.R")
load("l2.lookup.table.R")
load("l3.lookup.table.R")
load("generando.datos.primarios.pivote.R")



     


# Core wrapping function
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

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

dbHeader <- dashboardHeader(title = "Prospectiva 1.0",
                            tags$li(a(href = 'https://sicyt.jalisco.gob.mx/',
                                      icon("power-off"),
                                      title = "Regresar a p??gina de inicio"),
                                    class = "dropdown"))

ui <- dashboardPage(skin="green",
 # dashboardHeader(title = "PROSPECTIVA 1.0", dropdownMenu(type = "messages",
 #                                                    messageItem(
 #                                                      from = "Soporte",
 #                                                       message = "Telefono XXX-XXX-XXXX",
 #                                                        icon = icon("life-ring")
  #                                                    )
#)
#  )
  dbHeader
,
  dashboardSidebar(
    sidebarMenu(
      menuItem("Perfil por Zona", tabName = "perfil", icon = icon("table")),
      menuItem("Proyecciones", tabName = "proyeccion", icon = icon("bar-chart-o")),
      menuItem("Tendencias", tabName = "tendencias", icon = icon("globe-americas")),
      menuItem("Proyeccion Demanda Carrera", tabName = "demandacarrera", icon = icon("angle-double-right")),
      menuItem("Recomendaciones", tabName = "recomendacion", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "perfil",
              fluidRow(
              
                box(
                  title = "Filtros",
                  selectInput(inputId = "region", label = "Region", choices = unique(datos.perfil.carreras.t$region))
                )
              ),
              fluidRow(
                
                box(plotOutput("nuevoingreso"), height = 450),
                box(plotOutput("egresos"), height = 450)
                
             ),
             fluidRow(
               
               box(plotOutput("vocacionamiento"), height = 450),
               box(plotOutput("agricultura"), height = 450)
             ),
             fluidRow(
               
               box(plotOutput("construccion"), height = 450),
               box(plotOutput("manufactura"), height = 450)
             ),
             fluidRow(
               
               box(plotOutput("comerciomay"), height = 450),
               box(plotOutput("transporte"), height = 450)
             ),
             fluidRow(
               
               box(plotOutput("medios"), height = 450),
               box(plotOutput("cienciaytec"), height = 450)
             ),
             fluidRow(
               
               box(plotOutput("apoyoneg"), height = 450),
               box(plotOutput("salud"), height = 450)
             ),
             fluidRow(
               
               box(plotOutput("hospitalidad"), height = 450),
               box(plotOutput("gobierno"), height = 450)
             )
      ),
      
      # Second tab content
      tabItem(tabName = "proyeccion",
              #h2("Proyecciones: oferta y demanda"),
              # prospectiva 2
              fluidRow(
                box(
                  title = "Filtros",
                  selectInput(inputId = "regionp", label = "Region", choices = unique(datos.vocacionamiento.melt.t$region)),
                  selectInput(inputId = "sectorp", label = "Sector", choices = NULL)
                )
              ),
              fluidRow(
                box(plotOutput("proyeccionregion"), height = 450)
              )
      ),
      tabItem(tabName = "tendencias",
              #h2("Tendencias: relevancia y automatizaci??n"),
              #fluidRow(
              #  box(plotOutput("relevanciaautom"), height = 450)
              #)
              plotOutput("relevanciaautom")
      ),
   
      tabItem(tabName = "demandacarrera",
              fluidRow(
                box(
                  title = "Filtros",
                  selectInput(inputId = "regionl4", label = "Region", choices = c(regiones.master,"ALTOS NORTE","VALLES"), selected = 1),
                  selectInput(inputId = "carreral4", label = "Carrera", choices = NULL),
                  actionButton("generateprediction", "Generar Proyeccion")
                )
              ),
              fluidRow(
                rpivotTableOutput("pivoteconomia", width = "80%", height = "500px")
              ),
              fluidRow(
                box(
              uiOutput("plotdemandacarrera") )),
              fluidRow(
                box(
                  tableOutput("ramasrelacionadas") ))
             
      ),
      tabItem(tabName = "recomendacion",
              h2("Recomendaciones a matriculacion")
      )
    )
  )
)

server <- function(input, output, session) {

  # puedo usar dos eventreactive independientes para obtener las etiquetas de
  # los sectores y los datos del plot
  
  # cargar carreras
  # listado.carreras.por.centros(mario.molina.relevante("ALTOS SUR"))
  
  # tabla pivote
  
  datos.primarios.pivote <- reactive({
    datos.primarios.pivote <- generando.datos.primarios.pivote(denue.relevante(input$regionl4))
  })
  
  #datos.primarios.pivote <- generando.datos.primarios.pivote(denue.relevante(input$regionl4))
  
   output$ pivoteconomia <- renderRpivotTable({
   rpivotTable(data = datos.primarios.pivote()) 
  #             , rows = c( "Province"),cols="Party",
  # vals = "votes", aggregatorName = "Sum", rendererName = "Table",
  # width="100%", height="500px")
   })
  
  listado.carreras.centros <- reactive({
    listado.carreras.centros <- listado.carreras.por.centros(mario.molina.relevante(input$regionl4))
  })
  
  observe({
    updateSelectInput(session = session, inputId = "carreral4", choices = listado.carreras.centros())
  })
  
  ramas.a.mostrar <- eventReactive(input$generateprediction, {
    ramas.a.mostrar <- ramas.relevantes.a.la.carrera(input$carreral4)
  })
  
  datos.para.prediccion.l4 <- eventReactive(input$generateprediction, {
    datos.para.prediccion.l4 <- datos.para.proyeccion.carrera.region(denue.relevante(input$regionl4),input$carreral4,input$regionl4)
  })

 # output$plotdemandacarrera <- renderPlot({
  #  par(mar = c(5,4,4,4))
  #  plot(datos.para.prediccion.l4()$y, xaxt="n", ylab="Existencia", xlab="", pch = datos.para.prediccion.l4()$style, col = datos.para.prediccion.l4()$style)
  #  axis(1, labels=datos.para.prediccion.l4()$x, at=1:nrow(datos.para.prediccion.l4()), las=3)
#  })
  
  output$ramasrelacionadas <- renderTable(ramas.a.mostrar())
  # output$ramasrelacionadas <- renderTable(iris)
  
  
  data.plot2 <- eventReactive(input$generateprediction, {
    data.plot2 <- 10
  })
  
  data.plot <- reactive({
    aux <- input$regionl4
    data.plot <- NA
    if (input$regionl4 == "ALTOS SUR") {
      data.plot <- 1
    } else {
      data.plot <- 1
    }
   # data.plot <- rnorm(input$totaldatos,mean=input$totaldatos*2,sd=input$totaldatos*3)
  })
  
  output$prospectivaplot <- renderPlot({
    par(mar = c(5,4,4,4))
    plot(datos.para.prediccion.l4()$y, xaxt="n", main=input$carreral4, ylab="Existencia", xlab="", pch = datos.para.prediccion.l4()$style, col = datos.para.prediccion.l4()$style)
    axis(1, labels=datos.para.prediccion.l4()$x, at=1:nrow(datos.para.prediccion.l4()), las=3)
  })
  
  output$plotdemandacarrera <- renderUI({
    if (is.na(data.plot())) {
      p(paste0("Utilice los selectores para generar una proyeccion",data.plot2()))
    } else {
      plotOutput("prospectivaplot")
    }
      
    
  }) 
  
  
  # end ejercicio
  
  # prospectiva 3
  prospectiva.sectores.opt <- reactive({
    prospectiva.sectores.opt <- unique(datos.vocacionamiento.melt.t[datos.vocacionamiento.melt.t$region==input$regionp,]$sector.nombre)
    prospectiva.sectores.opt <- prospectiva.sectores.opt[!is.na(prospectiva.sectores.opt)]
     })
  
  observe({
    updateSelectInput(session = session, inputId = "sectorp", choices = prospectiva.sectores.opt())
  })
  
  prospectiva.sector.data <- reactive({
    sector.id.selection <- datos.vocacionamiento.melt.t[datos.vocacionamiento.melt.t$region==input$regionp & datos.vocacionamiento.melt.t$sector.nombre==input$sectorp,"sector.scian"][1]
    prospectiva.sector.data <- genera.datos.prediccion(datos.vocacionamiento.melt.t,sector.id.selection,F,input$regionp)
  })

  output$proyeccionregion <- renderPlot({
    par(mar = c(5,4,4,4))
    plot(prospectiva.sector.data()$y, xaxt="n", 
         ylab="Existencia", xlab="", pch = prospectiva.sector.data()$style, 
         col = prospectiva.sector.data()$style, main=input$sectorp)
    axis(1, labels=prospectiva.sector.data()$x, at=1:nrow(prospectiva.sector.data()), las=3)
  })
  
  # end prospectiva 3
  
  # relevancia autom
  
  output$relevanciaautom <- renderPlot({
    # plotdata <- resultado.altos.sur.sel
    par(cex.lab=.5)
    par(cex.axis=.5)
    par(mar = c(2,7,2,1))  # default is 5,4,4,2
    plotdata <- pertinencia.graph.data
    plotdata.melt <- melt(plotdata[,c('carrera','pertinencia','automatizable')],id.vars = 1)
    ggplot(plotdata.melt,aes(x = carrera,y = value)) + 
      geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      xlab("Carreras") + ylab("Totales") + labs(fill = "valores") + theme(
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 6)) +
      theme(legend.position = c(.8, .9)) +
      scale_fill_manual("variable", values = c("pertinencia" = "#4682B4", "automatizable" = "#006400"))
    
  })
  
  # end relevancia autom
  
  profile.plot.data <- reactive({
    profile.plot.data <- datos.perfil.carreras.t[datos.perfil.carreras.t$region==input$region,]
  })
  
  vocacionamiento.plot.data <- reactive({
    vocacionamiento.plot.data <- total.por.sectores.t[total.por.sectores.t$region==input$region,]
  })
  
  vocacionamiento.sub.agr.plot.data <- reactive({
    vocacionamiento.sub.agr.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="AGROPECUARIO", ]
  })
  
  vocacionamiento.sub.cons.plot.data <- reactive({
    vocacionamiento.sub.cons.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="CONSTRUCCION", ]
  })
  
  vocacionamiento.sub.man.plot.data <- reactive({
    vocacionamiento.sub.man.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="MANUFACTURA", ]
  })
  
  vocacionamiento.sub.cmay.plot.data <- reactive({
    vocacionamiento.sub.cmay.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="COMERCIO (MAYOR)", ]
  })
  
  vocacionamiento.sub.trans.plot.data <- reactive({
    vocacionamiento.sub.trans.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="TRANSPORTE", ]
  })
  
  vocacionamiento.sub.med.plot.data <- reactive({
    vocacionamiento.sub.med.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="MEDIOS", ]
  })
  
  vocacionamiento.sub.citec.plot.data <- reactive({
    vocacionamiento.sub.citec.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="S. PROF,CIEN,TEC", ]
  })
  
  vocacionamiento.sub.apoyo.plot.data <- reactive({
    vocacionamiento.sub.apoyo.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="S. APOYO NEG", ]
  })
  
  vocacionamiento.sub.salud.plot.data <- reactive({
    vocacionamiento.sub.salud.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="SALUD", ]
  })
  
  vocacionamiento.sub.hosp.plot.data <- reactive({
    vocacionamiento.sub.hosp.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="HOSPITALIDAD", ]
  })
  
  vocacionamiento.sub.gob.plot.data <- reactive({
    vocacionamiento.sub.gob.plot.data <- total.subsector.t[total.subsector.t$region==input$region & total.subsector.t$sector=="GOBIERNO", ]
  })
  
  output$nuevoingreso <- renderPlot({
    # plotdata <- resultado.altos.sur.sel
    par(cex.lab=.5)
    par(cex.axis=.5)
    par(mar = c(2,7,2,1))  # default is 5,4,4,2
    #plotdata <- datos.perfil.carreras.t[datos.perfil.carreras.t$region=="ALTOS SUR",]
    plotdata.melt <- melt(profile.plot.data()[,c('carr.corto','TOTAL.NUEVO.INGRESO','TOTAL.EGRESADOS')],id.vars = 1)
    ggplot(plotdata.melt,aes(x = carr.corto,y = value)) + 
      geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      xlab("Carreras") + ylab("Totales") + labs(fill = "valores") + theme(
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 6)) +
      theme(legend.position = c(.8, .9)) + ggtitle("ingresos vs egresos 2019/2020") +
      scale_fill_manual("variable", values = c("TOTAL.NUEVO.INGRESO" = "#4682B4", "TOTAL.EGRESADOS" = "#006400"))
    
    
     })
  
  output$egresos <- renderPlot({
    # plotdata <- resultado.altos.sur.sel
    par(cex.lab=.5)
    par(cex.axis=.5)
    par(mar = c(2,7,2,1))  # default is 5,4,4,2
    plotdata.melt <- melt(profile.plot.data()[,c('carr.corto','competencia.ingreso','demanda')],id.vars = 1)
    ggplot(plotdata.melt,aes(x = carr.corto,y = value)) + 
      geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      xlab("Carreras") + ylab("Totales") + labs(fill = "valores") + theme(
        legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 6)) +
      theme(legend.position = c(.8, .9)) + ggtitle("competencia y demanda 2019/2020") +
      scale_fill_manual("variable", values = c("competencia.ingreso" = "#4682B4", "demanda" = "#006400"))
    
  })
  
  output$vocacionamiento <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.plot.data(),
            index="sector",
            vSize="total.sector",
            type="index",
            title = "Todos los Sectores"
    )
  })
  
  output$agricultura <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.agr.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Agricultura"
    )
  })
  
  output$construccion <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.cons.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Construccion"
    )
  })
  
  output$manufactura <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.man.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Manufactura"
    )
  })
  
  output$comerciomay <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.cmay.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Comercio Mayoreo"
    )
  })
  
  output$transporte <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.trans.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Transporte"
    )
  })
  
  output$medios <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.med.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Medios"
    )
  })
   
  output$cienciaytec <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.citec.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Ciencia y Tecnologia"
    )
  })
  
  output$apoyoneg <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.apoyo.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Apoyo Negocio"
    )
  })
  
  output$salud <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.salud.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Salud"
    )
  })
  
  output$hospitalidad <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.hosp.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Hospitalidad"
    )
  })
  
  output$gobierno <- renderPlot({
    par(mar = c(5,4,4,4))
    #?treemap
    #vocacionamiento.plot.data
    treemap(vocacionamiento.sub.gob.plot.data(),
            index="nombre.subsector",
            vSize="total",
            type="index",
            title = "Gobierno"
    )
  })
  
  
}

shinyApp(ui, server)