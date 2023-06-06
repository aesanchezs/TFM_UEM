ui <- dashboardPage(

  dashboardHeader(title = HTML("<img src='https://upload.wikimedia.org/wikipedia/en/thumb/f/fc/Wolverhampton_Wanderers.svg/1200px-Wolverhampton_Wanderers.svg.png' height='40'/>")),
  

  dashboardSidebar(
    sidebarMenu(
      menuItem("Análisis de equipo", tabName = "Equipo", icon = icon("users")),
      menuItem("Análisis de entrenador", tabName = "Entrenador", icon = icon("user-tie")),
      menuItem("Scouting", tabName = "Scouting", icon = icon("binoculars")),
      menuItem("Jugadores", tabName = "Jugadores", icon = icon("futbol"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
.skin-blue .main-header .navbar{
background-color: #ffb700;
}

/* logo when hovered */
.skin-blue .main-header .logo:hover {
background-color: #ffb700 !important;
}
 .skin-blue .main-sidebar {
                                background-color: #ffb700;
                                }
.skin-blue .main-header .logo{
background-color: #ffb700;
}
.skin-blue .main-header .navbar .sidebar-toggle:hover{
background-color: #eba905;
}
.content-wrapper, .right-side {
background-color: #f2ecd3;
}
.skin-blue .sidebar-menu a,
.skin-blue .treeview-menu > li > a {
color: #ffffff;
}
"))
    ),
    tabItems(
      tabItem(tabName = "Equipo",
              fluidRow(
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26026/sa.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26013/collins.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26028/semedo.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26011/ait-nouri.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26023/neves.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26038/moutinho.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26178/nunes.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26024/podence.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26030/traore.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26039/neto.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/26019/jimenez.png?width=400&quality=80&format=webp", height = "100px")),
                column(width = 1,
                       tags$img(src = "https://func-wolves-playmaker-imageresizer-prod-uksouth.azurewebsites.net/api/media/28632/cunha.png?width=400&quality=80&format=webp", height = "100px"))
              ),
              fluidRow(
                column(width = 2, selectInput(inputId = "columna1", label = "Estadística", choices = colnames(Wolves), selected = "xG"))
                
              ),
              fluidRow(
                column(width = 4,tableOutput(outputId = "jugadores")),
                column(width = 8,
                       img(src = "https://pbs.twimg.com/media/FtqVUqcWAAEqzio?format=jpg&name=medium", width = "100%", height = "630px"))
              ),
              fluidRow(
                column(width = 6,
                       selectInput("variable", label = "Estadística:", 
                                   choices = colnames(Premier), selected = "Goles")),
                column(width = 3,
                       selectInput("correlacion1", label = "Valor de x:", 
                                   choices = colnames(Premier), selected = "Goles")),
                column(width = 3,
                       selectInput("correlacion2", label = "Valor de y", 
                                   choices = colnames(Premier), selected = "Tiros"))
              ),
              fluidRow(
                column(width = 6,
                       plotlyOutput("boxplot", height = "800px")),
                column(width = 6,
                       plotOutput("correlacion", height = "800px"))
              )
      ),
      tabItem(tabName = "Entrenador",
              fluidRow(
                column(width = 4,
                       checkboxGroupInput(inputId = "Liga",
                                          label = 'Liga:', choices = c("Premier League" = "Premier League",
                                                                       "Ligue 1" = "Ligue 1",
                                                                       "Bundesliga" = "Bundesliga",
                                                                       "Serie A" = "Serie A",
                                                                       "La Liga" = "La Liga",
                                                                       "Primeira Liga" = "Primeira Liga"), 
                                          selected = c("Primeira Liga" = "Primeira Liga"),inline=TRUE),
                       column(width = 3, offset = 2,
                              tags$img(src = "https://www.footyrenders.com/render/Julen-Lopetegui-310x540.png", height = "600px")
                       )
                ),
                column(width = 8,
                       plotlyOutput("RadarLopetegui", height = 700)
                )
              ),
              
              fluidRow(
                column(width = 2, selectInput(inputId = "columna", label = "Estadística", choices = colnames(datos_sin_equipos))),
                column(width = 2, offset = 2, selectInput("x", "Eje X", choices = colnames(datos_sin_equipos),
                                                          selected = "Pases completados")),
                column(width = 2, selectInput("y", "Eje Y", choices = colnames(datos_sin_equipos),
                                              selected = "Pases intentados"))
              ),
              fluidRow(
                column(width = 4,tableOutput(outputId = "tabla")),
                column(width = 8, plotlyOutput(outputId = "scatterplot", height = "600px"))
              )
              
      ),
      tabItem(tabName = "Scouting",
              fluidRow(
                column(width = 3,
                       selectInput('ID', label='Elige a un jugador:',choices=unique(data$ID)),
                       sliderInput("Edad", "Edad:",
                                   min = 16, max = 40,
                                   value = c(16,40)),
                       tags$style(".js-irs-0 .irs-bar {background: #000000; border-color: #000000;}
            .js-irs-0 .irs-from, .js-irs-0 .irs-to, .js-irs-0 .irs-single {background: #000000; border-color: #000000;}"),
                       sliderInput("Transferencia", "Valor de mercado:",
                                   min = 0, max = 110000000,
                                   value = c(0,110000000)),
                       tags$style(".js-irs-1 .irs-bar {background: #000000; border-color: #000000;}
            .js-irs-1 .irs-from, .js-irs-1 .irs-to, .js-irs-1 .irs-single {background: #000000; border-color: #000000;}"),
                       sliderInput("Minutos", "Minutos:",
                                   min = 0, max = 3800,
                                   value = c(0,3800)),
                       tags$style(".js-irs-2 .irs-bar {background: #000000; border-color: #000000;}
            .js-irs-2 .irs-from, .js-irs-2 .irs-to, .js-irs-2 .irs-single {background: #000000; border-color: #000000;}"),
                       
                       checkboxGroupInput(inputId = "Posicion",
                                          label = 'Posición:', choices = c("Central" = "Central",
                                                                           "Lateral derecho"="Lateral derecho","Lateral izquierdo"="Lateral izquierdo",
                                                                           "Carrilero derecho"="Carrilero derecho","Carrilero izquierdo"="Carrilero izquierdo",
                                                                           "Mediocentro"="Mediocentro",
                                                                           "Interior"="Interior",
                                                                           "Extremo derecho"="Extremo derecho","Extremo izquierdo"="Extremo izquierdo",
                                                                           "Mediapunta"="Mediapunta","Delantero"="Delantero"), 
                                          selected = c("Mediapunta"="Mediapunta"),inline=TRUE),
                       checkboxGroupInput(inputId = "Competicion",
                                          label = 'Competición:', choices = c("Premier League" = "Premier League",
                                                                              "Ligue 1" = "Ligue 1",
                                                                              "Bundesliga" = "Bundesliga",
                                                                              "Serie A" = "Serie A",
                                                                              "La Liga" = "La Liga",
                                                                              "Primeira Liga" = "Primeira Liga",
                                                                              "Eredivisie" = "Eredivisie"), 
                                          selected = c("Premier League" = "Premier League"),inline=TRUE),
                       tags$style(".btn-primary {background-color: #000000; border-color: #000000;}
            .btn-primary:hover {background-color: #000000; border-color: #000000;}
            .btn-primary:focus {background-color: #000000; border-color: #000000;}
             .btn-primary:active:focus {background-color: #303030; border-color: #303030;}
            .btn-primary:active {background-color: #000000; border-color: #000000;}")
                ),
                column(width = 9,
                       plotlyOutput("plot1", height = 700)
                )
              )
      ), 
      tabItem(tabName = "Jugadores",
              fluidRow(
                column(
                  width = 2,
                  selectInput("jugador", "Jugador:", 
                              choices = unique(data_2021_2022$Jugador), selected = head(unique(data_2021_2022$Jugador),2))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  plotOutput("grafico", height = 750)
                ),
                column(
                  width = 4,
                  plotOutput("grafico2", height =750)
                ),
                column(
                  width = 4,
                  plotOutput("grafico3", height =750)
                )
              ),
              fluidRow(
                column(width = 2,
                       selectInput("jugador2", "Jugador 2", choices = unique(data_2021_2022$Jugador), selected = head(unique(data_2021_2022$Jugador), 3)))
              ),
              fluidRow(
                column(
                  width = 4,
                  plotOutput("grafico4", height =750)
                ),
                column(
                  width = 4,
                  plotOutput("grafico5", height =750)
                ),
                column(
                  width = 4,
                  plotOutput("grafico6", height =750)
                )
              )
      )
      
    )
  )
)
