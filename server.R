
server <- function(input, output) {
  
  #PESTAÑA WOLVES
  
  #Tabla estadísticas de jugadores
  
  output$jugadores <- renderUI({
    
    columna_seleccionada <- input$columna1
    
    indices_ordenados <- order(Wolves[[columna_seleccionada]], decreasing = TRUE)
    
    tabla_html <- paste0("<table style='background-color: #f2e7c7;' border='1' width='435'>",
                         "<tr>",
                         "<th style='background-color: #f7cd4f; border: 1px solid black; width:35px; text-align:center;'>N</th>",
                         "<th style='background-color: #f7cd4f; border: 1px solid black; width:200px; text-align:center;'>Equipo</th>",
                         "<th style='background-color: #f7cd4f; border: 1px solid black; width:200px; text-align:center;'>", columna_seleccionada, "</th>",
                         "</tr>")
    
    for (i in seq_along(indices_ordenados)) {
      Jugador <-Wolves$Jugador[indices_ordenados[i]]
      valor <- Wolves[[columna_seleccionada]][indices_ordenados[i]]
      
      fila_html <- paste0("<tr>",
                          "<td>", i, "</td>",
                          "<td>", Jugador, "</td>",
                          "<td style='text-align:center;'>", valor, "</td>",
                          "</tr>")
      
      tabla_html <- paste0(tabla_html, fila_html)
    }
    
    tabla_html <- paste0(tabla_html, "</table>")
    
    HTML(tabla_html)
  })
  
  
  #Gráfico Caja y bigote
  
  output$boxplot <- renderPlotly({
    ggplot(Premier, aes(x = "", y = .data[[input$variable]], label = Equipo, group = Equipo)) +
      geom_boxplot(fill = "#d4cbb4") +
      geom_jitter(aes(color = ifelse(Equipo == "Wolves", "Wolves", "otros equipos"))) +  
      scale_color_manual(values = c("otros equipos" = "black", "Wolves" = "#ffb700")) +  
      coord_flip() -> p
    
    p <- p + theme(
      plot.background = element_rect(fill = "#f2ecd3", color = NA),
      panel.background = element_rect(fill = "#f2efe1", color = NA)
    ) + 
      guides(color = FALSE)
    
    ggplotly(p, tooltip = c("y", "label"))
  })
  
  #Gráfico de correlación
  
  
  datos_react <- reactive({
    ggplot(data = Premier, aes(x = eval(parse(text = input$correlacion1)), y = eval(parse(text = input$correlacion2)))) +
      geom_image(aes(image = image), size = 0.05) +
      theme(panel.background = element_rect(fill = "#f2efe1"),
            plot.background = element_rect(fill = "#f2ecd3"))+
      labs(x = "", y = "")
    
  })
  
  output$correlacion <- renderPlot({
    datos_react()
  })
  

  
  #PESTAÑA LOPETEGUI 
  
  
  
  
  #Radar Lopetegui 
  
  Data1 <- reactive({
    Julen_Lopetegui %>%
      filter(Equipo != "Sevilla Lopetegui")
  })
  
  
  Data2 <- reactive({
    Data1() %>%
      select(1, 133, 134, 135, 137, 139, 140, 142, 144, 146, 147, 149, 151, 181, 153,
             155, 157, 159, 161, 163, 165, 167, 169, 171, 173, 175, 177, 178, 179) %>%
      filter(Data1()$Liga %in% input$Liga)
  })
  
  Data3 <- reactive({
    Julen_Lopetegui %>%
      select(1, 133, 134, 135, 137, 139, 140, 142, 144, 146, 147, 149, 151, 181, 153,
             155, 157, 159, 161, 163, 165, 167, 169, 171, 173, 175, 177, 178, 179) %>%
      filter(Julen_Lopetegui$Equipo == gsub("[[:space:]]*$","",gsub("- .*",'',"Sevilla Lopetegui"))) 
    
    
  })
  
  Data4 <- reactive({
    rbind(Data3(),Data2())
    
  })
  
  Data5 <- reactive({
    Data4() %>%
      select(3:29)
  })
  
  Data6 <- reactive({
    as.numeric(knnx.index(Data5(), Data5()[1, , drop=FALSE], k=11))
  })
  
  Data7 <- reactive({
    Data4()[Data6(),]
  })
  
  Data8 <- reactive({
    Data7() %>%
      select(3:29)
    
  })
  
  output$RadarLopetegui <- renderPlotly({
    
    validate(
      need(dim(Data2())[1]>=10, "No se han encontrado 10 equipos similares."
      )
    )
    
    campos <- Data8()
    campos <- as.matrix(campos)
    
    
    plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    ) %>%
      add_trace(
        r = (campos[1,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        name = Data7()[1,1],
        fillcolor = '#eba90580'
      ) %>%
      add_trace(
        r = (campos[2,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[2,1], 1, nchar(Data7()[2,1]))
      ) %>%
      add_trace(
        r = (campos[3,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[3,1], 1, nchar(Data7()[3,1]))
      ) %>%
      add_trace(
        r = (campos[4,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[4,1], 1, nchar(Data7()[4,1]))
      ) %>%
      add_trace(
        r = (campos[5,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[5,1], 1, nchar(Data7()[5,1]))
      ) %>%
      add_trace(
        r = (campos[6,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[6,1], 1, nchar(Data7()[6,1]))
      ) %>%
      add_trace(
        r = (campos[7,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[7,1], 1, nchar(Data7()[7,1]))
      ) %>%
      add_trace(
        r = (campos[8,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[8,1], 1, nchar(Data7()[8,1]))
      ) %>%
      add_trace(
        r = (campos[9,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[9,1], 1, nchar(Data7()[9,1]))
      ) %>%
      add_trace(
        r = (campos[10,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[10,1], 1, nchar(Data7()[10,1]))
      ) %>%
      add_trace(
        r = (campos[11,]),
        theta = c("Puntos","Edad","Gol AF","GOL EC","Posesión","Tiro x jugada","Tiro x reg","Tiro x ABP",
                  "Dist tir","Pases para tiro","FDJ","Pases al espacio", "Regates","Pases","Pases largos","Pases progresivos","Cambios or","Toques 1/3", "Toques 2/3",
                  "Toques 3/3","Entradas 1/3","Entradas 2/3","Entradas 3/3", "Despejes", "Duelos Aéreos", "PPDA", "OPPDA"),
        showlegend = TRUE,
        mode = "markers",
        visible="legendonly",
        name = substr(Data7()[11,1], 1, nchar(Data7()[11,1]))
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,1)
          )
        ),
        
        plot_bgcolor = "#f2ecd3",
        paper_bgcolor = "#f2ecd3", # Cambiar el color de fondo de la región de trazado
        showlegend = TRUE
        
      )
    
    
  })
  
  #Tabla
  
  output$tabla <- renderUI({
    
    columna_seleccionada <- input$columna
    
    indices_ordenados <- order(Julen_Lopetegui1[[columna_seleccionada]], decreasing = TRUE)
    
    tabla_html <- paste0("<div style='max-height: 600px; max-width: 470px; overflow-y: scroll;'>",
                         "<table style='background-color: #f2e7c7;' border='1' width='435'>",
                         "<tr>",
                         "<th style='background-color: #f7cd4f; border: 1px solid black; width:35px; text-align:center;'>N</th>",
                         "<th style='background-color: #f7cd4f; border: 1px solid black; width:200px; text-align:center;'>Equipo</th>",
                         "<th style='background-color: #f7cd4f; border: 1px solid black; width:200px; text-align:center;'>", columna_seleccionada, "</th>",
                         "</tr>")
    
    
    
    for (i in seq_along(indices_ordenados)) {
      equipo <- Julen_Lopetegui1$Equipo[indices_ordenados[i]]
      valor <- Julen_Lopetegui1[[columna_seleccionada]][indices_ordenados[i]]
      
      if (equipo == "Sevilla Lopetegui") {
        fila_html <- paste0("<tr style='background-color: #ffa570;' border='1'>",
                            "<td border='1'>", i, "</td>",
                            "<td border='1'>", equipo, "</td>",
                            "<td border='1' style='text-align:center;'>", valor, "</td>",
                            "</tr>")
      } else {
        fila_html <- paste0("<tr border='1'>",
                            "<td border='1'>", i, "</td>",
                            "<td border='1'>", equipo, "</td>",
                            "<td border='1' style='text-align:center;'>", valor, "</td>",
                            "</tr>")
      }
      
      tabla_html <- paste0(tabla_html, fila_html)
    }
    
    tabla_html <- paste0(tabla_html, "</table>")
    
    HTML(tabla_html)
  }) 
  
  #Gráfico de dispersión 
  
  crear_grafica <- reactive({
    
    if (!input$x %in% colnames(Julen_Lopetegui1) || !input$y %in% colnames(Julen_Lopetegui1)) {
      return(NULL)
    }
    
    datos_validos <- Julen_Lopetegui1[!is.na(Julen_Lopetegui1[, input$x]) & !is.na(Julen_Lopetegui1[, input$y]), ]
    
    equipo_sevilla <- datos_validos[datos_validos$Equipo == "Sevilla Lopetegui", ]
    distancia <- sqrt((datos_validos[, input$x] - equipo_sevilla[[input$x]])^2 +
                        (datos_validos[, input$y] - equipo_sevilla[[input$y]])^2)
    
    cercanos <- order(distancia)[1:6]
    
    datos_validos$es_cercano <- FALSE
    datos_validos$es_cercano[cercanos] <- TRUE
    datos_validos$es_sevilla_lopetegui <- ifelse(datos_validos$Equipo == "Sevilla Lopetegui", TRUE, FALSE)
    p <- ggplot(datos_validos, aes(x = !!sym(input$x), y = !!sym(input$y), text = Equipo), width=800, height=600) +
      geom_point(data = datos_validos[datos_validos$es_sevilla_lopetegui, ], aes(x = !!sym(input$x), y = !!sym(input$y)), color = "#ffb700", size = 3) +
      geom_point(data = datos_validos[datos_validos$es_cercano & !datos_validos$es_sevilla_lopetegui, ], aes(x = !!sym(input$x), y = !!sym(input$y)), color = "#6b3e99", size = 3) +
      geom_point(data = datos_validos[!datos_validos$es_cercano & !datos_validos$es_sevilla_lopetegui, ], aes(x = !!sym(input$x), y = !!sym(input$y)), size = 3, color="#000000", fill="#ffff00") +
      labs(x = input$x, y = input$y) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            panel.background = element_rect(fill = "#f2efe1"),
            plot.background = element_rect(fill = "#f2ecd3"))
    ggplotly(p, tooltip = "text")
  })
  
  output$scatterplot <- renderPlotly({
    crear_grafica()
  })
  
  
  #PESTAÑA SCOUTING
  
  selectedData1 <- reactive({
    data %>%
      filter(ID != input$ID & Temporada == "2021-2022")
  })
  
    
    selectedData2 <- reactive({
      selectedData1() %>%
        select(122, 4, 7, 8, 11, 120, 132, 133, 134, 131, 135, 141,139, 140,
               144, 146, 147, 148, 136, 137, 138, 143, 127, 157, 128, 129, 130, 145) %>%
      filter(selectedData1()$Posicion %in% input$Posicion,
             selectedData1()$Competicion %in% input$Competicion) %>%
      filter(Edad >= input$Edad[1]) %>%
      filter(Edad <= input$Edad[2]) %>%
      filter(`Valor de mercado` >= input$Transferencia[1])  %>%
      filter(`Valor de mercado` <= input$Transferencia[2]) %>%
        filter(`Minutos jugados` >= input$Minutos[1])  %>%
        filter(`Minutos jugados` <= input$Minutos[2]) 
    })
    
    selectedData3 <- reactive({
      data %>%
        select(122, 4, 7, 8, 11, 120, 132, 133, 134, 131, 135, 141,139, 140,
               144, 146, 147, 148, 136, 137, 138, 143, 127, 157, 128, 129, 130, 145) %>%
        filter(data$ID == gsub("[[:space:]]*$","",gsub("- .*",'',input$ID))) 
      
      
    })
    
    selectedData4 <- reactive({
      rbind(selectedData3(),selectedData2())
      
    })
    
    selectedData5 <- reactive({
      selectedData4() %>%
      select(7:28)
    })
    
    selectedData6 <- reactive({
    as.numeric(knnx.index(selectedData5(), selectedData5()[1, , drop=FALSE], k=11))
    })
    
    selectedData7 <- reactive({
      selectedData4()[selectedData6(),]
    })
    
    selectedData8 <- reactive({
      selectedData7() %>%
        select(7:28)
      
    })


    output$plot1 <- renderPlotly({
      
      validate(
        need(dim(selectedData2())[1]>=10, "No se han encontrado 10 jugadores de caracterísiticas de juego similares."
        )
      )
      
      campos <- selectedData8()
      campos <- as.matrix(campos)
      
      
      plot_ly(
        type = 'scatterpolar',
        mode = "closest",
        fill = 'toself'
      ) %>%
        add_trace(
          r = (campos[1,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF", "DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          name = selectedData7()[1,1],
          fillcolor = '#eba90580'
        ) %>%
        add_trace(
          r = (campos[2,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF", "DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[2,1], 1, nchar(selectedData7()[2,1]) - 9)
        ) %>%
        add_trace(
          r = (campos[3,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF","DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[3,1], 1, nchar(selectedData7()[3,1]) - 9)
        ) %>%
        add_trace(
          r = (campos[4,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF","DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[4,1], 1, nchar(selectedData7()[4,1]) - 9)
        ) %>%
        add_trace(
          r = (campos[5,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF","DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[5,1], 1, nchar(selectedData7()[5,1]) - 9)
        ) %>%
        add_trace(
          r = (campos[6,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF","DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[6,1], 1, nchar(selectedData7()[6,1]) - 9)
        ) %>%
        add_trace(
          r = (campos[7,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF","DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[7,1], 1, nchar(selectedData7()[7,1]) - 9)
        ) %>%
        add_trace(
          r = (campos[8,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF","DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[8,1], 1, nchar(selectedData7()[8,1]) - 9)
        ) %>%
        add_trace(
          r = (campos[9,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF","DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[9,1], 1, nchar(selectedData7()[9,1]) - 9)
        ) %>%
        add_trace(
          r = (campos[10,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF","DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[10,1], 1, nchar(selectedData7()[10,1]) - 9)
        ) %>%
        add_trace(
          r = (campos[11,]),
          theta = c("GOLES","xG","REM","ACC ATAQ","ASIS","xA","PAS","PAS AD",
                    "PAS PROG","PAS L","PAS PROF","PAS UT","CENT","REG","CARR","DES","ACC DEF","DUEL D",
                    "ENT","TIR INT","INT","DUEL A"),
          showlegend = TRUE,
          mode = "markers",
          visible="legendonly",
          name = substr(selectedData7()[11,1], 1, nchar(selectedData7()[11,1]) - 9)
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,100)
            )
          ),
          
          plot_bgcolor = "#f2ecd3",
          paper_bgcolor = "#f2ecd3", # Cambiar el color de fondo de la región de trazado
          showlegend = TRUE
          
        )
      
      
    })

    #PESTAÑA JUGADORES
    
    
    output$grafico <- renderPlot({
      datos_jugador <- subset(data_2021_2022, Jugador == input$jugador)
      valores_barras <- rev(c(datos_jugador$percentil_Goles, datos_jugador$percentil_xG90, datos_jugador$percentil_Remates,datos_jugador$percentil_AccionesAT,
                              datos_jugador$percentil_duelos_atacantes, datos_jugador$percentil_toques_penalti, datos_jugador$percentil_Regates,
                              datos_jugador$percentil_Carreras, datos_jugador$percentil_faltas_recibidas, datos_jugador$percentil_desmarques, 
                              datos_jugador$percentil_aceleraciones))
      nombres_barras <- rev(c("Goles", "xG", "Rem", "Acc At", "Duel At", "Toq AP", "Reg", "Carr",
                              "F Rec", "Des", "Acel" ))
      par(bg="#f2ecd3")
      bp <- barplot(valores_barras, horiz = TRUE, 
                    names.arg = nombres_barras, xlab = "Percentil", ylab = NULL,
                    main = "Estadísticas ofensivas", col = "#fa234a",
                    xlim = c(0,100))
      
      valores_etiqueta <- round(valores_barras, 2) 
      etiquetas <- paste0(as.character(valores_etiqueta), "") 
      text(x = valores_barras - 14, y = bp, labels = etiquetas, pos = 4, cex = 1.2, col = "black")
    })
    
    output$grafico2 <- renderPlot({
      datos_jugador <- subset(data_2021_2022, Jugador == input$jugador)
      valores_barras <- c(datos_jugador$percentil_Centros, datos_jugador$percentil_pases_atras, datos_jugador$percentil_pases_laterales,
                          datos_jugador$percentil_pases_adelante, datos_jugador$percentil_pases_largos, datos_jugador$percentil_pases_progresivos,
                          datos_jugador$percentil_pases_profundidad, datos_jugador$percentil_pases_ultimo_tercio, datos_jugador$percentil_pases,
                          datos_jugador$percentil_xA, datos_jugador$percentil_Asistencias)
      nombres_barras <- c("Cent", "P atrás", "P lat", "P adel", "P lar", "P prog", "P prof", "P ut","Pas", "xA", "Asis")
      par(bg="#f2ecd3")
      bp <- barplot(valores_barras, horiz = TRUE, 
                    names.arg = nombres_barras, xlab = "Percentil", ylab = NULL,
                    main = "Estadísticas de pase", col = "#80e854",
                    xlim = c(0,100))
      
      valores_etiqueta <- round(valores_barras, 2) 
      etiquetas <- paste0(as.character(valores_etiqueta), "") 
      text(x = valores_barras - 14, y = bp, labels = etiquetas, pos = 4, cex = 1.2, col = "black")
    })
    
    output$grafico3 <- renderPlot({
      datos_jugador <- subset(data_2021_2022, Jugador == input$jugador)
      valores_barras <- c(datos_jugador$percentil_faltas, datos_jugador$percentil_TirosIntercep, datos_jugador$percentil_interceptaciones,
                          datos_jugador$percentil_entradas, datos_jugador$percentil_duelos_aereos, datos_jugador$percentil_duelos_defensivos,
                          datos_jugador$percentil_ADR)
      nombres_barras <- c("Faltas", "Tir int", "Inter", "Ent", "Duel A", "Duel D", "Acc Def")
      par(bg="#f2ecd3")
      bp <- barplot(valores_barras, horiz = TRUE, 
                    names.arg = nombres_barras, xlab = "Percentil", ylab = NULL,
                    main = "Estadísticas defensivas", col = "#617ded",
                    xlim = c(0,100))
      
      valores_etiqueta <- round(valores_barras, 2) 
      etiquetas <- paste0(as.character(valores_etiqueta), "") 
      text(x = valores_barras - 14, y = bp, labels = etiquetas, pos = 4, cex = 1.2, col = "black")
    })
    
    
    output$grafico4 <- renderPlot({
      
      datos_filtrados <- DumbBell1 %>%
        filter(Jugador %in% c(input$jugador, input$jugador2))
      
      
      ggplot(datos_filtrados, aes(x = Valor, y = Variable)) +
        geom_line() +
        geom_point(aes(color = Jugador), size = 4) + 
        geom_label_repel(aes(label = round(Valor, 1)), size = 3, nudge_x = 0.1, 
                         point.padding = 0.5, color = "black", fill = "#f2efe1") +
        scale_color_manual(values = c("#ffb700", "#6b3e99")) +
        labs(x = "", y = "") +
        theme(panel.background = element_rect(fill = "#f2efe1"),
              plot.background = element_rect(fill = "#f2ecd3"),
              legend.background = element_rect(fill = "#f2ecd3"),
              legend.key = element_rect(fill = "#f2ecd3"),
              legend.position = "bottom")
      
      
    })
    
    output$grafico5 <- renderPlot({
      
      datos_filtrados <- DumbBell2 %>%
        filter(Jugador %in% c(input$jugador, input$jugador2))
      
      
      ggplot(datos_filtrados, aes(x = Valor, y = Variable)) +
        geom_line() +
        geom_point(aes(color = Jugador), size = 4) +
        geom_label_repel(aes(label = round(Valor, 1)), size = 3, nudge_x = 0.1, 
                         point.padding = 0.5, color = "black", fill = "#f2efe1") +
        scale_color_manual(values = c("#ffb700", "#6b3e99")) +
        labs(x = "", y = "") +
        theme(panel.background = element_rect(fill = "#f2efe1"),
              plot.background = element_rect(fill = "#f2ecd3"),
              legend.background = element_rect(fill = "#f2ecd3"),
              legend.key = element_rect(fill = "#f2ecd3")) +
        theme(legend.position = "bottom")
    })
    
    output$grafico6 <- renderPlot({
      
      datos_filtrados <- DumbBell3 %>%
        filter(Jugador %in% c(input$jugador, input$jugador2))
      
      
      ggplot(datos_filtrados, aes(x = Valor, y = Variable)) +
        geom_line() +
        geom_point(aes(color = Jugador), size = 4) +
        geom_label_repel(aes(label = round(Valor, 1)), size = 3, nudge_x = 0.1, 
                         point.padding = 0.5, color = "black", fill = "#f2efe1") +
        scale_color_manual(values = c("#ffb700", "#6b3e99")) +
        labs(x = "", y = "") +
        theme(panel.background = element_rect(fill = "#f2efe1"),
              plot.background = element_rect(fill = "#f2ecd3"),
              legend.background = element_rect(fill = "#f2ecd3"),
              legend.key = element_rect(fill = "#f2ecd3")) +
        theme(legend.position = "bottom")
    })
    
}
