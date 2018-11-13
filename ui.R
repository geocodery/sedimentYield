bootstrapPage(
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("Map", width="100%", height="100%"),
  absolutePanel(top=10, right=10,
                sliderInput("dec", "Year", min=min(decades), max=max(decades), value=decades[1], step=1, sep="", post="s",ticks = T), # NEW LINE
                checkboxInput("show_communities", "Mostrar puntos ingresados", TRUE),
                #checkboxInput("legend", "Mostrar legenda", TRUE), # NEW LINE
                conditionalPanel("input.show_communities == true",
                                 selectInput("location","Escojer ubicacion de analisis", c("", locs$loc), selected=""),
                                 conditionalPanel("input.location !== null && input.location !== ''",
                                                  actionButton("button_plot_and_table", "View Plot/Table", class="btn-block"))
                ),
                selectInput("input_type", "Input type",
                            c("Erobrt", "Eroacum", "Eronta","MMC")),
                numericInput("Period", "Periodo a entrenar", 19812015,min =19811981, max = 20402040),
                numericInput("Lag","Numero de lag utilizados como entrada:", 8,min = 0, max = 200),
                numericInput("Pron", "Pronostico:", 10,min = 1, max = 200),
                numericInput("SED", "Ingresar la densidad del sedimento (Kg / m3): ",1500,min=1,max=10000)
                #checkboxInput("shp", "Mostrar cuenca", F), # Vertientes
                
      ),
      bsModal("Plot_and_table", "Plot and Table", "button_plot_and_table", size = "large",
              plotOutput("TestPlot"),
          dataTableOutput("TestTable"),
          verbatimTextOutput("summary")
  ),
  
  absolutePanel(bottom=1, left = 1, width = 200,
                # Header and Logo Div
                #headerPanel("SISTEMA DE ESTIMACION DE SEDIMENTOS EN LA VERTIENTE DEL PACIFICO"),
                div(style="background:#fffff; opacity:0.7",
                    # Logo
                    a( href = "http://www.ana.gob.pe/",target = "_blank",
                       img(src = "ana.png",width = "155px", height= "50px"))
                )
                # References Div
                ),
  absolutePanel(bottom=60, left = 10, width = 200,
                # Header and Logo Div
                #headerPanel("SISTEMA DE ESTIMACION DE SEDIMENTOS EN LA VERTIENTE DEL PACIFICO"),
                div(style="background:#fffff; opacity:0.7",
                    # Logo
                    a( href = "http://www.ana.gob.pe/",target = "_blank",
                       img(src = "PALETA.png",width = "200px", height= "200px"))
                )
                # References Div
  ),

  absolutePanel(top=1, left = 100, width = 700,
                # Header and Logo Div
                div(style="background:#fffff; opacity:1",
                    headerPanel(h4("SISTEMA DE ESTIMACION DE SEDIMENTOS EN LA VERTIENTE DEL PACIFICO", align = "center", 
                                style = "font-family: 'Lobster', bolt;
                                font-weight: 600; line-height: 1.1; 
                                color: #black;
                                "
                                )
                   )
                )
                # References Div
  )
  
)


