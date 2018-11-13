function(input, output, session) {
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  #ras <- reactive({ subset(x, which(decades==input$dec)) })
  ras <- reactive({ x[[input$dec-1980]]})
  ras_vals <- reactive({ values(ras()) })
  pal <- reactive({ colorNumeric(pallete, ras_vals(), na.color="transparent") })
  #pal <- reactive({ colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), ras_vals(), na.color="transparent") })
  output$Map <- renderLeaflet({
    leaflet() %>% setView(lon, lat, 5) %>% addTiles() %>% addProviderTiles("CartoDB.Positron")%>%
      addCircleMarkers(data=locs, radius=6, color="black", stroke=FALSE, fillOpacity=0.5, group="locations", layerId = ~loc)
  })
  observe({
    proxy <- leafletProxy("Map")
    proxy %>% clearImages() %>% removeTiles(layerId="rasimg") %>% addRasterImage(ras(), colors=pal(), opacity=0.8, layerId="rasimg")
  })

  
  observe({ # show or hide location markers
    proxy <- leafletProxy("Map")
    if (input$show_communities) {
      proxy %>% showGroup("locations")
    } else {
      updateSelectInput(session, "location", selected="")
      proxy %>% hideGroup("locations") %>% removeMarker(layerId="Selected")
    }
  })
  observe({ # show or hide location markers
    proxy <- leafletProxy("Map")
    proxy %>% clearControls()
      proxy %>% removeTiles(layerId="ID") %>% addPolylines(data=shp, color = "black", weight = 2)
  })
  
  observeEvent(input$Map_marker_click, { # update the map markers and view on map clicks
    p <- input$Map_marker_click
    proxy <- leafletProxy("Map")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
    }
  })
  
  observeEvent(input$Map_marker_click, { # update the location selectInput on map clicks
    p <- input$Map_marker_click
    if(!is.null(p$id)){
      if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
    }
  })
  
  observeEvent(input$location, { # update the map markers and view on location selectInput changes
    p <- input$Map_marker_click
    p2 <- subset(locs, loc==input$location)
    proxy <- leafletProxy("Map")
    if(nrow(p2)==0){
      proxy %>% removeMarker(layerId="Selected")
    } else if(length(p$id) && input$location!=p$id){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    } else if(!length(p$id)){
      proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
    }
  })
  #d %>% filter(Location=="PRESA CARACOL")
  # @knitr server03pointdata
  Data <- reactive({ d %>% filter(Location==input$location) %>% mutate(MMC=Eronta*1000/(input$SED*10^6)) })
  output$TestPlot <- renderPlot({ plotERO(data = Data(),pronost = input$Pron,type=input$input_type,perio =input$Period,sediment=input$SED,traini=input$Lag)})
    output$TestTable <- renderDataTable({Data()}, options = list(pageLength=5))
    output$summary <- renderPrint({
      set.seed(10)
      if(input$input_type=="Erobrt") ero2<-Data()$Erobruta
      if(input$input_type== "Eroacum") ero2<-Data()$Eroacum
      if(input$input_type== "Eronta") ero2<-Data()$Eronta
      if(input$input_type== "MMC") ero2<-Data()$Eronta*1000/(input$SED*10^6)
      ero.zoo<-zoo(ero2,seq(as.Date("1981-01-01"),as.Date("2015-01-01"),"year"))
      #............
      range=as.numeric(substr(input$Period,1,4)):as.numeric(substr(input$Period,5,8))-1980
      #............
      mod_neural = nnetar(ero2[range], p=input$Lag, size=5)
      fr<-forecast(mod_neural,pronost)
      observado<-as.numeric(ero2)
      simulado<-as.numeric(fr$mean)
      simulado[simulado<0]=0
      simulado[simulado==0]<-quantile(ero2,probs=0.1)
      ##---Poner Mediana--##
      Me<-median(c(observado,simulado))
      dataset <- Me
      print(dataset)
    })
  # @knitr server03remainder
}

