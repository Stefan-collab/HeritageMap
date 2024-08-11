### Before first usage, all steps in sites.R have to be performed once. 
### Then, the app can be started via "Run App" button in RStudio


library(leaflet); library(shiny); library(dplyr); library(tidyr); library(leaflet.extras)
library(WDI); library(mapview); library(ggplot2); library(scales); library(RColorBrewer)
library(plotly); library(shinycssloaders)


maxyear=as.double(substr(Sys.Date(),1,4))


#### ----- Initial setup ----
unesco_rawdat <- read.csv2("unesco_data.csv", na.strings=c("NA",""),
                           stringsAsFactors=FALSE, encoding="latin1")


maptypes = c("Esri.WorldStreetMap",# "Wikimedia",
             "OpenTopoMap",
             "Esri.WorldImagery",
             "NASAGIBS.ViirsEarthAtNight2012")

brewer_pal1 = c("#FFFFFF", brewer.pal(n=9, "Blues"))


function(input, output) {
  
  #### ----------- Tab1 (interactive map) -------------
  
  unesco_dat <- reactive({
    if(input$einfaerbung == ""){
      if(input$wer1 == "user1" | input$wer1 == "user2"){
        coln = which(colnames(unesco_rawdat) == input$wer1)
        refdat = as.numeric(substr(unesco_rawdat[,input$wer1],7,10))
  
        if(input$datumwahl2 == "DateVisited"){
          if(input$vonbis2 == "cumulated"){
            unesco_rawdat[(!is.na(unesco_rawdat[,coln]) &
                          refdat <= input$jahr2       &
                          unesco_rawdat$category %in% input$checkbox_tab1),]
          }else{
            unesco_rawdat[(!is.na(unesco_rawdat[,coln]) &
                           refdat == input$jahr2       &
                           unesco_rawdat$category %in% input$checkbox_tab1),]
          }
        }else{
          if(input$vonbis2 == "cumulated"){
            unesco_rawdat[(!is.na(unesco_rawdat[,coln]) &
                             refdat <= input$jahr2       &
                             unesco_rawdat$date_inscribed <= input$jahr2 &
                             unesco_rawdat$category %in% input$checkbox_tab1),]
          }else{
            unesco_rawdat[(!is.na(unesco_rawdat[,coln]) &
                             refdat <= input$jahr2       &
                             unesco_rawdat$date_inscribed <= input$jahr2 &
                             (refdat == input$jahr2       |
                             unesco_rawdat$date_inscribed == input$jahr2) &
                             unesco_rawdat$category %in% input$checkbox_tab1),]
          }
  
        }

      }else if(input$wer1 == "Alle"){
        if(input$vonbis1 == "cumulated"){
          unesco_rawdat[which(unesco_rawdat$date_inscribed <= input$jahr1 &
                              unesco_rawdat$category %in% input$checkbox_tab1),]
        }else{
          unesco_rawdat[which(unesco_rawdat$date_inscribed == input$jahr1 &
                              unesco_rawdat$category %in% input$checkbox_tab1),]
        }
      }
    }else if(input$einfaerbung == "DateInscribed"){
      if(input$wer1.E == "user1" | input$wer1.E == "user2"){
        coln = which(colnames(unesco_rawdat) == input$wer1.E)
        unesco_rawdat[(!is.na(unesco_rawdat[,coln])), ]
      }else if(input$wer1.E == "Alle"){
        unesco_rawdat
      }
    }else{
      unesco_rawdat
    }
  })


  output$MapPlot1 <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(maptypes[1], group="default") %>%
    addProviderTiles(maptypes[2], group="Topo") %>%
    addProviderTiles(maptypes[3], group="Satellit") %>%
    addProviderTiles(maptypes[4], group="Nacht") %>%
    addSearchOSM %>%
    addResetMapButton() %>%
      setView(lng=0, lat=25, zoom = 2)
  })

  observe({
    if(input$einfaerbung == "Type"){
      if(any(unesco_dat()$category %in% input$checkbox_tab1)){
        leafletProxy("MapPlot1") %>% clearMarkers() %>%
          addCircleMarkers(
            lng = unesco_dat()$longitude,
            lat = unesco_dat()$latitude,
            label = as.character(unesco_dat()$name),
            popup = if(input$wer1 == "Alle"){
                      paste("<h6><font color='black'>", as.character(unesco_dat()$name), "</font></h4>",
                        "<b>Welterbe seit:</b>", unesco_dat()$date_inscribed,
                        br(), br(), "<b>Beschreibung:</b>", unesco_dat()$description)
                    }else{
                      paste("<h6><font color='black'>", as.character(unesco_dat()$name), "</font></h4>", 
                        "<b>Welterbe seit:</b>", unesco_dat()$date_inscribed, br(),
                        "<b>Besucht am:</b>", unesco_dat()[,input$wer1],
                        br(), br(), "<b>Beschreibung:</b>", unesco_dat()$description)
                    },
            popupOptions = popupOptions(closeOnClick=T, autoPan=T),
            radius = 6,
            fillColor = if(input$dangerList){
                          unesco_dat()$color2
                        }else{
                          unesco_dat()$color
                        },
            color = "black",
            stroke = T,
            weight = 1,
            fillOpacity = 0.85
          ) %>%
          addLayersControl(baseGroups = c("default","Topo","Satellit","Nacht"))
      }else{leafletProxy("MapPlot1") %>% clearMarkers()}
      
    }else if(input$einfaerbung == "Visitor"){
      leafletProxy("MapPlot1") %>% clearMarkers() %>%
        addCircleMarkers(
          lng = unesco_dat()$longitude,
          lat = unesco_dat()$latitude,
          label = as.character(unesco_dat()$name),
          popup = paste("<h6><font color='black'>", as.character(unesco_dat()$name), "</font></h4>",
                        "<b>Welterbe seit:</b>", unesco_dat()$date_inscribed, br(),
                        ifelse(!is.na(unesco_dat()[,"user1"]), 
                                      paste("<b>DateVisited user1:</b>", unesco_dat()[,"user1"], "<br/>"), ""),
                        ifelse(!is.na(unesco_dat()[,"user2"]),
                                      paste("<b>DateVisited user2:</b>", unesco_dat()[,"user2"], "<br/>"), ""), 
                        br(), "<b>Beschreibung:</b>", unesco_dat()$description),
          popupOptions = popupOptions(closeOnClick=T,autoPan=T),
          radius = 6,
          fillColor = ifelse((!is.na(unesco_dat()$user1) & "user1" %in% input$checkbox_besucher), "blue", 
                             ifelse("user2" %in% input$checkbox_besucher &
                                      !("user1" %in% input$checkbox_besucher) &
                                      !is.na(unesco_dat()$user2),
                                    "red", "grey3")),
          color = ifelse((!is.na(unesco_dat()$user2) & "user2" %in% input$checkbox_besucher), 
                         "red", "grey3"),
          stroke = ifelse("user1" %in% input$checkbox_besucher & "user2" %in% input$checkbox_besucher, T, F),
          fillOpacity = 0.80
        ) %>%
        addLayersControl(baseGroups = c("default","Topo","Satellit","Nacht"))
      
    }else if(input$einfaerbung == "DateInscribed"){
      leafletProxy("MapPlot1") %>% clearMarkers() %>%
        addCircleMarkers(
          lng = unesco_dat()$longitude,
          lat = unesco_dat()$latitude,
          label = as.character(unesco_dat()$name),
          popup = paste("<h6><font color='black'>", as.character(unesco_dat()$name), "</font></h4>",
                        "<b>Welterbe seit:</b>", unesco_dat()$date_inscribed, br(),
                        ifelse(!is.na(unesco_dat()[,"user1"]), 
                               paste("<b>DateVisited user1:</b>", unesco_dat()[,"user1"], "<br/>"), ""),
                         ifelse(!is.na(unesco_dat()[,"user2"]),
                               paste("<b>DateVisited user2:</b>", unesco_dat()[,"user2"], "<br/>"), ""), 
                        br(), "<b>Beschreibung:</b>", unesco_dat()$description),
          popupOptions = popupOptions(closeOnClick=T,autoPan=T),
          radius = 6,
          fillColor = brewer_pal1[ceiling((maxyear - unesco_dat()$date_inscribed) /
                                          (maxyear - 1977) * 10)],
          color = "black",
          stroke = T,
          weight = 1,
          fillOpacity = 0.85
        ) %>%
        addLayersControl(baseGroups = c("default","Topo","Satellit","Nacht"))
      
    }
    
  })


  ### --------- Tab 2 (Table) -----------
  output$text <- renderUI({
    HTML(paste0("Sites total: ", nrow(unesco_rawdat)), '<br/>'#,
         )
  }) 
  
  output$tabelle1 <- renderTable({
    tabledat = data.frame(user1 = c(as.character(sum(unesco_rawdat["user1"]   != "", na.rm=T)),
                                   round(sum(unesco_rawdat["user1"]   != "", na.rm=T)/
                                     nrow(unesco_rawdat)*100,2)),
                          user2 = c(as.character(sum(unesco_rawdat["user2"]   != "", na.rm=T)),
                                    round(sum(unesco_rawdat["user2"]   != "", na.rm=T)/
                                      nrow(unesco_rawdat)*100,2))
    )
    rownames(tabledat)=c("Visited sites (Nb)","Visited Sites (%)")
      tabledat
  }, rownames = T)

  
  output$tabelle2 <- renderDataTable({
    tabledat = unesco_rawdat %>%
      filter( !(is.na(user1) & is.na(user2)) ) %>%
      mutate(user1 = as.numeric(substr(user1,7,10)),
             user2= as.numeric(substr(user2,7,10))) %>%
      transmute(Region = region, 
                Country = state, 
                Name = name,
                user1 = user1<=input$jahrT, 
                user2 = user2<=input$jahrT)
    
    if(input$aggregation == "Region"){
      tabledat = tabledat %>% 
        group_by(Region) %>%
        summarise(user1=sum(user1, na.rm=T), user2=sum(user2, na.rm=T)) %>%
        mutate(user1 = as.integer(user1),
               user2 = as.integer(user2)) %>%
        arrange(Region)
    }else if(input$aggregation == "Country"){
      tabledat = tabledat %>% 
        group_by(Region, Country) %>%
        summarise(user1=sum(user1, na.rm=T), user2=sum(user2, na.rm=T)) %>%
        mutate(user1 = as.integer(user1),
               user2 = as.integer(user2)) %>%
        arrange(Region, Country)
    }
    
    tabledat$user1  = ifelse(is.na(tabledat$user1), 0, tabledat$user1)
    tabledat$user2 = ifelse(is.na(tabledat$user2),0, tabledat$user2)
    tabledat
  })
  
  
  output$tabelle3 = renderDataTable({
    tabledat = unesco_rawdat %>%
      filter( !(is.na(user1) & is.na(user2)) ) %>%
       transmute(Region = region, 
                Country = state, 
                Name = name,
                user1 = as.Date(user1, "%d.%m.%Y"),
                user2 = as.Date(user2, "%d.%m.%Y"))
    tabledat     
    

  })
  
  
  
  ### --------- Tab 3 (comparison plots) -----------
  plotdat1 = unesco_rawdat %>%
    filter( !(is.na(user1) & is.na(user2)) ) %>%
    transmute(user1= as.numeric(substr(user1,7,10)),
              user2=as.numeric(substr(user2,7,10)))
  plotdat2 = unesco_rawdat %>%
    filter( !(is.na(user1) & is.na(user2)) ) %>%
    transmute(user1= as.Date(user1, "%d.%m.%Y"),
              user2=as.Date(user2, "%d.%m.%Y"))

  output$plot1 <- renderPlot({
      plotdat = data.frame(Jahr = 2002:maxyear)
      plotdat$user1 = sapply(plotdat$Jahr, function(x) sum(plotdat1$user1==x, na.rm=T))
      plotdat$user2= sapply(plotdat$Jahr, function(x) sum(plotdat1$user2==x, na.rm=T))

      p <- ggplot(gather(plotdat,wer,value,-Jahr), aes(x=Jahr, y=value)) +
        geom_bar(stat="identity", position="dodge", aes(fill=wer)) +
        scale_x_continuous(name=NULL, breaks=seq(2002,maxyear)) +
        labs(y=NULL)
      p

  })
   
  output$plot2 <- renderPlot({
      plotdat = data.frame(Date = as.Date(unique(c(plotdat2$user1,plotdat2$user2))))
      plotdat = plotdat %>%
        filter(!is.na(Date)) %>%
        arrange(Date) %>%
        mutate(user1 = sapply(Date, function(x) sum(x>=plotdat2$user1, na.rm=T)),
               user2= sapply(Date, function(x) sum(x>=plotdat2$user2,na.rm=T)))

      p = ggplot(gather(plotdat,wer,value,-Date), aes(x=Date, y=value, color=wer)) +
        geom_line() + labs(y=NULL, x=NULL) +
        scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"),
                     minor_breaks = date_breaks("years"))
      p
  
  })

  output$plot3 <- renderPlot({
    plotdat1 = unesco_rawdat %>%
      filter(!is.na(user1)) %>%
      select(name, longitude, latitude, region, category)
    plotdat1 = plotdat1 %>%
      mutate(distance = sapply(1:nrow(plotdat1), function (x) {
        dist(matrix(c(select(plotdat1,longitude,latitude)[x,],8.6821,50.1109),
                    nrow=2, byrow=T),method="manhattan")} ),
        wer="user1")
    plotdat2 = unesco_rawdat %>%
      filter(!is.na(user2)) %>%
      select(name, longitude, latitude, region, category)
    plotdat2 = plotdat2 %>%
      mutate(distance = sapply(1:nrow(plotdat2), function (x) {
        dist(matrix(c(select(plotdat2,longitude,latitude)[x,],8.1628,49.5630),
                    nrow=2, byrow=T),method="manhattan")} ),
        wer="user2")
    plotdat = full_join(plotdat1,plotdat2)

      p <- ggplot(plotdat, aes(x=region, y=distance, color=category), alpha=0.7) +
        facet_grid(~wer) +
        scale_color_manual(values=c("yellow","greenyellow","green")) +
        geom_jitter(width=0.2, height = 0) +
        labs(x = NULL)
      p

  })

  output$plot4 <- renderPlot({
    plotdat1 = unesco_rawdat %>%
      filter(!is.na(user1)) %>%
      select(name, longitude, latitude, date_inscribed)
    plotdat1 = plotdat1 %>%
      mutate(distance = sapply(1:nrow(plotdat1), function (x) {
                          dist(matrix(c(select(plotdat1,longitude,latitude)[x,],8.6821,50.1109),
                                      nrow=2, byrow=T),method="manhattan")} ),
             age = !!maxyear - date_inscribed,
             wer="user1")
    plotdat2 = unesco_rawdat %>%
      filter(!is.na(user2)) %>%
      select(name, longitude, latitude, date_inscribed)
    plotdat2 = plotdat2 %>%
      mutate(distance = sapply(1:nrow(plotdat2), function (x) {
                          dist(matrix(c(select(plotdat2,longitude,latitude)[x,],8.1628,49.5630),
                                      nrow=2, byrow=T),method="manhattan")} ),
            age = !!maxyear - date_inscribed,
            wer="user2")
    plotdat = full_join(plotdat1, plotdat2)
    
    p <- ggplot(plotdat, aes(x=distance, y=age, color=wer)) +
      facet_grid(~wer) +
      scale_color_manual(values=c("blue","red")) +
      geom_jitter(width=0.1, height = 0.1)
    p
  }) 
  

  
}
