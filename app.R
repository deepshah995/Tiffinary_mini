

library(shiny)
library(tidyr)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(reactable)
library(tableHTML)
library(geosphere)
library(reactlog)
library(shinycssloaders)
library(googlesheets4)
library(shinyalert)
library(shinyjs)
library(crosstalk)



gs4_auth(path = "tiffinary_credentials.json")
for (i in list.files("src") )
{ if(grepl("*.R",i)>0){
  source(paste0("src/",i))
  print(paste0("src/",i))}}


tiffin_list = readRDS("data/tiffin_list.rds")
postal_codes = readRDS("data/postal_codes.rds")

default_markers = tiffin_list %>%distinct(Areas.Delivered,.keep_all=TRUE)%>%select(Lat,Long)%>%distinct()

ui <- fluidPage( tags$style('.container-fluid {
                             background-color: #ffffff;
                              overflow-x: hidden; 
                           }'),
  useShinyjs(),
  #tags$script(type="text/javascript",'window.$crisp=[];window.CRISP_WEBSITE_ID="f4d7d86d-41c0-4229-8217-a3da95d782f9";(function(){d=document;s=d.createElement("script");s.src="https://client.crisp.chat/l.js";s.async=1;d.getElementsByTagName("head")[0].appendChild(s);})();'),

fluidRow(box(width="100%", leafletOutput("mymap", height =  "200", width = "100%"), tags$div(id = "main", style = "width: 75%"))),
br(),
fluidRow(  column(12, style = "padding-top: 25px; padding-bottom: 25px; background-color: #fff3f1;",box(width = 1),box(width =3,align = "center",searchInput("search_pin", label = "Enter your Postal Code", width = "80%" ,value = "",btnSearch = icon("search"),btnReset = icon("remove"),resetValue = "")), 
              box(width =3, align = "center", checkboxGroupButtons(
                inputId = "checkGroup",
                label = "Food Specifications",
                choiceValues = c(1,2),
                choiceNames = c("Veg","Non-Veg"),
                status = "default",
                checkIcon = list(
                  yes = icon("ok", 
                             lib = "glyphicon"),
                  no = icon("remove",
                            lib = "glyphicon"))
              )),
          

          box(width =4,align = "center",prettyRadioButtons(
            inputId = "radio",
            label = "Delivery/Pickup", 
            inline = TRUE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "info",
            animation = "jelly",
            choiceNames = c("Delivery", "Pickup"),
            choiceValues = c(1,2)
          ),
                                
                       box(width =12,id = "myBox", sliderInput(
             "slider", ticks = FALSE,
            label = "Distance for pickup:", 
            min = 0, max = 50,
            value = 25)
          ))
          )),
br(),
reactableOutput(outputId = "tiffins", width = "100%")%>% withSpinner(color="#0dc5c1"),
fluidRow(tags$div(align="center",tags$img(src = "https://tiffinary.com/wp-content/uploads/2021/04/move.png",width = "40vw", height="18vh"),tags$h5("Scrolll")))

)

server <- function(input, output, session) {
  
  
    points <- eventReactive(input$search_pin,{
      tiffin_list %>% filter(Postal.Code == pc_clean(input$search_pin))%>% select(Lat,Long)%>%distinct() })
  

  pickup_data =  reactive({ 
  
  if(input$search_pin=="") {
    return(
      tiffin_list%>%filter(Veg_NonVeg%in%searchvegnonveg(input$checkGroup))%>%
             mutate(Postal.Code=toupper(Postal.Code))%>%  arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>% filter(!is.na(Location))%>%
             arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>%
             select(Image.Files, Tiffin.Service, Location, All_Areas_Delivered, Slug, Daily, Weekly, Monthly, Duration, Delivery.Timing, Veg_NonVeg, Type.of.Cuisine,dist,TiffLat,TiffLong)%>%
             mutate(Slug= paste0("https://tiffinary.com/product/", Slug,"/")) %>% mutate(dist= as.character("Postal Code not provided"))
           )
  } else {
    
    input_Lat = postal_codes%>%filter(Postal.Code==pc_clean(input$search_pin))%>%pull(Lat)
    input_Long = postal_codes%>%filter(Postal.Code==pc_clean(input$search_pin))%>%pull(Long)
    if(length(input_Lat)==0){input_Lat=0}
    if(length(input_Long)==0){input_Long=0}
    p1 = tiffin_list%>%filter(!is.na(Location))%>%distinct(Tiffin.Service,.keep_all=TRUE)%>%arrange(Tiffin.Service)%>%select(TiffLong,TiffLat)
    p2 = c(input_Long,input_Lat)
    only_tiffinswith_location =cbind((tiffin_list%>%filter(!is.na(Location))%>%distinct(Tiffin.Service,.keep_all=TRUE)%>%arrange(Tiffin.Service)%>%select(Tiffin.Service)),
                                     as.data.frame(distVincentyEllipsoid(p1,p2)/1000 ))%>%rename(dist=2)
 
    return(tiffin_list%>%filter(Veg_NonVeg%in%searchvegnonveg(input$checkGroup))%>%
             mutate(Postal.Code=toupper(Postal.Code))%>%  arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>% filter(!is.na(Location))%>%
             select(-dist)%>% left_join(only_tiffinswith_location, by="Tiffin.Service")%>%filter(as.numeric(dist)<=input$slider)%>%
             select(Image.Files, Tiffin.Service, Location, All_Areas_Delivered, Slug, Daily, Weekly, Monthly, Duration, Delivery.Timing, Veg_NonVeg, Type.of.Cuisine,dist,TiffLat,TiffLong)%>%
             arrange(dist)%>%mutate(Slug= paste0("https://tiffinary.com/product/", Slug,"/"))%>%mutate(dist = paste0(as.integer(dist)," kms")))
  }
  })
  

delivery_data = reactive({
    if(input$search_pin=="") {
      tiffin_list%>%filter(Veg_NonVeg%in%searchvegnonveg(input$checkGroup))%>%filter(Pickup.Delivery%in%pickup_delivery(input$radio))%>%
        mutate(Postal.Code=toupper(Postal.Code))%>% mutate(check = ifelse(is.na(Radius_kms)| dist<=Radius_kms, 1,0))%>%filter(check == 1)%>%
        select(Image.Files, Tiffin.Service, Location, All_Areas_Delivered, Slug, Daily, Weekly, Monthly, Duration, Delivery.Timing, Veg_NonVeg, Type.of.Cuisine,dist,TiffLat,TiffLong)%>%
        arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>% 
        mutate(Slug= paste0("https://tiffinary.com/product/", Slug,"/"))
    } else {
     tiffin_list%>%filter(Postal.Code == pc_clean(input$search_pin))%>%filter(Veg_NonVeg%in%searchvegnonveg(1))%>%
        mutate(Postal.Code=toupper(Postal.Code))%>% mutate(check = ifelse(is.na(Radius_kms)| dist<=Radius_kms, 1,0))%>%filter(check == 1)%>%
        select(Image.Files, Tiffin.Service, Location, All_Areas_Delivered, Slug, Daily, Weekly, Monthly, Duration, Delivery.Timing, Veg_NonVeg, Type.of.Cuisine,dist,TiffLat,TiffLong)%>%
        arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)%>% 
        mutate(Slug= paste0("https://tiffinary.com/product/", Slug,"/"))
      } 
    })

pickup_no_pin_data = reactive({
  tiffin_list%>%filter(Veg_NonVeg%in%searchvegnonveg(input$checkGroup))%>%filter(Pickup.Delivery%in%pickup_delivery(input$radio))%>%
                   mutate(Postal.Code=toupper(Postal.Code))%>% mutate(check = ifelse(is.na(Radius_kms)| dist<=Radius_kms, 1,0))%>%filter(check == 1)%>%
                   arrange(Location)%>%distinct(Tiffin.Service,.keep_all=TRUE)                                                                                
})

all_delivery_data = tiffin_list %>%distinct(Areas.Delivered,.keep_all=TRUE)%>%select(Lat,Long)%>%distinct()
observe({
  if(input$radio == 1){
    output$mymap <- renderLeaflet({
      leaflet() %>%addTiles()%>% 
        addMarkers(lng = points()$Long, lat= points()$Lat ) %>% 
        addMarkers( lng = all_delivery_data$Long, lat = all_delivery_data$Lat, icon=LogoIcon ) })
  } else {
    if(input$search_pin==""){
      output$mymap <- renderLeaflet({
        leaflet()%>%addTiles()%>% setView(lat = 43.651070, lng = -79.347015, zoom = 7)%>%
          addMarkers(lng = points()$Long, lat= points()$Lat ) %>% 
          addMarkers( lng = pickup_no_pin_data()$TiffLong, lat = pickup_no_pin_data()$TiffLat, icon=LogoIcon )
        })
    } else {
      output$mymap <- renderLeaflet({
        leaflet() %>%addTiles()%>% 
          addMarkers(lng = points()$Long, lat= points()$Lat ) %>% 
          addMarkers( lng = pickup_data()$TiffLong, lat = pickup_data()$TiffLat, icon=LogoIcon ) }) 
    }  
  } 
})


  
observeEvent(input$radio, {
  
  if(input$radio == 1){
    shinyjs::hide(id = "myBox")
    output$tiffins = renderReactable({
      Delivery_Table(delivery_data())
    })
  }else{
    shinyjs::show(id = "myBox")
    output$tiffins = renderReactable({
      Pickup_Table(pickup_data())
    })
    }
}) 


}

shinyApp(ui, server)
