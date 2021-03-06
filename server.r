library(leaflet)
library(sp)
library(ggplot2)
library(shiny)
library(SDMTools)


#dir<-"C:/Users/anderstn/Desktop/RStudio/MangoApp/DM" #change this to your directory
dir<-"C:/Users/Zhenglin Wang/Documents/testApp/data" #change this to your directory
temp <- list.files(pattern="*.csv", path = dir, full.names = TRUE)
importDM<-do.call("rbind", lapply(temp, read.csv, header = TRUE))
rawDM <- as.data.frame(importDM)

rawDM$Created.Date <- as.Date(rawDM$Created.Date...Time, format="%d/%m/%Y")
rawDM$Week <- strftime(rawDM$Created.Date,format="%W")
rawDM$True.Date <- rawDM$Created.Date
rawDM$Created.Date <- rawDM$Created.Date - 1

#This is amended DM script, delete if user does not wish to use
#and change filly$DryMatter=rawDM$Prediction.1.Value
maxDate<-Sys.Date() #calculates most recent date
rawDM$amendDate<-rawDM$Created.Date-maxDate # calculates the days difference between scan date and maxDate
rawDM$incrementDM<-as.numeric((rawDM$amendDate)) #this is assuming 0.1DM is daily increase
#would want to have 0.1 switched to a user selection from interface
rawDM$DMamendV<-sqrt(rawDM$incrementDM^2)# removes the negation
rawDM$DMamendV <-rawDM$DMamendV
rawDM$DMamended<-rawDM$Prediction.1.Value+(rawDM$DMamendV*0.1)
#if the DM amend is 50
#this means 50 days, it will not be amended and will be classified as old data
# prodates the DM data to an estimated value based on maxDate
#for ex: maxDate = 23/11/2015    20/11/2015 DM= 15 -> DM= 15.3

filly <- data.frame(
  Date=rawDM$Created.Date,
  DryMatter=rawDM$DMamended,
  Lat=rawDM$Latitude,
  Long=rawDM$Longitude,
  Week=rawDM$Week,
  Amend=rawDM$DMamendV
)

hunk <- filly 

#setwd("C:/Users/anderstn/Desktop/RStudio/MangoApp/Paddocks")
setwd("C:/Users/Zhenglin Wang/Documents/testApp/data")

filenames<-list.files()
paddocknames <- substr(filenames,4,5)
filenumbers<-seq(filenames)
b <- 0
for(filenumber in filenumbers){ b <- b+1
#browser()
paddock1 <- as.matrix(read.csv(filenames[filenumber],
                               header=TRUE, colClasses=c("NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA)), colnames = "long", "lat")
tin <- ifelse(nchar(filenames[filenumber])<10, substr(filenames[filenumber], 1, nchar(filenames[filenumber])-4),  substr(filenames[filenumber], 6, nchar(filenames[filenumber])-4))
if (b==1) {blkname<-tin}else{blkname <- c(blkname,tin)}
trigger <- hunk[c(4,3)]
tent<- pnt.in.poly(trigger,paddock1)
hunk["trees"] <- tent$pip
hunk["paddock"] <- as.numeric(paddocknames[b])*hunk$trees
hunk$trees <- hunk$trees*hunk$DryMatter
treesinpad <- hunk[-which(hunk$paddock==0),]
AvDM <- sum(hunk$trees)/sum(tent$pip)
if(is.nan(AvDM)){AvDM<-0}else{AvDM<-AvDM}
p1 = Polygon(paddock1)
ps1 = Polygons(list(p1),paddocknames[filenumber])
if (filenumber == 1) {sp <- c(ps1)
paddAv <- AvDM
paddtrees <- treesinpad
} else {sp <- c(sp,ps1)
paddAv <- c(paddAv,AvDM)
paddtrees <- rbind(paddtrees,treesinpad)
}
hunk$trees <- NULL
hunk$paddock <- NULL
}

shinyServer(
  function(input, output) {
    paddtrees <<- paddtrees
    sps <<- SpatialPolygons(sp)
    
    output$MangoMap <- renderLeaflet({
      leaflet(sps) %>% setView(lng = 131.1828, lat = -12.7386, zoom = 15) %>% addTiles() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addPolygons(fillOpacity = 0.3,smoothFactor = 0.2, 
                    color = "green")
    })
    
    output$TestMangoMap <- renderLeaflet({
      leaflet(sps) %>% setView(lng = 131.1828, lat = -12.7386, zoom = 15) %>% addTiles() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addPolygons(fillOpacity = 0.3,smoothFactor = 0.2, 
                    color = "red")
    })
    
    observe({ 
      output$DateRange <- renderText({
        # make sure end date later than start date
        validate(
          need(input$dateRange[2] > input$dateRange[1], "The date selection results in an end date earlier than the start date, please change your date selection."
          )
        )})
      
      start <- input$dateRange[1]
      end <- input$dateRange[2]
      inc <- input$increment
      pts <<- paddtrees[which(paddtrees$Date>start&paddtrees$Date<end),]
      pts$DryMatter <- pts$DryMatter+(pts$Amend*inc)
      pts$DryMatter <- round(pts$DryMatter, digits=2)
      if(start>=end||input$treechoice==2||nrow(pts)==0){leafletProxy("MangoMap") %>% clearMarkers()
      }else{leafletProxy("MangoMap", data=pts)%>%  
          clearMarkers()%>%
          addCircleMarkers(lng = pts$Long, lat = pts$Lat, color = ifelse(pts$DryMatter>input$dmStdrd,"blue","red"), radius = 5, popup = paste0("Dry Matter ", pts$DryMatter, "%"))
      }})
    
    observe({
      start <- input$dateRange[1]
      end <- input$dateRange[2]
      inc <- input$increment
      pts <<- paddtrees[which(paddtrees$Date>start&paddtrees$Date<end),]
      pts$DryMatter <- pts$DryMatter+(pts$Amend*inc)
      pts$DryMatter <- round(pts$DryMatter, digits=2)
      start1 <- as.character(format(input$dateRange[1], format="%B %d %Y"))
      end1 <- as.character(format(input$dateRange[2], format="%B %d %Y"))
      quip <- pts[which(paddtrees$Date>start&paddtrees$Date<end),]
      
      
      
      if(start>=end||input$DMchoice==2||nrow(quip)==0){leafletProxy("MangoMap", data=sps) %>%
          setView(lng=mean(as.numeric(quip$Long)),lat=mean(as.numeric(quip$Lat)),zoom=14)%>%
          clearShapes()%>%
          addPolygons(fillOpacity = 0.3,smoothFactor = 0.2, color = "green")                                                                            
      }else{
        quippy <- as.numeric(paddocknames)  #unique(paddtrees$paddock)
        d <- 0
        for(i in 1:length(quippy)){ d <- d+1
        bugy <- quip[which(quip$paddock==quippy[d]),]
        if(nrow(bugy)==0){tye <- 0}else{
          tye <- mean(bugy$trees)}
        #if(is.nan(tye)){tye<-0}else{tye<-tye}
        nipy <- nrow(bugy)
        if (d == 1) {
          tyeAv <- tye
          numtrees <- nipy
        } else {
          tyeAv <- c(tyeAv,tye)
          numtrees <- c(numtrees,nipy)}
        }
        blippy <- tyeAv
        tyeAv <- round(tyeAv, digits=2)
        tyeAv <- as.character(tyeAv)
        pal <- colorNumeric(
          palette = c("tan","chocolate"),
          domain = c(14,16))
        leafletProxy("MangoMap", data=sps)%>%  
          clearShapes()%>%
          addPolygons(fillOpacity = 0.3,smoothFactor = 0.2, popup = ifelse(blippy==0, paste0("There were no trees sampled in ", blkname,
                                                                                             " between ", start1," and ", end1), 
                                                                           paste0("Average dry matter in ", blkname, " between ", start1," and ", end1, 
                                                                                  " was ", tyeAv, "% based on sampling ",numtrees," fruit(s).")), 
                      color = ifelse(blippy>input$dmStdrd,"green", ifelse(blippy==0, "grey", ifelse(blippy>(input$dmStdrd-1), "yellow", "red"))))
      }})
    observe({
      a <- which(blkname==input$e0)
      start <- input$dateRange[1]
      end <- input$dateRange[2]
      confInt <- input$CI
      inc <- input$increment
      output$Name <- renderText(paste0("Overview of ",input$e0," block"))
      start1 <- as.character(format(input$dateRange[1], format="%B %d %Y"))
      end1 <- as.character(format(input$dateRange[2], format="%B %d %Y"))
      output$Head <- renderText(paste0("Distribution of dry matter across ", input$e0, " between ",start1, " and ", end1))
      pts$DryMatter <- pts$DryMatter+(pts$Amend*inc)
      pts$DryMatter <- round(pts$DryMatter, digits=2)
      grap <- pts[which(pts$Date>start&pts$Date<end),]
      grap <- grap[which(grap$paddock==a),]
      grap$DryMatter<- round(grap$DryMatter, digits=2)
      yup <- round(mean(grap$DryMatter), digits=2)
      nump <- nrow(grap)
      grapdevdm <- grap[which(grap$Amend==0),]
      grapdev <- sd(grapdevdm$DryMatter)
      grapsampnum <- nrow(grapdevdm)
      grapdevmean <- mean(grapdevdm$DryMatter)
      cv <- (grapdev/grapdevmean)*100
      sampReq <- (1.7*1.7)*(cv*cv)/((100-confInt)^2)
      
      

      
      if(input$treechoice==2){
        leafletProxy("MangoMap", data=grap)%>% 
          clearMarkers()%>%
          setView(lng = mean(as.numeric(grap$Long)), mean(as.numeric(grap$Lat)), zoom=16)%>%
          addMarkers(mean(as.numeric(grap$Long)), mean(as.numeric(grap$Lat)), popup = ifelse(nump==0,paste0("There were no trees sampled in ", input$e0,
                                                                                                            " between ", start1," and ", end1), 
                                                                                             paste0("Average dry matter in ", input$e0, " between ", start1," and ", end1, 
                                                                                                    " was ", yup, "% based on sampling ",nump," fruit(s).")))}else{}
      if(nrow(grap)==0){output$paddrymatter <- renderPlot({
        plot(8,15,col="white",xlab= "Dry Matter", ylab = "Number of Trees")
        text(8,15,"Warning No Tree Data Recorded For This Period")
      })
      output$treenum <- renderValueBox({
        valueBox(
          "Warning", "Zero Trees",
          icon = icon("thumbs-down", lib = "glyphicon"),
          color = "red"
        )
      })
      output$dayBox <- renderValueBox({
        valueBox(
          paste0(end-start), "Sample Days", icon = icon("list"),
          color = "orange"
        )
      })
      output$meanBox <- renderValueBox({
        valueBox(
          "Warning", "No Average Dry Matter", icon = icon("thumbs-down", lib = "glyphicon"), color = "red"
        )
      })
      
      }else{output$paddrymatter <- renderPlot({
        
        
        hist(grap$DryMatter, col="green", main=NULL, xlab= "Dry Matter", ylab = "Number of Trees")
      })
      
      output$treenum <- renderValueBox({
        valueBox(
          paste0(nump), "Trees", if(nump>20){ 
            icon = icon("thumbs-up", lib = "glyphicon")}else
            {icon = icon("thumbs-down", lib = "glyphicon")},
          if(nump>20){ color = "green"} else {colour = "red"}
        )
      })
      
      output$dayBox <- renderValueBox({
        valueBox(
          paste0(end-start), "Sample Days", icon = icon("list"),
          color = "orange"
        )
      })
      output$meanBox <- renderValueBox({
        valueBox(
          paste0(round(mean(grap$DryMatter),2),"%"), "Average Dry Matter", if((mean(grap$DryMatter)>input$dmStdrd)){ 
            icon = icon("thumbs-up", lib = "glyphicon")}else
            {icon = icon("thumbs-down", lib = "glyphicon")},
          if((mean(grap$DryMatter)>input$dmStdrd)){ color = "green"} else {colour = "red"}
        )
      })
      }      
    })
  })