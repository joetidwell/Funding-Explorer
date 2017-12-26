library(shiny)
library(shinyBS)
library(shinydashboard)
library(ggplot2)
library(scales)
library(data.table)
theme_set(theme_classic())


# path.data <- file.path("~/git/District-Explorer/data")
# load(file.path(path.data,"districts.RData"))
# path.data <- file.path("~/git/WADA-Explorer/data")
# load(file.path(path.data,"mydata.RData"))

# path.data <- file.path("/srv/shiny-server/Funding-Explorer/data")
load("data/mydata.RData")

# exclude districts who don't collect taxes, i.e. charter and federal school
mydata <- mydata[`Local Taxes`>0]

dcolor <- c("steelblue","#EE8A12")
tcolor <- c("white","black")

# Define some equations
eq <- data.table(name=c("Local Taxes / ADA",
                        "Total Revenue/WADA",
                        "(Total Revenue-Chap41)/WADA",
                        "(Total Revenue-Chap41)/ADA"),
                 form=c("`Local Taxes` / ADA",
                        "(`Total Revenue`)/WADA",
                        "(`Total Revenue`-Chap41)/WADA",
                        "(`Total Revenue`-Chap41)/ADA"),
                 desc=c("Local Taxes / ADA",
                        "Total Revenue per WADA",
                        "Total Revenue - Chapter 41 per WADA",
                        "(Total Revenue-Chap41)/ADA"))


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Some functions
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fplot1 <- function(input, txtsize=1, group="gsa", title="Greater San Antonio") {
        groupidx <- unlist(mydata[,group,with=FALSE])
        data <- mydata[groupidx & year==input$year,]
        data[,color:="grey"]
        data[`District Name`==input$primary_district, color:=dcolor[1]]
        data[`District Name`==input$secondary_district, color:=dcolor[2]]
        # data[,value:=ADA]
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        data[,hjust := ifelse(value > 0, "left", "right")]
    
    
    
        ggplot(data, aes(x=reorder(CDN, value), 
                         y=value,
                         label=gsub(" ISD","",`District Name`),
                         hjust=hjust)) +
          geom_bar(fill=data$color[order(data$value)], position="dodge", stat="identity") +
          coord_flip() +
          # scale_fill_manual(values=c("grey","#EE8A12","steelblue")) +
          # scale_x_discrete(labels=gsub(" ISD", "", data[order(value)]$`District Name`)) +
          expand_limits(y = 0) +
          scale_y_continuous(expand = c(0, 0)) +
          labs(x="", title=title, y=paste0(data$year[1]," : ",input$formula)) +
          guides(fill=FALSE, color=FALSE) +
          geom_text(aes(y=0, color=color), size=txtsize) +
          scale_color_manual(values=c("black","black","white")) +
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
}


tsfplot1 <- function(input, txtsize=1, inflation=FALSE) {
        data <- mydata[gsa==TRUE]
        data[,color:="grey"]
        data[`District Name`==input$primary_district, color:=dcolor[1]]
        data[`District Name`==input$secondary_district, color:=dcolor[2]]
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        if(inflation) {
          data[,value:=value*inf.factor]
        }
        data[,hjust := ifelse(value > 0, "left", "right")]
    
        tsdata <- data[,as.list(quantile(value,c(.25,.5,.75))),by=year]
        # tsdata <- melt(tsdata, id.var="year")
    
        alldata <- tsdata[,.(year,`50%`)]
        setnames(alldata, "50%", "value")
        alldata$`District Name` <- "Greater San Antonio"
        alldata <- rbind(alldata,
          data[`District Name` %in% c(input$primary_district,input$secondary_district),
             .(`District Name`,year,value)])
        alldata[,`District Name`:=ordered(`District Name`, 
                                          levels=c("Greater San Antonio",
                                                   input$primary_district,
                                                   input$secondary_district))]        

        ggplot(tsdata, aes(x=year, y=`25%`)) + 
          # geom_ribbon(aes(ymin=`25%`,ymax=`75%`), fill="lightgrey", alpha="0.5") +
          geom_line(data=alldata, aes(x=year, y=value, 
                                      group=`District Name`,
                                      color=`District Name`), size=1) +
          labs(title="Greater San Antonio",
              x="Year",
              y=paste(input$formula, ifelse(inflation, "   (2015 Dollars)", ""))) +
          scale_x_continuous(expand = c(0, 0), 
                             limits=c(min(data$year),max(data$year)+1),
                             breaks= pretty_breaks()) +
          scale_color_manual(name="",
                             values=c("black",dcolor)) +
          theme(legend.position="bottom")

}


fplot2 <- function(input, txtsize=3, title="Texas") {

        data <- mydata[year==input$year,]
        data[,color:="grey"]
        data[,tcolor:="black"]
        data[`District Name`==input$primary_district, color:=dcolor[1]]
        data[`District Name`==input$primary_district, tolor:="white"]
        data[`District Name`==input$secondary_district, color:=dcolor[2]]
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
    
        s <- sd(data$value)
        data <- data[value<(median(value) + 5*s)]    
        data <- data[value>(median(value) - 5*s)]    
        
        Fn <- ecdf(data$value)

        data[,percentile:=Fn(value)]
        ddata <- rbind(data[`District Name` == input$primary_district],
                       data[`District Name` == input$secondary_district])



        ggplot(data, aes(x=value,y=percentile)) +
          geom_line(size=1) +
          geom_label(data=ddata, aes(x=value,
                                     y=percentile, 
                                     label=gsub(" ISD","",`District Name`)),
                    color=ddata$tcolor, fill=ddata$color, size=3) +
          # scale_fill_manual(values=dcolor) +
          # scale_color_manual(values=tcolor) +
          expand_limits(y = 0) +
          scale_y_continuous(expand = c(0, 0)) +
          labs(y="Percentile", title=title, x=paste0(data$year[1]," : ",input$formula)) +
          guides(fill=FALSE, color=FALSE) 
          # theme(axis.title.y=element_blank(),
          #       axis.text.y=element_blank(),
          #       axis.ticks.y=element_blank())
}


tsfplot2 <- function(input, txtsize=1, inflation=FALSE, groups) {
        data <- mydata
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        if(inflation) {
          data[,value:=value*inf.factor]
        }
        data[,hjust := ifelse(value > 0, "left", "right")]
    
        tsdata.gsa <- data[gsa==TRUE,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata <- data[,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.41 <- data[Chap41>0,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.not41 <- data[Chap41==0,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.tsc <- data[TSC==TRUE,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.fgsd <- data[fgsd==TRUE,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.d1 <- data[`District Name`==input$primary_district,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.d2 <- data[`District Name`==input$secondary_district,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.houston <- data[houston==TRUE,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.dfw <- data[dfw==TRUE,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.austin <- data[austin==TRUE,as.list(quantile(value,c(.25,.5,.75))),by=year]
        tsdata.equity <- data[equity==TRUE,as.list(quantile(value,c(.25,.5,.75))),by=year]

        alldata <- rbind(tsdata[,.(year,`25%`,`50%`,`75%`,group="Texas")],
                         tsdata.41[,.(year,`25%`,`50%`,`75%`,group="Chapter 41")],
                         tsdata.not41[,.(year,`25%`,`50%`,`75%`,group="Non Chapter 41")],
                         tsdata.gsa[,.(year,`25%`,`50%`,`75%`,group="Greater San Antonio")],
                         tsdata.tsc[,.(year,`25%`,`50%`,`75%`,group="Texas School Coalition")],
                         tsdata.fgsd[,.(year,`25%`,`50%`,`75%`,group="Fast-Growth")],
                         tsdata.d1[,.(year,`25%`,`50%`,`75%`,group=input$primary_district)],
                         tsdata.austin[,.(year,`25%`,`50%`,`75%`,group="Greater Austin")],
                         tsdata.houston[,.(year,`25%`,`50%`,`75%`,group="Greater Houston")],
                         tsdata.dfw[,.(year,`25%`,`50%`,`75%`,group="Dallas-Fort Worth")],
                         tsdata.equity[,.(year,`25%`,`50%`,`75%`,group="Equity Center")])

        if(input$secondary_district!="NONE") {
          alldata <- rbind(alldata, tsdata.d2[,.(year,`25%`,`50%`,`75%`,group=input$secondary_district)])
 
          alldata[,group:=ordered(group, 
                                  levels=c("Texas",
                                           "Chapter 41",
                                           "Non Chapter 41",
                                           "Greater San Antonio",
                                           "Texas School Coalition",
                                           "Fast-Growth",
                                           input$primary_district,
                                           input$secondary_district,
                                           "Greater Austin",
                                           "Dallas-Fort Worth",
                                           "Greater Houston",
                                           "Equity Center"))]        

          dt.color <- data.table(group=levels(alldata$group),
                                 color=c("black",
                                         "tomato1",
                                         "orchid",
                                         "firebrick",
                                         "slateblue4",
                                         "forestgreen",
                                         dcolor[1],
                                         dcolor[2],
                                         "hotpink4",
                                         "thistle",
                                         "khaki4",
                                         "brown"))
        } else {

          alldata[,group:=ordered(group, 
                                  levels=c("Texas",
                                           "Chapter 41",
                                           "Non Chapter 41",
                                           "Greater San Antonio",
                                           "TSC",
                                           "Fast-Growth",
                                           input$primary_district,
                                           "Greater Austin",
                                           "Dallas-Fort Worth",
                                           "Greater Houston",
                                           "Equity Center"))]        

          dt.color <- data.table(group=levels(alldata$group),
                                 color=c("black",
                                         "tomato1",
                                         "orchid",
                                         "firebrick",
                                         "slateblue4",
                                         "forestgreen",
                                         dcolor[1],
                                         "hotpink4",
                                         "thistle",
                                         "khaki4",
                                         "brown"))
        }




        ggplot(alldata[group%in%groups], aes(x=year, y=`50%`, group=group)) +
          geom_line(aes(color=group), size=1) +
          # geom_ribbon(aes(ymin=`25%`, ymax=`75%`), fill="lightgrey", alpha=.35) +
          labs(x="Year",
              y=ifelse(inflation, "2015 Dollars", "Dollars"),
              title=paste(input$formula)) +
          scale_x_continuous(expand = c(0, 0), 
                             limits=c(min(data$year),max(data$year)+1),
                             breaks= pretty_breaks()) +
          scale_color_manual(name="",
                             values=dt.color[group%in%groups]$color) 


        # ggplot(tsdata, aes(x=year, y=`25%`)) + 
        #   geom_ribbon(aes(ymin=`25%`,ymax=`75%`), fill="lightgrey", alpha="0.5") +
        #   geom_ribbon(data=tsdata.41, aes(ymin=`25%`,ymax=`75%`), fill="lightgrey", alpha="0.5") +
        #   geom_ribbon(data=tsdata.not41, aes(ymin=`25%`,ymax=`75%`), fill="lightgrey", alpha="0.5") +
        #   geom_line(data=alldata, aes(x=year, y=`50%`, 
        #                               group=group,
        #                               color=group), size=1) +
        #   labs(title="Texas",
        #       x="Year",
        #       y=paste(input$formula, ifelse(inflation, "   (2015 Dollars)", ""))) +
        #   scale_x_continuous(expand = c(0, 0), 
        #                      limits=c(min(data$year),max(data$year)+1),
        #                      breaks= pretty_breaks()) +
        #   scale_color_manual(name="",
        #                      values=c("black","tomato1","orchid")) +
        #   theme(legend.position="bottom")

}

fplot3 <- function(input, txtsize=3, title="Chapter 41 Districts") {
        # data <- mydata[fgsd==TRUE & year==input$year,]
        data <- mydata[Chap41>0 & year==input$year,]
        data[,color:="grey"]
        data[,tcolor:="black"]
        data[`District Name`==input$primary_district, color:=dcolor[1]]
        # data[`District Name`==input$primary_district, tcolor:="white"]
        data[`District Name`==input$secondary_district, color:=dcolor[2]]
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        data[,hjust := ifelse(value > 0, "left", "right")]
    
        s <- sd(data$value)
        # data <- data[value<(median(value) + 5*s)]    
        # data <- data[value>(median(value) - 5*s)]    
        
        Fn <- ecdf(data$value)

        data[,percentile:=Fn(value)]
        ddata <- rbind(data[`District Name` == input$primary_district],
                       data[`District Name` == input$secondary_district])



        ggplot(data, aes(x=value,y=percentile)) +
          geom_line(size=1) +
          geom_label(data=ddata, aes(x=value,
                                     y=percentile, 
                                     label=gsub(" ISD","",`District Name`)),
                    color=ddata$tcolor, fill=ddata$color, size=3) +
          # scale_fill_manual(values=dcolor) +
          # scale_color_manual(values=tcolor) +
          expand_limits(y = 0) +
          scale_y_continuous(expand = c(0, 0)) +
          labs(y="Percentile", title=title, x=paste0(data$year[1]," : ",input$formula)) +
          guides(fill=FALSE, color=FALSE) 
          # theme(axis.title.y=element_blank(),
          #       axis.text.y=element_blank(),
          #       axis.ticks.y=element_blank())    
    


        # ggplot(data, aes(x=reorder(CDN, value), 
        #                  y=value,
        #                  label=gsub(" ISD","",`District Name`),
        #                  hjust=hjust)) +
        #   geom_bar(fill=data$color[order(data$value)], position="dodge", stat="identity") +
        #   coord_flip() +
        #   # scale_fill_manual(values=c("grey","#EE8A12","steelblue")) +
        #   # scale_x_discrete(labels=gsub(" ISD", "", data[order(value)]$`District Name`)) +
        #   expand_limits(y = 0) +
        #   scale_y_continuous(expand = c(0, 0)) +
        #   labs(x="", title="Chapter 41 Districts", y=paste0(data$year[1]," : ",input$formula)) +
        #   guides(fill=FALSE, color=FALSE) +
        #   geom_text(aes(y=0), color=data$tcolor[order(data$value)], size=txtsize) +
        #   scale_color_manual(values=c("black","black","white")) +
        #   theme(axis.title.y=element_blank(),
        #         axis.text.y=element_blank(),
        #         axis.ticks.y=element_blank())
}


fplot4 <- function(input, txtsize=3, title="TSC Districts") {
        data <- mydata[Chap41>0 & TSC==TRUE & year==input$year,]
        data[,color:="grey"]
        data[,tcolor:="black"]
        data[`District Name`==input$primary_district, color:=dcolor[1]]
        # data[`District Name`==input$primary_district, tcolor:="white"]
        data[`District Name`==input$secondary_district, color:=dcolor[2]]
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        data[,hjust := ifelse(value > 0, "left", "right")]
    
        s <- sd(data$value)
        # data <- data[value<(median(value) + 5*s)]    
        # data <- data[value>(median(value) - 5*s)]    
        
        Fn <- ecdf(data$value)

        data[,percentile:=Fn(value)]
        ddata <- rbind(data[`District Name` == input$primary_district],
                       data[`District Name` == input$secondary_district])



        ggplot(data, aes(x=value,y=percentile)) +
          geom_line(size=1) +
          geom_label(data=ddata, aes(x=value,
                                     y=percentile, 
                                     label=gsub(" ISD","",`District Name`)),
                    color=ddata$tcolor, fill=ddata$color, size=3) +
          # scale_fill_manual(values=dcolor) +
          # scale_color_manual(values=tcolor) +
          expand_limits(y = 0) +
          scale_y_continuous(expand = c(0, 0)) +
          labs(y="Percentile", title=title, x=paste0(data$year[1]," : ",input$formula)) +
          guides(fill=FALSE, color=FALSE) 
}

fplot5 <- function(input, txtsize=3, title="Fast Growth Districts") {
        data <- mydata[fgsd==TRUE & year==input$year,]
        data[,color:="grey"]
        data[,tcolor:="black"]
        data[`District Name`==input$primary_district, color:=dcolor[1]]
        # data[`District Name`==input$primary_district, tcolor:="white"]
        data[`District Name`==input$secondary_district, color:=dcolor[2]]
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        data[,hjust := ifelse(value > 0, "left", "right")]
    
        s <- sd(data$value)
        # data <- data[value<(median(value) + 5*s)]    
        # data <- data[value>(median(value) - 5*s)]    
        
        Fn <- ecdf(data$value)

        data[,percentile:=Fn(value)]
        ddata <- rbind(data[`District Name` == input$primary_district],
                       data[`District Name` == input$secondary_district])



        ggplot(data, aes(x=value,y=percentile)) +
          geom_line(size=1) +
          geom_label(data=ddata, aes(x=value,
                                     y=percentile, 
                                     label=gsub(" ISD","",`District Name`)),
                    color=ddata$tcolor, fill=ddata$color, size=3) +
          # scale_fill_manual(values=dcolor) +
          # scale_color_manual(values=tcolor) +
          expand_limits(y = 0) +
          scale_y_continuous(expand = c(0, 0)) +
          labs(y="Percentile", title=title, x=paste0(data$year[1]," : ",input$formula)) +
          guides(fill=FALSE, color=FALSE) 
}

fplot6 <- function(input, txtsize=3, title="Equity Districts") {
        data <- mydata[equity==TRUE & year==input$year,]
        data[,color:="grey"]
        data[,tcolor:="black"]
        data[`District Name`==input$primary_district, color:=dcolor[1]]
        # data[`District Name`==input$primary_district, tcolor:="white"]
        data[`District Name`==input$secondary_district, color:=dcolor[2]]
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        data[,hjust := ifelse(value > 0, "left", "right")]
    
        s <- sd(data$value)
        # data <- data[value<(median(value) + 5*s)]    
        # data <- data[value>(median(value) - 5*s)]    
        
        Fn <- ecdf(data$value)

        data[,percentile:=Fn(value)]
        ddata <- rbind(data[`District Name` == input$primary_district],
                       data[`District Name` == input$secondary_district])



        ggplot(data, aes(x=value,y=percentile)) +
          geom_line(size=1) +
          geom_label(data=ddata, aes(x=value,
                                     y=percentile, 
                                     label=gsub(" ISD","",`District Name`)),
                    color=ddata$tcolor, fill=ddata$color, size=3) +
          # scale_fill_manual(values=dcolor) +
          # scale_color_manual(values=tcolor) +
          expand_limits(y = 0) +
          scale_y_continuous(expand = c(0, 0)) +
          labs(y="Percentile", title=title, x=paste0(data$year[1]," : ",input$formula)) +
          guides(fill=FALSE, color=FALSE) 
}


shinyServer(function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)


   output$current_district <- renderText({ 
      if(dist1) {
        input$primary_district
      } else {
        input$secondary_district
      }
   })

   output$radioDist <- renderUI({
    mychoices <- list(1,2)
    names(mychoices) <- c(input$primary_district,input$secondary_district)
    radioButtons("radio", label = NULL,
              choices = mychoices, 
              selected = input$radio,
              inline=TRUE)
   })


   # output$dist_box <- renderUI({
   #  if(!is.null(input$radio)) {
   #    mystatus <- ifelse(input$radio==1, "primary", "warning")      
   #  } else {
   #    mystatus <- "primary"
   #  }

   #    box(title=textOutput("current_district"),
   #          radioButtons("radio", label = NULL,
   #            choices = list("Primary" = 1, "Secondary" = 2), 
   #            selected = input$radio,
   #            inline=TRUE),
   #          solidHeader=TRUE, 
   #          status=mystatus, width=3
   #        )
   # })

    GSAbox <- reactive({
      if(!is.null(input$radio) & !is.null(input$formula) & !is.null(input$selectPlot1)) {
        mycol <- ifelse(input$radio==1, "blue", "yellow")
        data <- switch(input$selectPlot1,
          "1" = mydata[austin==TRUE & year==input$year],
          "2" = mydata[dfw==TRUE & year==input$year],
          "3" = mydata[houston==TRUE & year==input$year],
          "4" = mydata[gsa==TRUE & year==input$year])

        title <- switch(input$selectPlot1,
          "1" = "In the Austin Area",
          "2" = "In the DFW Area",
          "3" = "In the Houston Area",
          "4" = "In the San Antonio Area")


        # data <- mydata[gsa==TRUE & year==input$year]
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        Fn <- ecdf(data$value)
        data[,per:=Fn(value)]
        if(input$radio==1) {
          txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")
        } else {
          txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")
        }

        if(txt=="%") txt <- "--%"


      } else {
        mycol <- "blue"
        txt<-"--%"
        title<-"---"
      }
        valueBox(txt, title, icon = icon("map-pin"), 
                 color=mycol)
     
      })


   statebox <- reactive({
      if(!is.null(input$radio)) {
        mycol <- ifelse(input$radio==1, "blue", "yellow")

        data <- mydata[year==input$year]
        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        Fn <- ecdf(data$value)
        data[,per:=Fn(value)]
        if(input$radio==1) {
          txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")
        } else {
          txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")
        }

        if(txt=="%") txt <- "--%"


      } else {
        txt<-"--%"
        mycol <- "blue"
      }
        valueBox(txt, "State-Wide", icon = icon("map"), 
                 color=mycol)  
      })

   Chap41box <- reactive({
      if(!is.null(input$radio)) {
        mycol <- ifelse(input$radio==1, "blue", "yellow")
        # data <- mydata[fgsd==TRUE & year==input$year]
        data <- mydata[Chap41 > 0 & year==input$year]

        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        Fn <- ecdf(data$value)
        data[,per:=Fn(value)]
        if(input$radio==1) {
          txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")
        } else {
          txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")
        }

        if(txt=="%") txt <- "--%"

      } else {
        txt<-"--%"
        mycol <- "blue"
      }
        # valueBox(txt, "In Fast Growth Districts", icon = icon("line-chart"), 
        valueBox(txt, "In Chapter 41 Districts", icon = icon("home"), 
                 color=mycol)
      })

   FGSDbox <- reactive({
      if(!is.null(input$radio)) {
        mycol <- ifelse(input$radio==1, "blue", "yellow")
        # data <- mydata[fgsd==TRUE & year==input$year]
        data <- mydata[fgsd==TRUE & year==input$year]

        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        Fn <- ecdf(data$value)
        data[,per:=Fn(value)]
        if(input$radio==1) {
          txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")
        } else {
          txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")
        }

        if(txt=="%") txt <- "--%"

      } else {
        txt<-"--%"
        mycol <- "blue"
      }
        # valueBox(txt, "In Fast Growth Districts", icon = icon("line-chart"), 
        valueBox(txt, "In Fast-Growth Districts", icon = icon("line-chart"), 
                 color=mycol)
      })

   TSCbox <- reactive({
      if(!is.null(input$radio)) {
        mycol <- ifelse(input$radio==1, "blue", "yellow")
        # data <- mydata[fgsd==TRUE & year==input$year]
        data <- mydata[TSC==TRUE & Chap41 > 0 & year==input$year]

        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        Fn <- ecdf(data$value)
        data[,per:=Fn(value)]
        if(input$radio==1) {
          txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")
        } else {
          txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")
        }

        if(txt=="%") txt <- "--%"

      } else {
        txt<-"--%"
        mycol <- "blue"
      }
        # valueBox(txt, "In Fast Growth Districts", icon = icon("line-chart"), 
        valueBox(txt, "In TSC Districts", icon = icon("group"), 
                 color=mycol)
      })

   equitybox <- reactive({
      if(!is.null(input$radio)) {
        mycol <- ifelse(input$radio==1, "blue", "yellow")
        # data <- mydata[fgsd==TRUE & year==input$year]
        data <- mydata[equity==TRUE & year==input$year]

        data[,value:=eval(parse(text=eq[name==input$formula]$form))]
        Fn <- ecdf(data$value)
        data[,per:=Fn(value)]
        if(input$radio==1) {
          txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")
        } else {
          txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")
        }

        if(txt=="%") txt <- "--%"

      } else {
        txt<-"--%"
        mycol <- "blue"
      }
        # valueBox(txt, "In Fast Growth Districts", icon = icon("line-chart"), 
        valueBox(txt, "In Equity Center Districts", icon = icon("balance-scale"), 
                 color=mycol)
      })



  output$vbox1 <- renderValueBox({
    if(!is.null(input$selectPlot1)) {
         switch(input$selectPlot1,  
               '1' = GSAbox(),
               '2' = GSAbox(),
               '3' = GSAbox(),
               '4' = GSAbox(),
               '5' = statebox(),
               '6' = Chap41box(),
               '7' = TSCbox(),
               '8' = FGSDbox(),
               '9' = equitybox())   
    }  
  })
   

  output$vbox2 <- renderValueBox({
    if(!is.null(input$selectPlot2)) {
         switch(input$selectPlot2,  
               '1' = GSAbox(),
               '2' = GSAbox(),
               '3' = GSAbox(),
               '4' = GSAbox(),
               '5' = statebox(),
               '6' = Chap41box(),
               '7' = TSCbox(),
               '8' = FGSDbox(),
               '9' = equitybox())   
    }
  })

  output$vbox3 <- renderValueBox({
    if(!is.null(input$selectPlot3)) {    
     switch(input$selectPlot3,  
               '1' = GSAbox(),
               '2' = GSAbox(),
               '3' = GSAbox(),
               '4' = GSAbox(),
               '5' = statebox(),
               '6' = Chap41box(),
               '7' = TSCbox(),
               '8' = FGSDbox(),
               '9' = equitybox())   
   }
  })


  # Change current district focus
  observeEvent(input$radio, {
    mycol <- ifelse(input$radio==1, "blue", "yellow")

    output$dist_box <- renderUI({
          box(title=textOutput("current_district"),
            radioButtons("radio", label = NULL,
              choices = list("Primary" = 1, "Secondary" = 2), 
              selected = input$radio),
              solidHeader=TRUE, 
              status=ifelse(input$radio==1, "primary", "warning"), 
              width=3
          )
    })
   # output$GSA <- renderValueBox({
   #      valueBox("25%", "In the San Antonio Area", icon = icon("map-pin"), 
   #               color=mycol)
   #    })
   # output$state <- renderValueBox({
   #      valueBox("35%", "State-Wide", icon = icon("map"), 
   #               color=mycol)  
   #    })
   # output$FGSD <- renderValueBox({
   #      valueBox("30%", "In Fast Growth Districts", icon = icon("line-chart"), 
   #               color=mycol)
   #    })



    output$current_district <- renderText({
      if(input$radio==1) {
        input$primary_district
      } else {
        input$secondary_district
      }
    })
  })
 


  # Drop-down selection box for which formula
  output$choose_formula <- renderUI({
    selectInput("formula", NULL, as.list(eq$name), selected="(Total Revenue-Chap41)/WADA")
  })

  # Drop-down selection box for which formula
  # output$choose_formula_time <- renderUI({
  #   selectInput("formula2", NULL, as.list(eq$name))
  # })

  # Drop-down selection box for which year
  output$choose_year <- renderUI({
    selectInput("year", NULL, as.list(unique(mydata$year)), selected=2014)
  })



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Greater San Antonio - Most Recent Year
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$plot1 <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      switch(input$selectPlot1,
             '1' = fplot1(input, txtsize=1, group="austin", title=""),
             '2' = fplot1(input, txtsize=1, group="dfw", title=""),
             '3' = fplot1(input, txtsize=1, group="houston", title=""),
             '4' = fplot1(input, txtsize=1, group="gsa", title=""),
             '5' = fplot2(input, txtsize=1, title=""),
             '6' = fplot3(input, txtsize=1, title=""),
             '7' = fplot4(input, txtsize=1, title=""),
             '8' = fplot5(input, txtsize=1, title=""),
             '9' = fplot6(input, txtsize=1, title=""))
      }
  })

  output$plot1L <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      switch(input$selectPlot1,
             '1' = fplot1(input, txtsize=3, group="austin", title="Greater Austin"),
             '2' = fplot1(input, txtsize=3, group="dfw", title="Dallas-Fort Worth"),
             '3' = fplot1(input, txtsize=3, group="houston", title="Greater Houston"),
             '4' = fplot1(input, txtsize=3, group="gsa", title="Greater San Antonio"),
             '5' = fplot2(input, txtsize=3, title="State-Wide"),
             '6' = fplot3(input, txtsize=3, title="Chapter 41 Districts"),
             '7' = fplot4(input, txtsize=3, title="Texas School Coalition"),
             '8' = fplot5(input, txtsize=3, title="Fast Growth Districts"),
             '9' = fplot6(input, txtsize=3, title="Equity Center Districts"))
    }
  })


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Greater San Antonio - Time Series
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$tsplot1 <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district2))) {
        tsfplot1(input, txtsize=1, ifelse(input$inflation==TRUE, TRUE, FALSE))
      }
  })

  output$tsplot1L <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district2))) {
        tsfplot1(input, txtsize=3,ifelse(input$inflation==TRUE, TRUE, FALSE))
      }
  })




####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Texas - Most Recent Year
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



  output$plot2 <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      switch(input$selectPlot2,
             '1' = fplot1(input, txtsize=1, group="austin", title=""),
             '2' = fplot1(input, txtsize=1, group="dfw", title=""),
             '3' = fplot1(input, txtsize=1, group="houston", title=""),
             '4' = fplot1(input, txtsize=1, group="gsa", title=""),
             '5' = fplot2(input, txtsize=1, title=""),
             '6' = fplot3(input, txtsize=1, title=""),
             '7' = fplot4(input, txtsize=1, title=""),
             '8' = fplot5(input, txtsize=1, title=""),
             '9' = fplot6(input, txtsize=1, title=""))
      }
  })

  output$plot2L <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      switch(input$selectPlot2,
             '1' = fplot1(input, txtsize=3, group="austin", title="Greater Austin"),
             '2' = fplot1(input, txtsize=3, group="dfw", title="Dallas-Fort Worth"),
             '3' = fplot1(input, txtsize=3, group="houston", title="Greater Houston"),
             '4' = fplot1(input, txtsize=3, group="gsa", title="Greater San Antonio"),
             '5' = fplot2(input, txtsize=3, title="State-Wide"),
             '6' = fplot3(input, txtsize=3, title="Chapter 41 Districts"),
             '7' = fplot4(input, txtsize=3, title="Texas School Coalition"),
             '8' = fplot5(input, txtsize=3, title="Fast Growth Districts"),
             '9' = fplot6(input, txtsize=3, title="Equity Center Districts"))
    }
  })



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Texas - Time Series
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  output$tsplot2 <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      tsfplot2(input, txtsize=3,ifelse(input$inflation==TRUE, TRUE, FALSE),input$tsgroups)
    }
  })

  output$tsplot2L <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      tsfplot2(input, txtsize=3,ifelse(input$inflation==TRUE, TRUE, FALSE),input$tsgroups)
    }
  })



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Greater San Antonio - Most Recent Year
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  output$plot3 <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      switch(input$selectPlot3,
             '1' = fplot1(input, txtsize=1, group="austin", title=""),
             '2' = fplot1(input, txtsize=1, group="dfw", title=""),
             '3' = fplot1(input, txtsize=1, group="houston", title=""),
             '4' = fplot1(input, txtsize=1, group="gsa", title=""),
             '5' = fplot2(input, txtsize=1, title=""),
             '6' = fplot3(input, txtsize=1, title=""),
             '7' = fplot4(input, txtsize=1, title=""),
             '8' = fplot5(input, txtsize=1, title=""),
             '9' = fplot6(input, txtsize=1, title=""))
      }
  })

  output$plot3L <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      switch(input$selectPlot3,
             '1' = fplot1(input, txtsize=3, group="austin", title="Greater Austin"),
             '2' = fplot1(input, txtsize=3, group="dfw", title="Dallas-Fort Worth"),
             '3' = fplot1(input, txtsize=3, group="houston", title="Greater Houston"),
             '4' = fplot1(input, txtsize=3, group="gsa", title="Greater San Antonio"),
             '5' = fplot2(input, txtsize=3, title="State-Wide"),
             '6' = fplot3(input, txtsize=3, title="Chapter 41 Districts"),
             '7' = fplot4(input, txtsize=3, title="Texas School Coalition"),
             '8' = fplot5(input, txtsize=3, title="Fast Growth Districts"),
             '9' = fplot6(input, txtsize=3, title="Equity Center Districts"))
    }
  })


  # output$plot4 <- renderPlot({
  #   data <- histdata[1:100]
  #   hist(data)
  # })


  # output$plot5 <- renderPlot({
  #   data <- histdata[1:100]
  #   hist(data)
  # })

  # output$plot6 <- renderPlot({
  #   data <- histdata[1:100]
  #   hist(data)
  # })


  output$downloadPlot1 <- downloadHandler(
      filename = function() { paste(input$formula, '.pdf', sep='') },
      content = function(file) {
          ggsave(file, device = "pdf", width=5, height=4, units="in")
      }
  )
  output$downloadPlot2 <- downloadHandler(
      filename = function() { paste(input$formula, '.pdf', sep='') },
      content = function(file) {
          ggsave(file, device = "pdf", width=5, height=4, units="in")
      }
  )
  output$downloadPlot3 <- downloadHandler(
      filename = function() { paste(input$formula, '.pdf', sep='') },
      content = function(file) {
          ggsave(file, device = "pdf", width=5, height=4, units="in")
      }
  )
  output$tsdownloadPlot2 <- downloadHandler(
      filename = function() { paste(input$formula, '.pdf', sep='') },
      content = function(file) {
          ggsave(file, device = "pdf", width=8, height=4, units="in")
      }
  )


  # output$downloadPlot3 <- downloadHandler(
  #     filename = function() { paste(input$formula, '.pdf', sep='') },
  #     content = function(file) {
  #         ggsave(file, device = "pdf", width=5, height=4, units="in")
  #     }
  # )

  formula_text <- eventReactive(input$save_formula, {
    print(eq)
    # Put formula in format god for eval  
    form <- input$new_formula
    # form <- "(Total Revenue - Local Taxes) / ADA"
    form <- trimws(unlist(strsplit(form,"(?=[^\\w ])",perl=TRUE)))
    form[grepl("^[[:alpha:]]", form)] <- paste0("`",form[grepl("^[[:alpha:]]", form)],"`")
    form <- paste0(form, collapse=" ")
    form
  })
  


  output$formula_msg <- renderText({
    form <- formula_text()
    if(form%in%eq$form) {
      return("<span style='color: red;'>Formula already present</span>")
    }
    msg <- tryCatch({
      tmp <- mydata[,eval(parse(text=form))]
      eq <<- rbind(eq, list(gsub("`","",form),form,gsub("`","",form))) 
      updateSelectInput(session, "formula", choices=eq$name)
      "<span style='color: green;'>Formula Saved</span>"
    }, error = function(e) {
      "<span style='color: red;'>Not a valid formula</span>"
    })
    return(msg)
  })


  output$tsGroups <- renderUI({
    choices = c(input$primary_district,
                input$secondary_district,
                "Greater Austin",
                "Dallas-Fort Worth",
                "Greater Houston",
                "Greater San Antonio",
                "Texas",
                "Equity Center",
                "Chapter 41",
                "Non Chapter 41",
                "Texas School Coalition",
                "Fast-Growth")
    checkboxGroupInput("tsgroups", NA,
                     choices=choices,
                     selected=choices[c(7,8,9)])
  })

  output$selected_formula <- renderText(input$formula)

})



