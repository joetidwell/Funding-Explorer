library(shiny)
library(shinyBS)
library(shinydashboard)
library(ggplot2)
library(data.table)
theme_set(theme_classic())


# path.data <- file.path("~/git/District-Explorer/data")
# load(file.path(path.data,"districts.RData"))
# path.data <- file.path("~/git/WADA-Explorer/data")
# load(file.path(path.data,"mydata.RData"))

path.data <- file.path("~/srv/shiny-server/districts/data")
load(file.path(path.data,"districts.RData"))
path.data <- file.path("~/srv/shiny-server/Funding-Explorer/data")
load(file.path(path.data,"mydata.RData"))

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

fplot1 <- function(input, txtsize=1) {
        data <- mydata[gsa==TRUE & year==input$year,]
        data[,color:="grey"]
        data[`District Name`==input$primary_district, color:=dcolor[1]]
        data[`District Name`==input$secondary_district, color:=dcolor[2]]
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
          labs(x="", title="Greater San Antonio", y=paste0(data$year[1]," : ",input$formula)) +
          guides(fill=FALSE, color=FALSE) +
          geom_text(aes(y=0, color=color), size=txtsize) +
          scale_color_manual(values=c("black","black","white")) +
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
}


fplot2 <- function(input, txtsize=3) {

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
                    color=ddata$tcolor, fill=ddata$color, size=txtsize) +
          # scale_fill_manual(values=dcolor) +
          # scale_color_manual(values=tcolor) +
          expand_limits(y = 0) +
          scale_y_continuous(expand = c(0, 0)) +
          labs(y="Percentile", title="Texas", x=paste0(data$year[1]," : ",input$formula)) +
          guides(fill=FALSE, color=FALSE) 
          # theme(axis.title.y=element_blank(),
          #       axis.text.y=element_blank(),
          #       axis.ticks.y=element_blank())
}


fplot3 <- function(input, txtsize=1) {
        data <- mydata[fgsd==TRUE & year==input$year,]
        data[,color:="grey"]
        data[,tcolor:="black"]
        data[`District Name`==input$primary_district, color:=dcolor[1]]
        # data[`District Name`==input$primary_district, tcolor:="white"]
        data[`District Name`==input$secondary_district, color:=dcolor[2]]
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
          labs(x="", title="Fast Growth Districts", y=paste0(data$year[1]," : ",input$formula)) +
          guides(fill=FALSE, color=FALSE) +
          geom_text(aes(y=0), color=data$tcolor[order(data$value)], size=txtsize) +
          scale_color_manual(values=c("black","black","white")) +
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
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

    output$GSA <- renderValueBox({
      if(!is.null(input$radio) & !is.null(input$formula)) {
        mycol <- ifelse(input$radio==1, "blue", "yellow")

        data <- mydata[gsa==TRUE & year==input$year]
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
      }
        valueBox(txt, "In the San Antonio Area", icon = icon("map-pin"), 
                 color=mycol)
     
      })
   output$state <- renderValueBox({
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

   output$FGSD <- renderValueBox({
      if(!is.null(input$radio)) {
        mycol <- ifelse(input$radio==1, "blue", "yellow")
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
        valueBox(txt, "In Fast Growth Districts", icon = icon("line-chart"), 
                 color=mycol)
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
    selectInput("formula", NULL, as.list(eq$name))
  })

  # Drop-down selection box for which year
  output$choose_year <- renderUI({
    selectInput("year", NULL, as.list(unique(mydata$year)), selected=2014)
  })



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Greater San Antonio - Most Recent Year
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$plot1 <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
        fplot1(input, txtsize=1)
      }
  })

  output$plot1L <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
        fplot1(input, txtsize=3)
      }
  })


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Texas - Most Recent Year
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  output$plot2 <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      fplot2(input, txtsize=3)
    }
  })

  output$plot2L <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      fplot2(input, txtsize=3)
    }
  })


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Greater San Antonio - Most Recent Year
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$plot3 <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      fplot3(input, txtsize=1)
    }
  })

  output$plot3L <- renderPlot({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      fplot3(input, txtsize=1)
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

  output$downloadPlot3 <- downloadHandler(
      filename = function() { paste(input$formula, '.pdf', sep='') },
      content = function(file) {
          ggsave(file, device = "pdf", width=5, height=4, units="in")
      }
  )

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


  
})



