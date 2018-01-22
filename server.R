library(shiny)
library(shinyBS)
library(shinydashboard)
library(ggplot2)
library(scales)
library(data.table)
library(plotly)
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
    
    print(data)
    
  data <- data[order(value)]
  data[,`District Name`:=factor(`District Name`, levels=`District Name`)]
  data[,htext:=paste0("</br>",`District Name`," HOWDY")]

  ax <- list(
    title="",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  if(txtsize==1) {
    f <- list(size=5)
  } else {
    f <- list(size=10)
  }
  if(txtsize==1) {
    titlef <- list(size=10)
  } else {
    titlef <- list(size=20)
  }

  plot_ly(
    data = data,
    y = ~`District Name`,
    x = ~value,
    type = "bar",
    marker = list(color = ~color),
    orientation = 'h',
    text = ~`District Name`, 
    # textposition = 'auto',
    # textfont = list(color = '#FFFFFF', size = 16),
    hoverinfo = 'x+text'
  ) %>%
    layout(title=ifelse(txtsize>1,title,""),
           yaxis = ax,
           xaxis = list(title=input$formula,
                        titlefont=titlef),
           margin = list(l = 10, r=20, t=50),
           annotations = list(
            x = ~value,
            y = ~`District Name`,
            text = ~`District Name`,
            # xanchor = ifelse(~value > 0, 'left', 'right'),
            xanchor = 'left',
            showarrow = FALSE,
            font = f
           )
    ) %>%
    config(displayModeBar = ifelse(txtsize>1,TRUE,TRUE),
           displaylogo = FALSE,
           collaborate = FALSE,
           cloud = FALSE,
           modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
    )


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


  if(txtsize==1) {
    f <- list(size=5)
  } else {
    f <- list(size=10)
  }
  if(txtsize==1) {
    titlef <- list(size=10)
  } else {
    titlef <- list(size=20)
  }

  data <- data[order(percentile)]
  plot_ly(
    data = data,
    y = ~percentile,
    x = ~value,
    type = "scatter",
    mode="lines",
    hoverinfo = 'text',
    text = ~paste('</br> District: ', `District Name`,
                  '</br> Value: ', round(value,2),
                  '</br> Percentile: ', paste(round(percentile,2)*100,"%") )) %>%
  add_annotations(x=ddata$value,
                  y=ddata$percentile,
                  text=ddata$`District Name`) %>%
  layout(title=ifelse(txtsize>1,title,""),
         yaxis = list(title="Percentile"),
         xaxis = list(title=input$formula,
                      titlefont=titlef),
         margin = list(l = 50, r=20, t=50)
         ) %>%
  config(displayModeBar = ifelse(txtsize>1,TRUE,TRUE),
         displaylogo = FALSE,
         collaborate = FALSE,
         cloud = FALSE,
         modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
  )



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

        }

  plot_ly(alldata[group%in%groups], x = ~year, y = ~`50%`, color = ~group,
             hoverinfo='text',
             text = ~paste('</br> Group: ', group, 
                           '</br> Year: ', year,
                           '</br> Value: ', round(`50%`,2))) %>%
    add_lines(annotations=NULL) %>% add_markers(showlegend=FALSE) %>%
    layout(yaxis=list(title="Median", tickformat=",d"),
           xaxis=list(title="Year"), title=input$formula,
           margin = list(l = 50, r=20, t=70)) %>%
  config(displayModeBar = ifelse(txtsize>1,TRUE,TRUE),
         displaylogo = FALSE,
         collaborate = FALSE,
         cloud = FALSE,
         modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
  )


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



    if(txtsize==1) {
      f <- list(size=5)
    } else {
      f <- list(size=10)
    }
    if(txtsize==1) {
      titlef <- list(size=10)
    } else {
      titlef <- list(size=20)
    }

    data <- data[order(percentile)]
    plot_ly(
      data = data,
      y = ~percentile,
      x = ~value,
      type = "scatter",
      mode="lines",
      hoverinfo = 'text',
      text = ~paste('</br> District: ', `District Name`,
                    '</br> Value: ', round(value,2),
                    '</br> Percentile: ', paste(round(percentile,2)*100,"%") )) %>%
    add_annotations(x=ddata$value,
                    y=ddata$percentile,
                    text=ddata$`District Name`) %>%
    layout(title=ifelse(txtsize>1,title,""),
           yaxis = list(title="Percentile"),
           xaxis = list(title=input$formula,
                        titlefont=titlef),
           margin = list(l = 50, r=20, t=50)
           ) %>%
    config(displayModeBar = ifelse(txtsize>1,TRUE,TRUE),
           displaylogo = FALSE,
           collaborate = FALSE,
           cloud = FALSE,
           modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
    )


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

    if(txtsize==1) {
      f <- list(size=5)
    } else {
      f <- list(size=10)
    }
    if(txtsize==1) {
      titlef <- list(size=10)
    } else {
      titlef <- list(size=20)
    }

    data <- data[order(percentile)]
    plot_ly(
      data = data,
      y = ~percentile,
      x = ~value,
      type = "scatter",
      mode="lines",
      hoverinfo = 'text',
      text = ~paste('</br> District: ', `District Name`,
                    '</br> Value: ', round(value,2),
                    '</br> Percentile: ', paste(round(percentile,2)*100,"%") )) %>%
    add_annotations(x=ddata$value,
                    y=ddata$percentile,
                    text=ddata$`District Name`) %>%
    layout(title=ifelse(txtsize>1,title,""),
           yaxis = list(title="Percentile"),
           xaxis = list(title=input$formula,
                        titlefont=titlef),
           margin = list(l = 50, r=20, t=50)
           ) %>%
    config(displayModeBar = ifelse(txtsize>1,TRUE,TRUE),
           displaylogo = FALSE,
           collaborate = FALSE,
           cloud = FALSE,
           modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
    )


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

    if(txtsize==1) {
      f <- list(size=5)
    } else {
      f <- list(size=10)
    }
    if(txtsize==1) {
      titlef <- list(size=10)
    } else {
      titlef <- list(size=20)
    }

    data <- data[order(percentile)]
    plot_ly(
      data = data,
      y = ~percentile,
      x = ~value,
      type = "scatter",
      mode="lines",
      hoverinfo = 'text',
      text = ~paste('</br> District: ', `District Name`,
                    '</br> Value: ', round(value,2),
                    '</br> Percentile: ', paste(round(percentile,2)*100,"%") )) %>%
    add_annotations(x=ddata$value,
                    y=ddata$percentile,
                    text=ddata$`District Name`) %>%
    layout(title=ifelse(txtsize>1,title,""),
           yaxis = list(title="Percentile"),
           xaxis = list(title=input$formula,
                        titlefont=titlef),
           margin = list(l = 50, r=20, t=50)
           ) %>%
    config(displayModeBar = ifelse(txtsize>1,TRUE,TRUE),
           displaylogo = FALSE,
           collaborate = FALSE,
           cloud = FALSE,
           modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
    )


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

    if(txtsize==1) {
      f <- list(size=5)
    } else {
      f <- list(size=10)
    }
    if(txtsize==1) {
      titlef <- list(size=10)
    } else {
      titlef <- list(size=20)
    }

    data <- data[order(percentile)]
    plot_ly(
      data = data,
      y = ~percentile,
      x = ~value,
      type = "scatter",
      mode="lines",
      hoverinfo = 'text',
      text = ~paste('</br> District: ', `District Name`,
                    '</br> Value: ', round(value,2),
                    '</br> Percentile: ', paste(round(percentile,2)*100,"%") )) %>%
    add_annotations(x=ddata$value,
                    y=ddata$percentile,
                    text=ddata$`District Name`) %>%
    layout(title=ifelse(txtsize>1,title,""),,
           yaxis = list(title="Percentile"),
           xaxis = list(title=input$formula,
                        titlefont=titlef),
           margin = list(l = 50, r=20, t=50)
           ) %>%
    config(displayModeBar = ifelse(txtsize>1,TRUE,TRUE),
           displaylogo = FALSE,
           collaborate = FALSE,
           cloud = FALSE,
           modeBarButtonsToRemove = c('sendDataToCloud','hoverCompareCartesian','zoom2d')
    )

}


shinyServer(function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)

    GSAbox <- reactive({
      if(!is.null(input$formula) & !is.null(input$selectPlot1)) {
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
        txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")

        if(txt=="%") txt <- "--%"


      } else {
        txt<-"--%"
        title<-"---"
      }
        valueBox(txt, title, icon = icon("map-pin"), 
                 color="blue")
     
      })


   statebox <- reactive({
      data <- mydata[year==input$year]
      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
      txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      valueBox(txt, "State-Wide", icon = icon("map"), 
               color="blue")  
      })

   Chap41box <- reactive({
      data <- mydata[Chap41 > 0 & year==input$year]

      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
      txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      # valueBox(txt, "In Fast Growth Districts", icon = icon("line-chart"), 
      valueBox(txt, "In Chapter 41 Districts", icon = icon("home"), 
               color="blue")
    })

   FGSDbox <- reactive({
      data <- mydata[fgsd==TRUE & year==input$year]

      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
      txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      # valueBox(txt, "In Fast Growth Districts", icon = icon("line-chart"), 
      valueBox(txt, "In Fast-Growth Districts", icon = icon("line-chart"), 
               color="blue")
    })

   TSCbox <- reactive({
      data <- mydata[TSC==TRUE & Chap41 > 0 & year==input$year]

      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
      txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      # valueBox(txt, "In Fast Growth Districts", icon = icon("line-chart"), 
      valueBox(txt, "In TSC Districts", icon = icon("group"), 
               color="blue")
    })

   equitybox <- reactive({
      data <- mydata[equity==TRUE & year==input$year]

      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
      txt <- paste0(as.integer((data[`District Name`==input$primary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      valueBox(txt, "In Equity Center Districts", icon = icon("balance-scale"), 
               color="blue")
    })

    GSAbox2 <- reactive({
      if(!is.null(input$formula) & !is.null(input$selectPlot1)) {
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
          txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")

        if(txt=="%") txt <- "--%"


      } else {
        txt<-"--%"
        title<-"---"
      }
        valueBox(txt, title, icon = icon("map-pin"), 
                 color="yellow")
     
      })


   statebox2 <- reactive({
      data <- mydata[year==input$year]
      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
        txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      valueBox(txt, "State-Wide", icon = icon("map"), 
               color="yellow")  
    })

   Chap41box2 <- reactive({
      data <- mydata[Chap41 > 0 & year==input$year]
      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
        txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      valueBox(txt, "In Chapter 41 Districts", icon = icon("home"), 
               color="yellow")
    })

   FGSDbox2 <- reactive({
      data <- mydata[fgsd==TRUE & year==input$year]
      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
        txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      valueBox(txt, "In Fast-Growth Districts", icon = icon("line-chart"), 
               color="yellow")
    })

   TSCbox2 <- reactive({
      data <- mydata[TSC==TRUE & Chap41 > 0 & year==input$year]
      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
        txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      valueBox(txt, "In TSC Districts", icon = icon("group"), 
               color="yellow")
    })

   equitybox2 <- reactive({
      data <- mydata[equity==TRUE & year==input$year]
      data[,value:=eval(parse(text=eq[name==input$formula]$form))]
      Fn <- ecdf(data$value)
      data[,per:=Fn(value)]
        txt <- paste0(as.integer((data[`District Name`==input$secondary_district]$per*100)),"%")

      if(txt=="%") txt <- "--%"

      valueBox(txt, "In Equity Center Districts", icon = icon("balance-scale"), 
               color="yellow")
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

  output$vbox4 <- renderValueBox({
    if(!is.null(input$selectPlot1)) {
         switch(input$selectPlot1,  
               '1' = GSAbox2(),
               '2' = GSAbox2(),
               '3' = GSAbox2(),
               '4' = GSAbox2(),
               '5' = statebox2(),
               '6' = Chap41box2(),
               '7' = TSCbox2(),
               '8' = FGSDbox2(),
               '9' = equitybox2())   
    }  
  })
   

  output$vbox5 <- renderValueBox({
    if(!is.null(input$selectPlot2)) {
         switch(input$selectPlot2,  
               '1' = GSAbox2(),
               '2' = GSAbox2(),
               '3' = GSAbox2(),
               '4' = GSAbox2(),
               '5' = statebox2(),
               '6' = Chap41box2(),
               '7' = TSCbox2(),
               '8' = FGSDbox2(),
               '9' = equitybox2())   
    }
  })

  output$vbox6 <- renderValueBox({
    if(!is.null(input$selectPlot3)) {    
     switch(input$selectPlot3,  
               '1' = GSAbox2(),
               '2' = GSAbox2(),
               '3' = GSAbox2(),
               '4' = GSAbox2(),
               '5' = statebox2(),
               '6' = Chap41box2(),
               '7' = TSCbox2(),
               '8' = FGSDbox2(),
               '9' = equitybox2())   
   }
  })


  # Drop-down selection box for which formula
  output$choose_formula <- renderUI({
    selectInput("formula", NULL, as.list(eq$name), selected="(Total Revenue-Chap41)/WADA")
  })

  # Drop-down selection box for which year
  output$choose_year <- renderUI({
    selectInput("year", NULL, as.list(unique(mydata$year)), selected=2015)
  })



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Greater San Antonio - Most Recent Year
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$plot1 <- renderPlotly({
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

  output$plot1L <- renderPlotly({
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



  output$plot2 <- renderPlotly({
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

  output$plot2L <- renderPlotly({
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


  output$tsplot2 <- renderPlotly({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      tsfplot2(input, txtsize=3,ifelse(input$inflation==TRUE, TRUE, FALSE),input$tsgroups)
    }
  })

  output$tsplot2L <- renderPlotly({
    if(!(is.null(input$formula) | is.null(input$primary_district))) {
      tsfplot2(input, txtsize=3,ifelse(input$inflation==TRUE, TRUE, FALSE),input$tsgroups)
    }
  })



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Greater San Antonio - Most Recent Year
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  output$plot3 <- renderPlotly({
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

  output$plot3L <- renderPlotly({
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
                     selected=choices[c(1,7)])
  })

  output$selected_formula <- renderText(input$formula)
  output$selected_formula2 <- renderText(input$formula)

  output$dist_1 <- renderUI(tags$h4(input$primary_district))
  output$dist_2 <- renderUI(tags$h4(input$secondary_district))

df2 <- data.frame(x = 1:10, y = 1:10)
output$scatter1 <- renderPlotly({
    plot_ly(df2, x = ~x, y = ~y) %>% add_markers()
  })

  output$box1 <- renderPlotly({
    eventdata <- event_data('plotly_click')
    validate(need(!is.null(eventdata),
                  'Hover over the scatter plot to populate this boxplot'))

    plot_ly(df2, x = ~x, y = ~y, type = 'box')
  })

})



