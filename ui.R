library(shiny)
library(shinyBS)
library(shinydashboard)
library(data.table)


# path.data <- file.path("~/git/District-Explorer/data")
# load(file.path(path.data,"districts.RData"))
  # path.data <- file.path("~/srv/shiny-server/Funding-Explorer/data")
load("data/mydata.RData")
mydata <- mydata[`Local Taxes`>0]


# path.data <- file.path("/srv/shiny-server/Funding-Explorer/data")
# load("data/mydata.RData")
# exclude districts who don;t collect taxes, i.e. charter and federal school
# mydata <- mydata[`Local Taxes`>0]


districtChoices <- sort(unique(mydata$`District Name`))
names(districtChoices) <- districtChoices

## ui.R ##
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Funding Explorer"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      # HTML('<li>
      #       <a style="">
      #         <i class=""></i>
      #         <h4>Pages</h4>
      #       </a>
      #     </li>'),
      menuItem("Single Year Reports", tabName = "plots", icon = icon("bar-chart")),
      menuItem("Time Series Reports", tabName = "time", icon = icon("line-chart")),
      # menuItem("Tables", tabName = "widgets", icon = icon("table")),
      # menuItem("Data", tabName = "widgets", icon = icon("database")),
      menuItem("Settings & Help", tabName = "settings", icon = icon("cog"))
      # HTML('<li style="margin-top: 10px;">
      #       <a style="">
      #         <i class=""></i>
      #         <h4>Formulae</h4>
      #       </a>
      #     </li>')
      # uiOutput("choose_formula")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "plots",
        fluidPage(
          # titlePanel("Settings"),
          # fluidRow(
          #   # uiOutput("dist_box"),
          #   box(
          #     title = "Target Districts", width = 4, 
          #     solidHeader = TRUE, status = "primary",
          #     selectizeInput("primary_district", "District 'A': (Type to search)",
          #       list(choices=districtChoices),
          #       selected="BOERNE ISD"),
          #     selectizeInput("secondary_district", "District 'B': (Type to search)",
          #       list(choices=districtChoices),
          #       selected="EDGEWOOD ISD (BEXAR)")
          #     )
          # ),
          fluidRow(
            titlePanel("Formula & Year"),
            # uiOutput("display_formula"),
            # box(width=4,
            #     "Choose one of the formulas in the selection box to the right, or create a new one.",
            #     status="primary"),
            box("Select a formula, or create a new formula in the 'Settings & Help' menu, where you can also select two 'focus' districts for the Percentile Ranks and Plot displays. ", width=4,
               solidHeader=TRUE,
               status="primary"),
            box(title="Select Formula",
                uiOutput("choose_formula"), width=4,
                solidHeader=TRUE,
                status="primary"),
            box(title="Select Year",
                uiOutput("choose_year"), width=4,
                solidHeader=TRUE,
                status="primary")

            
          ),
          fluidRow(
            titlePanel("Percentile Ranks"),
            uiOutput("radioDist"),
            valueBoxOutput("GSA", width=4),
            valueBoxOutput("state", width=4),
            valueBoxOutput("FGSD", width=4)
          ),        
          fluidRow(
            titlePanel("Plots"),
            box(plotOutput("plot1", height = 250), 
                actionButton("save1", "Zoom/Download"),
                width=4),
            bsModal("modalPlot1", "Your plot", "save1", size = "large",plotOutput("plot1L", width="75%"),downloadButton('downloadPlot1', 'Download')),
            box(plotOutput("plot2", height = 250), 
                actionButton("save2", "Zoom/Download"),
                width=4),
            bsModal("modalPlot2", "Your plot", "save2", size = "large",plotOutput("plot2L", width="75%"),downloadButton('downloadPlot2', 'Download')),
            box(plotOutput("plot3", height = 250), 
                actionButton("save3", "Zoom/Download"),
                width=4),
            bsModal("modalPlot3", "Your plot", "save3", size = "large",plotOutput("plot3L", width="75%"),downloadButton('downloadPlot3', 'Download'))
          )
          # fluidRow(
          #   box(plotOutput("plot4", height = 250), 
          #       actionButton("large1", "Zoom"),
          #       actionButton("save1", "Save PDF"),
          #       width=4),
          #   box(plotOutput("plot5", height = 250), 
          #       actionButton("large1", "Zoom"),
          #       actionButton("save1", "Save PDF"),
          #       width=4),
          #   box(plotOutput("plot6", height = 250), 
          #       actionButton("large1", "Zoom"),
          #       actionButton("save1", "Save PDF"),
          #       width=4)
          # )
        )
      ),
      tabItem(tabName = "time",
        fluidPage(
          fluidRow(
            titlePanel("Formula & Year"),
            box("Select a formulas, or create a new formula in the 'Settings & Help' menu, where you can also select two 'focus' districts for the Percentile Ranks and Plot displays. ", width=4,
               solidHeader=TRUE,
               status="primary"),
            box(title="Select Formula",
                uiOutput("choose_formula_time"), width=4,
                solidHeader=TRUE,
                status="primary"),
            box(title="Options",
                checkboxInput("inflation", "Adjust for Inflation?", FALSE),
                width=2,
                solidHeader=TRUE,
                status="primary")
            # box(title="Select Year",
            #     uiOutput("choose_year"), width=4,
            #     solidHeader=TRUE,
            #     status="primary")
          ),
          # fluidRow(
          #   titlePanel("Percentile Ranks")
          #   # uiOutput("radioDist"),
          #   # valueBoxOutput("GSA", width=4),
          #   # valueBoxOutput("state", width=4),
          #   # valueBoxOutput("FGSD", width=4)
          # ),        
          fluidRow(
            titlePanel("Plots"),
            box(plotOutput("tsplot1", height = 250), 
                actionButton("tssave1", "Zoom/Download"),
                width=6),
            bsModal("tsmodalPlot1", "Your plot", "tssave1", size = "large",plotOutput("tsplot1L", width="75%"),downloadButton('tsdownloadPlot1', 'Download')),
            box(plotOutput("tsplot2", height = 250), 
                actionButton("tssave2", "Zoom/Download"),
                width=6),
            bsModal("tsmodalPlot2", "Your plot", "tssave2", size = "large",plotOutput("tsplot2L", width="75%"),downloadButton('tsdownloadPlot2', 'Download'))
            # box(plotOutput("tsplot3", height = 250), 
            #     actionButton("tssave3", "Zoom/Download"),
            #     width=4),
            # bsModal("tsmodalPlot3", "Your plot", "tssave3", size = "large",plotOutput("tsplot3L", width="75%"),downloadButton('tsdownloadPlot3', 'Download'))

          )
          # fluidRow(
          #   box(plotOutput("plot4", height = 250), 
          #       actionButton("large1", "Zoom"),
          #       actionButton("save1", "Save PDF"),
          #       width=4),
          #   box(plotOutput("plot5", height = 250), 
          #       actionButton("large1", "Zoom"),
          #       actionButton("save1", "Save PDF"),
          #       width=4),
          #   box(plotOutput("plot6", height = 250), 
          #       actionButton("large1", "Zoom"),
          #       actionButton("save1", "Save PDF"),
          #       width=4)
          # )
        )
      ),
      # Settings
      tabItem(tabName = "settings",
        fluidRow(
            box(
              title = "Target Districts", width = 4, 
              solidHeader = TRUE, status = "primary",
              "Select two districts to highlight in the various plots. District 'A' will be will be plotted in blue. District 'B' will be plotted in orange",
              HTML("<p>"),
              selectizeInput("primary_district", "District 'A': (Type to search)",
                list(choices=districtChoices),
                selected="BOERNE ISD"),
              selectizeInput("secondary_district", "District 'B': (Type to search)",
                list(choices=districtChoices),
                selected="EDGEWOOD ISD (BEXAR)")
            )
        ),
        fluidRow(
            box(title="Data Description",
                width=5,
                solidHeader=TRUE,
                status="primary",
                h4("Variables"),
                tags$ul(
                  tags$li(HTML("<strong>ADA</strong> - <em>average daily attendance</em>")), 
                  tags$li(HTML("<strong>Chap41</strong> - <em>district funds reclaimed by Chapter 41</em>")), 
                  tags$li(HTML("<strong>Federal Revenue</strong> - <em>value of federal funds included in the general fund total operating revenue</em>")), 
                  tags$li(HTML("<strong>Local Taxes</strong> - <em>value of local taxes included in the general fund total operating revenue</em>")), 
                  tags$li(HTML("<strong>State Revenue</strong> - <em>value of state funds included in the general fund total operating revenue</em>")), 
                  tags$li(HTML("<strong>Total Revenue</strong> - <em>value of general fund total operating revenue</em>")), 
                  tags$li(HTML("<strong>WADA</strong> - <em>weighted average daily attendance</em>")) 
                ),
                h4("Source"),
                tags$ul(tags$li(HTML("All financial data downloaded from the TEA <a href='http://tea.texas.gov/Reports_and_Data/'>website</a>")))
                
            ),
            box(title="Add a formula",
                width=6,
                solidHeader=TRUE,
                status="primary",
                tags$p(),
                "Create a new formula to calculate data for the Summary page by typing it in the box below. You can use any of the variable names listed in Data Description tab to the left. Type the name exactly as it appears in the description.",
                tags$p(),
                HTML("For example, if you wanted to create a formula for taxes collected per average attendance you could type <code>Local Taxes/ADA</code>, or for a formula for local taxes per weighted average attendance adjusted for Chapter 41 payments would be <code>(Local Taxes - Chap41)/WADA</code>"),
                tags$p(),
                HTML("Once you have successfully created a new formula, and pressed 'Submit', it will appear on the formula selection list on the Summary page."),
                tags$p(),
                textInput("new_formula", 
                          "Formula", value = "", width = NULL, placeholder = "Enter formula"),
                actionButton("save_formula", "Save Formula"),
                htmlOutput("formula_msg")


            )
        )
      ),


      # Second tab content
      tabItem(tabName = "widgets",
        h2("Widgets tab content")
      )
    )
  )
)