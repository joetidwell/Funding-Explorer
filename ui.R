library(shiny)
library(shinyBS)
library(shinydashboard)
library(data.table)

load("data/mydata.RData")
mydata <- mydata[`Local Taxes`>0]


districtChoices <- sort(unique(mydata$`District Name`))
names(districtChoices) <- districtChoices

library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Funding Explorer (beta)"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Settings", tabName = "settings", icon = icon("cog")),
      menuItem("Single Year Reports", tabName = "plots", icon = icon("bar-chart")),
      menuItem("Time Series Reports", tabName = "time", icon = icon("line-chart")),
      menuItem("Formulae", tabName = "formulae", icon = icon("superscript")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="overview",
        fluidPage(
          fluidRow(
            box(
              title="Overview",
              solidHeader=TRUE,
              status="primary",
              width=6,
              HTML("<p>This tool is intended to help anyone explore and visualize different aspects of Texas public school funding at both the state and local district levels.
                    <p>The 'Settings' tab lets you select which data and specific school districts to display in the reports.
                    <p>The 'Single Year Reports' tab provides graphs and summary statistics for a single academic year.
                    <p>The 'Time Series Reports' tab provides graphs covering all years of available data.
                    <p>The 'Formulae' tab allows you to define your own custom metrics based on the available data.
                    <p>The 'Data' tab describes the information used in this tool, and provides direct sources to the raw data.")

            ),
            box(
              title="Contact",
              width = 4, 
              solidHeader = TRUE, status = "primary",
              HTML("This project is still in development and is continuously updated. If you have any questions, suggestions, or bugs to report, please contact me at <span style='unicode-bidi:bidi-override; direction: rtl;'>moc.liamg@llewditeoj</span>")
            )

          )
        )
      ),
      tabItem(tabName="settings",
        fluidPage(  
          fluidRow(
            titlePanel("Focal Districts"),
            box(
              solidHeader=TRUE,
              status="primary",
              width=4,
              "Select up to two districts to highlight in the graphs and summary statistics. District 'A' will be will be plotted in blue. District 'B' will be plotted in orange. If you only want to display one district, select NONE for District 'B'."
            ),
            box(
              title="District 'A'",              
              width = 4, 
              solidHeader = TRUE, status = "primary",
              selectizeInput("primary_district", "District 'A': (Type to search)",
                list(choices=districtChoices),
                selected="BOERNE ISD")),
            box(
              title="District 'B'",              
              width = 4, 
              solidHeader = TRUE, status = "primary",
              selectizeInput("secondary_district", "District 'B': (Type to search)",
                list(choices=c("NONE", districtChoices)),
                selected="NONE")
            )
          ),

          fluidRow(
            titlePanel("Formula & Year"),
            box("Select a formula, or create a new formula in the 'Formulae' menu.", width=4,
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
          )
        )
      ),
      tabItem(tabName = "plots",
        fluidPage(
          fluidRow(
            titlePanel("Percentile Ranks"),
            uiOutput("radioDist"),
            valueBoxOutput("vbox1", width=4),
            valueBoxOutput("vbox2", width=4),
            valueBoxOutput("vbox3", width=4)
          ),        
          fluidRow(
            titlePanel("Plots"),
            box(selectInput("selectPlot1", label = NA, 
                            choices = list("Greater Austin" = 1,
                                           "Dallas-Fort Worth" = 2,
                                           "Greater Houston" = 3,
                                           "Greater San Antonio" = 4, 
                                           "State-Wide" = 5, 
                                           "Chapter 41 Districts" = 6,
                                           "Equity Center Districts" = 9,
                                           "Texas School Coalition" = 7,
                                           "Fast Growth Districts" = 8),
                            selected = 4),
                plotOutput("plot1", height = 250), 
                actionButton("save1", "Zoom/Download"),
                width=4),
            bsModal("modalPlot1", "Your plot", "save1", size = "large",plotOutput("plot1L", width="75%"),downloadButton('downloadPlot1', 'Download')),
            box(selectInput("selectPlot2", label = NA, 
                            choices = list("Greater Austin" = 1,
                                           "Dallas-Fort Worth" = 2,
                                           "Greater Houston" = 3,
                                           "Greater San Antonio" = 4, 
                                           "State-Wide" = 5, 
                                           "Chapter 41 Districts" = 6,
                                           "Equity Center Districts" = 9,
                                           "Texas School Coalition" = 7,
                                           "Fast Growth Districts" = 8),
                            selected = 5),
                plotOutput("plot2", height = 250), 
                actionButton("save2", "Zoom/Download"),
                width=4),
            bsModal("modalPlot2", "Your plot", "save2", size = "large",plotOutput("plot2L", width="75%"),downloadButton('downloadPlot2', 'Download')),
            box(selectInput("selectPlot3", label = NA, 
                             choices = list("Greater Austin" = 1,
                                           "Dallas-Fort Worth" = 2,
                                           "Greater Houston" = 3,
                                           "Greater San Antonio" = 4, 
                                           "State-Wide" = 5, 
                                           "Chapter 41 Districts" = 6,
                                           "Equity Center Districts" = 9,
                                           "Texas School Coalition" = 7,
                                           "Fast Growth Districts" = 8),
                            selected = 8),
                plotOutput("plot3", height = 250), 
                actionButton("save3", "Zoom/Download"),
                width=4),
            bsModal("modalPlot3", "Your plot", "save3", size = "large",plotOutput("plot3L", width="75%"),downloadButton('downloadPlot3', 'Download'))
          )
        )
      ),
      tabItem(tabName = "time",
        fluidPage(
          fluidRow(
            titlePanel("Time Series"),
            box(plotOutput("tsplot2", height = 250), 
                actionButton("tssave2", "Zoom/Download"),
                width=7),
            bsModal("tsmodalPlot2", "Your plot", "tssave2", size = "large",plotOutput("tsplot2L", width="75%"),downloadButton('tsdownloadPlot2', 'Download')),
            box(title="Groups to display:",
                solidHeader=TRUE,
                status="primary",
                width=3,
                uiOutput("tsGroups")
            ),
            box(title="Options",
                checkboxInput("inflation", "Adjust for Inflation?", TRUE),
                width=2,
                solidHeader=TRUE,
                status="primary"
            )
          )
        )
      ),
      # Settings
      tabItem(tabName = "formulae",
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
                )

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

      tabItem(tabName = "data",
          fluidRow(
            box(
              title="Variables",
              solidHeader=TRUE,
              status="primary",
              width=4,
              "This tool is intended to help anyone ..."
            ),
            box(
              title="Groups",
              solidHeader=TRUE,
              status="primary",
              width=4,
              "This tool is intended to help anyone ..."
            ),
            box(
              title="Sources",
              solidHeader=TRUE,
              status="primary",
              width=4,
              tags$ul(tags$li(HTML("All financial data downloaded from the TEA <a href='http://tea.texas.gov/Reports_and_Data/'>website</a>")),
                        tags$ul(tags$li(HTML("<a href='http://tea.texas.gov/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=25769819568&libID=25769819677'>2005-2015 ADA and WADA Report</a>")),
                                tags$li(HTML("<a href='http://tea.texas.gov/WorkArea/linkit.aspx?LinkIdentifier=id&ItemID=25769825545&libID=25769825641'>PEIMS 2000-2015 Summarized Financial Data Report</a>")),
                                tags$li(HTML("<a href='http://tea.texas.gov/WorkArea/DownloadAsset.aspx?id=51539610094'>1994-2016 Chapter 41 Recapture Paid by District Report</a>"))
                                ),
                        tags$li(HTML("Equity Center membership obtained from the <a href='http://equitycenter.org/membership/equity-center-members/'>Equity Center</a>")),
                        tags$li(HTML("Texas School Coalition membership obtained from the <a href='http://www.txsc.org/membership/'>Texas School Coalition</a>")),
                        tags$li(HTML("Fast growth school districts obtained from the <a href='http://fastgrowthtexas.org/member-districts/'>Fast Growth School Coalition</a>"))
              )
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



