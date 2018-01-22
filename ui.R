library(shiny)
library(shinyBS)
library(shinydashboard)
library(data.table)
library(plotly)

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
              title=NULL,
              solidHeader = TRUE,
              status = "primary",
              width = 10,
              HTML("<div style='width: 100%; text-align: center; font-size: 1.25em; background-color: red; color: white;' '>You must click on the <span style='font-weight: bold;'>Settings</span> tab before viewing any reports.</div>")
            )
          ),
          fluidRow(
            box(
              title="Overview",
              solidHeader=TRUE,
              status="primary",
              width=6,
              HTML("<p>This tool is intended to help anyone explore and visualize different aspects of Texas public school funding at both the state and local district levels.
                    <p>The <span style='font-size: 1.25em; font-weight: bold;'>Settings</span> tab lets you select which data and specific school districts to display in the reports.
                    <p>The <span style='font-size: 1.25em; font-weight: bold;'>Single Year Reports</span> tab provides graphs and summary statistics for a single academic year.
                    <p>The <span style='font-size: 1.25em; font-weight: bold;'>Time Series Reports</span> tab provides graphs covering all years of available data.
                    <p>The <span style='font-size: 1.25em; font-weight: bold;'>Formulae</span> tab allows you to define your own custom metrics based on the available data.
                    <p>The <span style='font-size: 1.25em; font-weight: bold;'>Data</span> tab describes the information used in this tool, and provides direct sources to the raw data.")

            ),
            box(
              title="Contact",
              width = 4, 
              solidHeader = TRUE, status = "primary",
              HTML("This project is still in development and is occasionally updated. If you have any questions, suggestions, or bugs to report, please contact me at <span style='unicode-bidi:bidi-override; direction: rtl;'>moc.liamg@llewditweoj</span>")
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
                selected="EDGEWOOD ISD (BEXAR)")
            )
          ),

          fluidRow(
            titlePanel("Formula & Year"),
            box("Select a formula, or create a new formula in the 'Formulae' menu.", width=4,
               solidHeader=TRUE,
               status="primary"),
            box(title="Select Formula or Variable",
                uiOutput("choose_formula"), width=4,
                solidHeader=TRUE,
                status="primary"),
            box(title="Select Year",
                uiOutput("choose_year"), width=4,
                solidHeader=TRUE,
                status="primary")            
          ),
          fluidRow(
            box(title="Description of Built-In Formulas",
                width=12,
                solidHeader=TRUE,
                status='primary',
                tags$ul(
                  tags$li(HTML("<code>Local Taxes / ADA</code> - <em>Total revenue collected from local property taxes divided by the average daily student attendance (ADA) for the district.</em>")),  
                  tags$li(HTML("<code>Total Revenue / WADA</code> - <em>Total revenue for the district divided by the weighted average daily student attendance (WADA) for the district. WADA is calculated from a complex set of formulas, defined by the state, that attempts to determine how much local and state funding a district is entitled to.</em>")),  
                  tags$li(HTML("<code>(Total Revenue - Chap 41) / WADA</code> - <em>Total revenue for the district, minus 'Robin Hood' (Chap 41) tax dollar taken by the state, divided by the weighted average daily student attendance (WADA) for the district. <span style='font-weight: bold; font-size: 1.25em;'>This is the best 'apples to apples' metric to compare finances between districts.</span></em>")),  
                  tags$li(HTML("<code>(Total Revenue - Chap 41) / ADA</code> - <em>Total revenue for the district, minus 'Robin Hood' (Chap 41) tax dollar taken by the state, divided by the average daily student attendance (ADA) for the district. </em>"))  
                )              
            )
          )
        )
      ),
      tabItem(tabName = "plots",
        fluidPage(
          tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}"))),
          fluidRow(
            titlePanel("Formula"),
            HTML(paste0("<h3><code>",textOutput("selected_formula"),"</code></h3>"))
          ),
          fluidRow(
            titlePanel("Percentiles"),
            uiOutput("dist_1"),
            valueBoxOutput("vbox1", width=4),
            valueBoxOutput("vbox2", width=4),
            valueBoxOutput("vbox3", width=4)
          ),        
          fluidRow(
            uiOutput("dist_2"),
            valueBoxOutput("vbox4", width=4),
            valueBoxOutput("vbox5", width=4),
            valueBoxOutput("vbox6", width=4)
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
                plotlyOutput("plot1", height = 250), 
                actionButton("save1", "Zoom"),
                width=4),
            bsModal("modalPlot1", "", "save1", size = "large",
              plotlyOutput("plot1L", width="95%")
            ),            
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
                plotlyOutput("plot2", height = 250), 
                actionButton("save2", "Zoom"),
                width=4),
            bsModal("modalPlot2", "", "save2", size = "large",plotlyOutput("plot2L", width="75%")),
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
                plotlyOutput("plot3", height = 250), 
                actionButton("save3", "Zoom"),
                width=4),
            bsModal("modalPlot3", "", "save3", size = "large",plotlyOutput("plot3L", width="75%"))
          )
        )
      ),
      tabItem(tabName = "time",
        fluidPage(
          fluidRow(
            titlePanel("Time Series"),
            HTML(paste0("<h3><code>",textOutput("selected_formula2"),"</code></h3>")),
            box(plotlyOutput("tsplot2", height = 250), 
                actionButton("tssave2", "Zoom"),
                width=7),
            bsModal("tsmodalPlot2", "", "tssave2", size = "large",
              plotlyOutput("tsplot2L", width="95%")
            ),
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
        ),
        fluidRow(
          box(title="Description of Built-In Formulas",
              width=11,
              solidHeader=TRUE,
              status='primary',
              tags$ul(
                tags$li(HTML("<code>Local Taxes / ADA</code> - <em>Total revenue collected from local property taxes divided by the average daily student attendance (ADA) for the district.</em>")),  
                tags$li(HTML("<code>Total Revenue / WADA</code> - <em>Total revenue for the district divided by the weighted average daily student attendance (WADA) for the district. WADA is calculated from a complex set of formulas, defined by the state, that attempts to determine how much local and state funding a district is entitled to.</em>")),  
                tags$li(HTML("<code>(Total Revenue - Chap 41) / WADA</code> - <em>Total revenue for the district, minus 'Robin Hood' (Chap 41) tax dollars taken by the state, divided by the weighted average daily student attendance (WADA) for the district. <span style='font-weight: bold; font-size: 1.25em;'>This is the best 'apples to apples' metric to compare finances between districts.</span></em>")),  
                tags$li(HTML("<code>(Total Revenue - Chap 41) / ADA</code> - <em>Total revenue for the district, minus 'Robin Hood' (Chap 41) tax dollars taken by the state, divided by the average daily student attendance (ADA) for the district. </em>"))  
              )              
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
            box(
              title="Groups",
              solidHeader=TRUE,
              status="primary",
              width=4,
                tags$ul(
                  tags$li(HTML("<strong>Greater Austin</strong> - <em>School Districts in the Austin Area</em>")), 
                  tags$li(HTML("<strong>Greater Houston</strong> - <em>School Districts in the Houston Area</em>")), 
                  tags$li(HTML("<strong>Greater San Antonio</strong> - <em>School Districts in the San Antonio Area</em>")), 
                  tags$li(HTML("<strong>State Wide</strong> - <em>All Texas school districts</em>")), 
                  tags$li(HTML("<strong>Chapter 41 Districts</strong> - <em>All districts subject to local property tax confiscation by the state</em>")), 
                  tags$li(HTML("<strong>Equity Center Districts</strong> - <em>School Districts that are members of the Equity Center advocacy organization</em>")), 
                  tags$li(HTML("<strong>Texas School Coalition</strong> - <em>School Districts that are members of the Texas School Coalition advocacy organization</em>")), 
                  tags$li(HTML("<strong>Fast Growth Districts</strong> - <em>School Districts that are members of the Fast Growth School Coalition advocacy organization  </em>"))
                )
            ),
            box(
              title="Sources",
              solidHeader=TRUE,
              status="primary",
              width=4,
              tags$ul(tags$li(HTML("All financial data downloaded from the TEA <a href='http://tea.texas.gov/Reports_and_Data/'>website</a>")),
                        tags$ul(tags$li(HTML("<a href='https://tea.texas.gov/Finance_and_Grants/State_Funding/State_Funding_Reports_and_Data/Average__Daily_Attendance_and_Wealth_per_Average_Daily_Attendance/'>2005-2016 ADA and Wealth per ADA Report</a>")),
                                tags$li(HTML("<a href='https://tea.texas.gov/Finance_and_Grants/State_Funding/State_Funding_Reports_and_Data/PEIMS__Financial_Data_Downloads/'>PEIMS 2000-2016 Summarized Financial Data Report</a>")),
                                tags$li(HTML("<a href='https://tea.texas.gov/WorkArea/DownloadAsset.aspx?id=51539610452'>1994-2016 Chapter 41 Recapture Paid by District Report</a>"))
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



