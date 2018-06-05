##################### Loading packages ##########################

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(highlightHTML)
library(pander)
library(markdown)
library(stringr)


uu_color <- " #ffcd00"

##################################################################


ui <- dashboardPage(
# Appearance I #####
    skin = "black",
  
dashboardHeader(title = "ANOVA and Regression equivalence", titleWidth = 350),
dashboardSidebar(width = 350,
                 sidebarMenu(
                   menuItem("ANOVA = Regression", tabName = "tab1"),
                   menuItem("Disclaimer", tabName = "Disclaimer"),
                   HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
                   img(src = 'cm_hs_uu-logoengels_diapositief_rgb.png', align = "left"),
                   HTML("<br>"),
                   
                   div("Shiny app by",
                       a(href="https://www.uu.nl/staff/FKlaassen/0",
                         target = "_blank",
                         "Fayette Klaassen"),align="left", style = "font-size: 10pt"),
                   
                   div("Base Layout by",
                       a(href="https://www.uu.nl/medewerkers/KMLek/0",target="_blank",
                         "Kimberley Lek"),align="left", style = "font-size: 10pt"),
                   
                   div("Shiny source files:",
                       a(href="https://github.com/EducationalShinyUU/ANOVA-Regression",
                         target="_blank","GitHub"),align="left", style = "font-size: 10pt")
                  
     ### you can easily add extra tabs by including an extra "menuItem("...", tabName = "")," before the disclaimer ###
      ### you can remove or add <br> statements in the HTML function (menuItem("Disclaimer")) to adjust the position of the UU logo (make sure it is approximately at the bottom of the screen when opened)
    )
  ),
  
# Appearance II #####  
  dashboardBody(
    # CSS styles
    tags$style(HTML(".irs-bar {background: uu_color}")),
    tags$style(HTML(".irs-bar {border-top: 1px solid black}")),
    tags$style(HTML(".irs-bar-edge {background: uu_color}")),
    tags$style(HTML(".irs-bar-edge {border: 1px solid black}")),
    tags$style(HTML(".irs-single {background: uu_color}")),
    tags$style(HTML(
      ".selectize-input {border-color: uu_color}"
    )),
    tags$style(HTML(
      ".selectize-dropdown {border-color: uu_color}"
    )),
    
    ### note that #EAC626 is the mustard yellow color used in the sidebar. ###
    ### If possible, you can use this color + different shades of grey (+ black & white) in your figures. ###
    
    tags$head(tags$style(
      HTML(
        '.skin-black .main-header .logo {
        background-color: ',  uu_color, ';
        }
        .skin-black .main-header .logo:hover {
        background-color: ',  uu_color, ';
        }
        
        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: ',  uu_color, ';
        }
        
        /* navbar (rest of the header) */
        .skin-black .main-header .navbar {
        background-color: ',  uu_color, ';
        }
        
        /* toggle button when hovered  */
        .skin-black .main-header .navbar .sidebar-toggle:hover{
        background-color: ',  uu_color, ';
        }
        
        /* other links in the sidebarmenu when hovered */
        .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: ',  uu_color, ';
        }
        /* other links in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu a{
        background-color: ',  uu_color, ';
        color: #000000;
        }
        
        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #000000;
        color: #FFFFFF;
        }
        
        .skin-black .main-sidebar {color: #000000; background-color: ',  uu_color, ';}
        
        '
      )
      )),
    
# Tab layout #####
    tabItems(
      tabItem(tabName = "Disclaimer", box(
        width = 12,
        h5(
          "Terms of Usage Utrecht Unversity Shiny Server",
          br(),
          br(),
          tags$ul(
            tags$li(
              "Purpose of the service “utrecht-university.shinyapps.io” is to provide a digital place for trying out, evaluating and/or comparing methods developed by researchers of Utrecht University for the scientific community worldwide. The app and its contents may not be preserved in such a way that it can be cited or can be referenced to. "
            ),
            tags$li(
              "The web application is provided ‘as is’ and ‘as available’ and is without any warranty. Your use of this web application is solely at your own risk."
            ),
            tags$li(
              "	You must ensure that you are lawfully entitled and have full authority to upload  data in the web application. The file data must not contain any  data which can raise issues relating to abuse, confidentiality, privacy,  data protection, licensing, and/or intellectual property. You shall not upload data with any confidential or proprietary information that you desire or are required to keep secret. "
            ),
            tags$li("By using this app you agree to be bound by the above terms.")
          )
        )
      )),
      # tabItem(tabName = "home", box(
      #   width = 12,
      #   align = "center",
      #   h4("Welcome"),
      #   column(12, align = "left", h5("add app background info"))
      # )),
## Tab 1 AOV = REG #####      
      tabItem(
        tabName = "tab1",
        
### input / settings #####
        
box(width = 12, align = "center",
    h4("ANOVA = Regression"),
    column(12, align = "left", h4("Input / settings")),
    
    fluidRow(
      column(width = 4, align = "left",
             radioButtons("Ngroups", "Number of groups", choices = c("2" = 2, "3" = 3, "4" = 4))
      ),
      column(width = 4, align = "left", 
             uiOutput("refGroupUI")
      ),
      column(width = 4, align = "left",
             actionButton("sample", "Different sample"))
    # column(width = 3,
    #        selectInput("selectHyp", "True in the population", choices = c("H0" = 1, "Ha, small effect" = 2, "Ha, medium effect" = 3, "Ha, large effect" = 4))
    # ),
    # column(width = 3,
    #        selectInput("sampleSize", "Sample size per group", choices = c("10" = 10, "20" = 20, "60" = 60, "100" = 100))
    # )),
    # column(width = 12, align = "center",
    #        actionButton("sample", "Sample data")
     )
),

### Tab within page setup #####
tabsetPanel(
  tabPanel("Data",
           box(width = 3, align = "left", radioButtons("dataForm", "Data format", choices = c("Group coding" = 1, "Dummy coding" = 2))),
           uiOutput("dataTab")),
  tabPanel("Plot",
plotOutput("Plots")),
  tabPanel("Output",
           fluidRow(
             column(width = 6, align = "center", textOutput("regSum")),
             column(width = 6, align = "center", textOutput("aovSum"))
           ))
  # tabPanel("Hypotheses",
  #          fluidRow(
  #            column(width = 3, align = "center", textOutput("H0R")),
  #            column(width = 3, align = "center", textOutput("H0A"))           ))
),

### MODEL equation fixed at bottom
box(width = 12, 
    column(width = 6, align = "center", h4("Regression")),
    column(width = 6, align = "center", h4("ANOVA")),
    column(width = 6, align = "center", textOutput("modelR")),
    column(width = 6, align = "center", textOutput("modelA")),
    textOutput("TextGroup"),
    textOutput("TextModelAbs"),
    textOutput("TextModelNum"))

# ##### Step 2a Hypotheses #####


 
##### Step 2 Data sampling #####       


##### Step 3 Output #####        



      #   
      # ),
      # tabItem(tabName = "tab2", box(
      #   width = 12,
      #   align = "center",
      #   h4("add title"),
      #   column(12, align = "left", h5("add content"))
      # )),
      # tabItem(tabName = "tab3", box(
      #   width = 12,
      #   align = "center",
      #   h4("add title"),
      #   column(12, align = "left", h5("add content"))
      # ))
      
      ### If needed, you can add extra tabItems above by simply adding an extra "tabItem(tabName = ...., box(...))" statement (don't forget to add a comma at the end of "tab3")
      
    )
      )



)
#####
)
