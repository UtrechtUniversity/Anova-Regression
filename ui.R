##### Loading packages ##########################

# install.packages("shiny")
require("shiny")

# install.packages("shinydashboard")
library("shinydashboard")


#####

ui <- dashboardPage(
# Appearance I #####
    skin = "black",
  
  dashboardHeader(title = "ANOVA = REGRESSION", titleWidth = 350),
# Dashboard #####
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("", tabName = "home", icon = icon("home")),
      menuItem("ANOVA = Regression", tabName = "tab1"),
      menuItem("add 2nd tab name", tabName = "tab2"),
      menuItem("add 3rd tab name", tabName = "tab3"),
      menuItem("Disclaimer", tabName = "Disclaimer"),
      HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
      img(src = 'cm_hs_uu-logoengels_diapositief_rgb.png', align = "left")
      ### you can easily add extra tabs by including an extra "menuItem("...", tabName = "")," before the disclaimer ###
      ### you can remove or add <br> statements in the HTML function (menuItem("Disclaimer")) to adjust the position of the UU logo (make sure it is approximately at the bottom of the screen when opened)
    )
  ),
  
# Appearance II #####  
  dashboardBody(
    # CSS styles
    tags$style(HTML(".irs-bar {background: #EAC626}")),
    tags$style(HTML(".irs-bar {border-top: 1px solid black}")),
    tags$style(HTML(".irs-bar-edge {background: #EAC626}")),
    tags$style(HTML(".irs-bar-edge {border: 1px solid black}")),
    tags$style(HTML(".irs-single {background: #EAC626}")),
    tags$style(HTML(
      ".selectize-input {border-color: #EAC626}"
    )),
    tags$style(HTML(
      ".selectize-dropdown {border-color: #EAC626}"
    )),
    
    ### note that #EAC626 is the mustard yellow color used in the sidebar. ###
    ### If possible, you can use this color + different shades of grey (+ black & white) in your figures. ###
    
    tags$head(tags$style(
      HTML(
        '.skin-black .main-header .logo {
        background-color: #EAC626;
        }
        .skin-black .main-header .logo:hover {
        background-color: #EAC626;
        }
        
        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #EAC626;
        }
        
        /* navbar (rest of the header) */
        .skin-black .main-header .navbar {
        background-color: #EAC626;
        }
        
        /* toggle button when hovered  */
        .skin-black .main-header .navbar .sidebar-toggle:hover{
        background-color: #EAC626;
        }
        
        /* other links in the sidebarmenu when hovered */
        .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #EAC626;
        }
        /* other links in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu a{
        background-color: #EAC626;
        color: #000000;
        }
        
        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #000000;
        color: #FFFFFF;
        }
        
        .skin-black .main-sidebar {color: #000000; background-color: #EAC626;}
        
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
      tabItem(tabName = "home", box(
        width = 12,
        align = "center",
        h4("Welcome"),
        column(12, align = "left", h5("add app background info"))
      )),
## Tab 1 AOV = REG #####      
      tabItem(
        tabName = "tab1",
        
### Step 1 Predictor #####
        
box(width = 12, align = "center",
    h4("ANOVA = Regression"),
    column(12, align = "left", h5("Continuous outcome, categorical predictor")),
    column(12, align = "left", h4("Step 1: Predictor")),
    
    fluidRow(
      column(width = 3,
             numericInput("Ngroups", "Number of groups", value = 2, min = 2, max = 4, step = 1)
      ),
      column(width = 3, offset = 1,
             uiOutput("refGroupUI")
      )
    )
),

##### Step 2a Hypotheses #####
        column(width = 6, align = "center", h4("Regression")),
        column(width = 6, align = "center", h4("ANOVA")),
        column(width = 6, align = "center", textOutput("modelR")),
        column(width = 6, align = "center", textOutput("modelA")),
        column(width = 3, align = "center", textOutput("H0R")),
        column(width = 3, align = "center", h5("Ha: Not H0")),
        column(width = 3, align = "center", textOutput("H0A")),
        column(width = 3, align = "center", h5("Ha: Not H0")),

 
##### Step 2 Data sampling #####       
        box(width = 12, align = "left",
          h4("Step 2: Data"),
          column(width = 3,
                 selectInput("selectHyp", "Sample from hypothesis", choices = c("H0" = 1, "Ha" = 2))
          ),
        column(width = 3, offset = 1,
               uiOutput("efSizeUI")),
        column(width = 3, offset = 1,
               numericInput("sampleSize", "Sample size", value = 20)
        ),
        column(width = 12, align = "center",
               actionButton("sample", "Sample data")
        )
        ),

##### Step 3 Output #####        
        column(width = 6, align = "center", plotOutput("regPlot")),
        column(width = 6, align = "center", plotOutput("aovPlot")),

        column(width = 6, align = "center", textOutput("regSum")),
        column(width = 6, align = "center", textOutput("aovSum"))
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
