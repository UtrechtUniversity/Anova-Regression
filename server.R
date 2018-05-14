library(ggplot2)
library(tidyr)
library(dplyr)

efsizes <- c(0, .2,.5,.8)
server <- function(input, output) {
  
##### Input / output #####
  
Ngroups <- reactive({as.numeric(input$Ngroups)})

output$refGroupUI <- renderUI({
  selectInput("refGroup", "Reference group", choices = 1:Ngroups())
})
  
refGroup <- reactive({as.numeric(input$refGroup)})

truePop <- reactive({as.numeric(input$selectHyp)})

sampSize <- reactive({as.numeric(input$sampleSize)})
  
data <- eventReactive(input$sample, {
  set.seed(123+input$sample)
  means <- rep(seq(0, by = efsizes[truePop()], length.out = Ngroups()), each = sampSize())
  score <- rnorm(sampSize()*Ngroups(), means, 1)
  group <- rep(1:Ngroups(), each = sampSize())
  data.frame(score = score, group = factor(group))
  })

dataDumm <- reactive({
  x <- cbind("score" = data()[,1], model.matrix(~group-1, data()))
  data.frame(x)
})

##### Data tab #####

dataFormat <- reactive({as.numeric(input$dataForm)})


## data tab for data() doesnt work!
output$dataTab <- renderTable({
  if(is.null(input$sample)){NULL}
  if(dataFormat() == 1){data()}
  if(dataFormat() == 2){dataDumm()[,-(refGroup()+1)]}
})


# Step 2a Hypotheses #####

  
  output$modelR <- renderText({
    dummies <- paste("D", 1:(Ngroups()))
    reg <- paste("b", 1:(Ngroups()-1), "*", dummies[-refGroup()], sep = "", collapse = " + ")
    paste("score = b0 + ", reg, "+ e", sep = "")
  })

  output$modelA <- renderText({
    mus <- paste("mean", 1:Ngroups(), "*D", 1:Ngroups(), sep = "", collapse = " + ")
    paste("score = ", mus, " + e", sep = "")
  })

  output$H0R <- renderText({
    betas <- paste("b", 1:(Ngroups()-1), sep = "", collapse = " = ")
    paste("H0: ", betas,"= 0")
  })
  output$H0A <- renderText({
    mus <- paste("mean", 1:Ngroups(), sep = "", collapse = " = ")
    paste("H0: ", mus)
  })


# Step 2 Sample data ----


# Step 3 Output #####
 
  ###############################
  ###############################
  ##############################
#  plot maken
  linMod <- reactive({
    lm(score~group, data())
  })

  
  output$regPlot <- renderPlot({
    g <- ggplot(data(), aes(x = group, y = score)) + theme_minimal()
    g + geom_abline(slope = linMod()$coefficients[2], intercept = linMod()$coefficients[1], label = "b1")
  })
  
  # Step 3b text output #####
regSumT <- eventReactive(input$sample, {
  summary(linMod())
})
  
  output$regSum <- renderText(paste(regSumT()))
  
  
# REMAINING CODE #####  
# 
#   
# # second section 
# 
# 
# #   ## #input 
#   ngroups <- reactive(as.numeric(input$Ngroups))
#   N <- reactive(as.numeric(input$N))
#   
#   data <- eventReactive(input$genData, {
#     score <- rnorm(N()*ngroups(), 0, 1)
#     group <- factor(rep(1:ngroups(), each = N()), labels = rep(paste0("Group", 1:ngroups())))
#     modelM <- model.matrix(~group)
#     dataM <- data.frame(cbind(score, group, modelM))
#   })
#   
#   means <- reactive({
#     data.frame(
#       data()%>%
#       group_by(group) %>%
#       summarise(Mean = mean(score), 
#                 se = sd(score)/sqrt(length(score)), 
#                 n = length(score)) %>%
#       mutate(Group = factor(group))
#       )
#   })
#     
#     base_means <- reactive({
#     g <- ggplot(data(), aes(x = group, y = score)) + theme_minimal()
#     # # gg <- ggplot(means(), aes(x = Group, y = Mean)) + theme_minimal()
#     # gg <- g + geom_point(aes(x = group, y = score))
#     # gg <- ggplot(means(), aes(x= Group, y = Mean)) + theme_minimal()
#     
#   })
#     
#     base_reg <- reactive({
#       fit <- lm(score~groupGroup2, data())
#     })
#   
#   
#   # text hypotheses
# #   output$H_aov <- renderText({
# # "    Htest"
# #     })
# #   output$H_reg <- renderText({
# #     "Htest"
# #   })
# 
#   output$Fig_aov <- renderPlot({
#     # gg <- ggplot(means(), aes(x= Group, y = Mean)) + theme_minimal()
#     # base_means() + geom_crossbar(data = means(), aes(x = Group, y = Mean, ymin = Mean - 2*se, ymax = Mean + 2*se, color = Group, fill = Group)) + 
#     #   ylab("Mean score") + ggtitle("Means and standard errors")
#     base_means() + geom_crossbar(data = means(), aes(x = Group, y = Mean, ymin = Mean - 2*se, ymax = Mean + 2*se, color = Group)) + 
#                    geom_point(data = data(), aes(x = factor(group), y = score, color = factor(group))) +
#       ylab("Mean score") + ggtitle("Means and standard errors")
#   })
#   
#   output$Fig_reg <- renderPlot({
#     base_means() + geom_point(data = data(), aes(x = factor(group-1), y = score, color = factor(group-1))) +
#                    stat_smooth(data = data(), aes(x = groupGroup2, y = score), method = "lm")
#   })
}

