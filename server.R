###########################
# function for colourtable
###########################
# function derived from the highlightHTMLcells() function of the highlightHTML package
colortable <- function(htmltab, css, style="table-condensed table-bordered"){
  tmp <- str_split(htmltab, "\n")[[1]]
  CSSid <- gsub("\\{.+", "", css)
  CSSid <- gsub("^[\\s+]|\\s+$", "", CSSid)
  CSSidPaste <- gsub("#", "", CSSid)
  CSSid2 <- paste(" ", CSSid, sep = "")
  ids <- paste0("<td id='", CSSidPaste, "'")
  for (i in 1:length(CSSid)) {
    locations <- grep(CSSid[i], tmp)
    tmp[locations] <- gsub("<td", ids[i], tmp[locations])
    tmp[locations] <- gsub(CSSid2[i], "", tmp[locations],
                           fixed = TRUE)
  }
  htmltab <- paste(tmp, collapse="\n")
  Encoding(htmltab) <- "UTF-8"
  list(
    tags$style(type="text/css", paste(css, collapse="\n")),
    tags$script(sprintf(
      '$( "table" ).addClass( "table %s" );', style
    )),
    HTML(htmltab)
  )
}

###########################
# Default values
###########################
# effect sizes to sample from
efsizes <- c(0,2,4,6)
# group names
groups <- list("Null" = c(NULL), 
               "Gender" = c("Female", "Male"), 
               "Condition" = c("Control", "Experimental", "Placebo"), 
               "Diet" = c("Carnivore", "Pescetarian", "Vegan", "Vegetarian"))
# colour palette
colcode <- brewer.pal(4, "Dark2")
# sample size per group
sampSize <- 15

###########################
# Server
###########################

server <- function(input, output) {
  
##### Input / output #####
  
Ngroups <- reactive({as.numeric(input$Ngroups)})

output$refGroupUI <- renderUI({
  radioButtons("refGroup", "Reference group", choices = groups[[Ngroups()]], selected = groups[[Ngroups()]][1])
})

refGroup <- reactive({input$refGroup})


data <- reactive({
  set.seed(123 + input$sample)
  means <- rep(sample(efsizes, Ngroups(), replace = F), each = sampSize)
  Score <- rnorm(sampSize*Ngroups(), means, 1)
  data.frame(Score = Score, Group = factor(rep(paste(groups[[Ngroups()]]), each = sampSize)))
})

dataDumm <- reactive({
  x <- cbind("Score" = data()[,1], model.matrix(~Group-1, data()))
  colnames(x) <- c("Score", paste0("Dummy", groups[[Ngroups()]]))
  data.frame(x)
})

##### Data tab #####

dataFormat <- reactive({as.numeric(input$dataForm)})

output$dataTab <- renderUI({
  # if(is.null(input$sample)){return(NULL)}
  set.seed(10)
  rowSelect <- sample(1:(Ngroups()*sampSize), 10, replace = F)
  if(dataFormat() == 2){
    dataT <- dataDumm()[, -which(names(dataDumm()) %in% paste0("Dummy", refGroup()))]
  } else {
    dataT <- data()
  }
tab <- dataT[rowSelect,]
rownames(tab) <- NULL
  # define CSS tags
csstext <- paste0("\"#", groups[[Ngroups()]], " {color: ", colcode[1:Ngroups()], ";}\"", collapse = ", ")
  css <- paste0("c(", csstext, ")", collapse = "")
  
  # add the tag inside the cells
  tab <- apply(tab, 2, function(x) paste(x, paste("#", data()[rowSelect,2], sep ="")))
  
  # generate html table with pander package and markdown package
  htmltab <- markdownToHTML(
    text = pandoc.table.return(
      tab,
      style="rmarkdown", split.tables=Inf
    ),
    fragment.only=TRUE
  )
  colortable(htmltab, eval(parse(text = css)))
})

##### Plot tab #####
output$Plots <- renderPlot({
  
  # data manipulation #####
  width_of_things <- .1    # jitter
  
  mean_df <- data.frame(aggregate(data()$Score, by = list(data()$Group), FUN=mean), grandmean = mean(data()$Score))
  names(mean_df) <- c("Group", "groupmean", "grandmean")
  mean_df$GroupC <- as.numeric(mean_df$Group) # numeric group for axis
  mean_df$refmean <- mean_df$groupmean[which(as.numeric(mean_df$Group) == refGroup())[1]] # reference mean (intercept)
  mean_df$dumm <- ifelse(mean_df$GroupC == refGroup(), 0, 1) # dummy coding reference group or not
  print(mean_df$dumm)
  mean_df_ref <- mean_df[mean_df$dumm == 1,] # subset containing only other than reference group data

  dataM <- merge(data(), mean_df, by = "Group")
  dataM$dumm <- ifelse(as.numeric(dataM$Group) == refGroup(), 0, 1)
  dataM$dummC <- as.numeric(dataM$dumm)+rnorm(sampSize*Ngroups(), sd = .5*width_of_things)
  dataM$GroupC <- as.numeric(dataM$Group)+rnorm(sampSize*Ngroups(), sd = .5*width_of_things)

  
 # anova plot ##### 
  aov_plot <- ggplot(dataM, aes(x = GroupC, y = Score)) + 
    # individual points #####
    geom_point(aes(colour = Group, pch = Group)) + 
    
    # group means #####
    geom_segment(
      data = mean_df, aes(
        x = (GroupC - (1.5 * width_of_things)),
        xend = (GroupC + (1.5 * width_of_things)),
        y = groupmean,
        yend = groupmean,
        colour = Group),
      linetype = 1,
      size = 1)  + 
 
    # grand mean #####
      geom_segment(
      data = mean_df, aes(
        x = 1 - (1.5 * width_of_things),
        xend = Ngroups() + (1.5 * width_of_things),
        y = grandmean,
        yend = grandmean),
      colour = "black",
      linetype = 1,
      size = .5) +    

    # longer reference group line #####
    geom_segment(
      data = mean_df,
      aes(
        x = 1 - (1.5 * width_of_things),
        xend = Ngroups() + (1.5 * width_of_things),
        y = refmean,
        yend = refmean
      ),
      colour = colcode[refGroup()],
      linetype = 2,
      size = .5
    ) + 
    
    # difference Scores #####
    geom_segment(
      data = mean_df_ref,
      aes(
        x = GroupC,
        xend = GroupC,
        y = groupmean,
        yend = refmean,
        colour = Group
      ),
      linetype = 3,
      size = .5
    ) +
    
    # colour scale #####
    scale_color_manual(values = colcode) +

    # theme labels and axes #####
      theme_bw() +
      scale_x_continuous(breaks = 1:Ngroups(),
                       label = names(data()$Group)) + 
      theme(axis.title.x = element_blank())
  
 # end anova plot #####
  
  # regression plot #####
  reg_plot <- ggplot(dataM, aes(x = dummC, y = Score)) + 
    
    # points #####
    geom_point(aes(color = Group, pch = Group)) + 
    
    # group means #####
    geom_segment(
      data = mean_df, aes(
        x = (dumm - (1.5 * width_of_things)),
        xend = (dumm + (1.5 * width_of_things)),
        y = groupmean,
        yend = groupmean,
        colour = Group
      ),
      linetype = 1,
      size = 1
    ) + 
    
    # regression lines #####
    geom_segment(
      data = mean_df_ref, aes(
        x = 0, 
        xend = 1, 
        y = refmean, 
        yend = groupmean,
        color = Group)
    ) + 
    # scale color #####
    scale_color_manual(values = colcode) +
    
    # theme labels and axes #####
    theme_bw() +
    scale_x_continuous(breaks = 0:1,
                       label = c("Reference group", "Dummy")) + 
    theme(axis.title.x = element_blank())
# end regression plot #####
  
  # arrange grid #####
  grid.arrange(reg_plot, aov_plot, ncol = 2)
})

##### Output tab #####

##### Hypotheses tab #####
# output$H0R <- renderText({
#   betas <- paste("b", 1:(Ngroups()-1), sep = "", collapse = " = ")
#   paste("H0: ", betas,"= 0")
# })
# output$H0A <- renderText({
#   mus <- paste("mean", 1:Ngroups(), sep = "", collapse = " = ")
#   paste("H0: ", mus)
# })


##### Model banner #####

  output$modelR <- renderText({
    # dummies <- paste("D", 1:(Ngroups()))
    # reg <- paste("b", 1:(Ngroups()-1), "*", dummies[-refGroup()], sep = "", collapse = " + ")
    # paste("Score = b0 + ", reg, "+ e", sep = "")
  })

  output$modelA <- renderText({
    mus <- paste("mean", 1:Ngroups(), "*D", 1:Ngroups(), sep = "", collapse = " + ")
    paste("Score = ", mus, " + e", sep = "")
  })




# Step 2 Sample data ----


# Step 3 Output #####
 
  ###############################
  ###############################
  ##############################
# #  plot maken
#   linMod <- reactive({
#     lm(Score~group, data())
#   })
# 
#   
#   output$regPlot <- renderPlot({
#     g <- ggplot(data(), aes(x = group, y = Score)) + theme_minimal()
#     g + geom_abline(slope = linMod()$coefficients[2], intercept = linMod()$coefficients[1], label = "b1")
#   })
  
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
#     Score <- rnorm(N()*ngroups(), 0, 1)
#     group <- factor(rep(1:ngroups(), each = N()), labels = rep(paste0("Group", 1:ngroups())))
#     modelM <- model.matrix(~group)
#     dataM <- data.frame(cbind(Score, group, modelM))
#   })
#   
#   means <- reactive({
#     data.frame(
#       data()%>%
#       group_by(group) %>%
#       summarise(Mean = mean(Score), 
#                 se = sd(Score)/sqrt(length(Score)), 
#                 n = length(Score)) %>%
#       mutate(Group = factor(group))
#       )
#   })
#     
#     base_means <- reactive({
#     g <- ggplot(data(), aes(x = group, y = Score)) + theme_minimal()
#     # # gg <- ggplot(means(), aes(x = Group, y = Mean)) + theme_minimal()
#     # gg <- g + geom_point(aes(x = group, y = Score))
#     # gg <- ggplot(means(), aes(x= Group, y = Mean)) + theme_minimal()
#     
#   })
#     
#     base_reg <- reactive({
#       fit <- lm(Score~groupGroup2, data())
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
#     #   ylab("Mean Score") + ggtitle("Means and standard errors")
#     base_means() + geom_crossbar(data = means(), aes(x = Group, y = Mean, ymin = Mean - 2*se, ymax = Mean + 2*se, color = Group)) + 
#                    geom_point(data = data(), aes(x = factor(group), y = Score, color = factor(group))) +
#       ylab("Mean Score") + ggtitle("Means and standard errors")
#   })
#   
#   output$Fig_reg <- renderPlot({
#     base_means() + geom_point(data = data(), aes(x = factor(group-1), y = Score, color = factor(group-1))) +
#                    stat_smooth(data = data(), aes(x = groupGroup2, y = Score), method = "lm")
#   })
}

