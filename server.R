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
               "Diet" = c("Omnivore", "Pescetarian", "Vegan", "Vegetarian"))
groupsShort <- list("Null" = c(NULL), 
               "Gender" = c("Fe", "Ma"), 
               "Condition" = c("Co", "Ex", "Pl"), 
               "Diet" = c("Om", "Pe", "Vn", "Vt"))

# colour palette
colcode <- brewer.pal(4, "Dark2")
# sample size per group
sampSize <- 15
DV <- "Score"

###########################
# Server
###########################

server <- function(input, output) {
  # 
  # # obtain input ####
  # Ngroups <- reactive({as.numeric(input$Ngroups)})
  # Ntotal <- reactive({sampSize * as.numeric(input$Ngroups)})

  output$refGroupUI <- renderUI({
    cur_Ngroups <- as.numeric(input$Ngroups)
     radioButtons("refGroup", "Reference group", 
                  choices = groups[[cur_Ngroups]], 
                  selected = groups[[cur_Ngroups]][1])
  })
# 
#   refGroupF <- reactive({input$refGroup})
#   allGroups <- reactive({groups[[as.numeric(input$Ngroups)]]})
#   allButRef <- reactive({groups[[as.numeric(input$Ngroups)]][-which(groups[[as.numeric(input$Ngroups)]] == input$refGroup)]})

  # output$dataFormUI <- renderUI({
  #   if(input$tabselected == 1){
  #     radioButtons("dataForm", "Data format", choices = c("Group coding" = 1, "Dummy coding" = 2))
  #   } else {
  #     if(input$tabselected == 2){
  #       radioButtons("selectReg", "Show the regression line for...", choices = as.numeric(input$Ngroups)[-which(as.numeric(input$Ngroups) == input$refGroup)])
  #     } else{
  #       NULL
  #     }
  #   }
  # })
  # 
  # 
  # 
  # # sample data #####  
  data <- reactive({

    cur_Ngroups <- as.numeric(input$Ngroups)
    cur_allGroups <- groups[[as.numeric(input$Ngroups)]]

    set.seed(123 + input$sample)

    means <- rep(sample(efsizes, cur_Ngroups, replace = F), each = sampSize)
    Score <- rnorm(sampSize * cur_Ngroups, means, 1)
    df <- data.frame(Score = Score, Group = factor(rep(paste(cur_allGroups), each = sampSize)))
    colnames(df) <- c(DV, "Group")
    return(df)
  })
  # 
  dataDumm <- reactive({
    cur_data <- data()
    cur_allGroups <- groups[[as.numeric(input$Ngroups)]]

    df <- cbind("Score" = data()[,1], model.matrix(~Group-1, cur_data))
    colnames(df) <- c(DV, paste0("D.", cur_allGroups))
    return(data.frame(df))
  })
  # 
  # ##### Data tab #####
  dataFormat <- reactive({as.numeric(input$dataForm)})

  output$dataTab <- renderUI({
    set.seed(10)
    cur_Ntotal <- sampSize * as.numeric(input$Ngroups)
    cur_Ngroups <- as.numeric(input$Ngroups)
    cur_dataFormat <- as.numeric(input$dataForm)
    cur_data <- data()
    cur_dataDumm <- dataDumm()
    cur_refGroupF <- input$refGroup

    rowSelect <- sample(1:cur_Ntotal, 10, replace = FALSE)
    if(identical(cur_dataFormat, 2)){
      dataT <- cur_dataDumm[, -which(names(cur_dataDumm) %in% paste0("D.", cur_refGroupF))]
    } else {
      dataT <- cur_data
    }
    tab <- dataT[rowSelect,]
    tab[1] <- round(tab[1],2)
    rownames(tab) <- NULL
    # define CSS tags
    csstext <- paste0("\"#", groups[[cur_Ngroups]], " {color: ", colcode[1:cur_Ngroups], ";}\"", collapse = ", ")
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
  # 
  # ##### Plot tab #####
  output$regPlot <- renderUI({
    cur_refGroupF <- input$refGroup
    cur_allGroups <- groups[[as.numeric(input$Ngroups)]]
    if(cur_refGroupF %in% cur_allGroups){
    radioButtons("selectReg", "Show the regression line for...", 
                 choices = cur_allGroups[-which(cur_allGroups == cur_refGroupF)])}
  })

  output$Plots <- renderPlot({
    cur_data <- data()
    cur_Ngroups <- as.numeric(input$Ngroups)
    cur_refGroupF <- input$refGroup
    cur_plotReg <- input$selectReg
    cur_allGroups <- groups[[as.numeric(input$Ngroups)]]

    # data manipulation #####
    width_of_things <- .05*cur_Ngroups    # jitter
    mean_df <- data.frame(aggregate(cur_data$Score, by = list(cur_data$Group), FUN=mean),
                          grandmean = mean(cur_data$Score))
    names(mean_df) <- c("Group", "groupmean", "grandmean")
    mean_df$GroupC <- as.numeric(mean_df$Group) # numeric group for axis
    mean_df$refmean <- mean_df$groupmean[which(mean_df$Group == cur_refGroupF)[1]] # reference mean (intercept)
    mean_df$dumm <- ifelse(mean_df$groupmean == mean_df$refmean, 0, 1) # dummy coding reference group or not
    refmean <- mean_df$refmean[1]
    plotmean <- mean_df$groupmean[mean_df$Group == cur_plotReg][1]-refmean
    dummmeans <- mean_df$groupmean[mean_df$dumm==1]-refmean
    # create dataset merged with the group and grand mean and reference coding
    dataM <- merge(cur_data, mean_df, by = "Group")
    dataM$dumm <- ifelse(dataM$Group == cur_refGroupF, 0, 1) # column indicating whether case is in reference group or not
    set.seed(1305)
    dataM$dummC <- as.numeric(dataM$dumm)+rnorm(sampSize*cur_Ngroups, sd = .5*width_of_things)
    set.seed(1305)
    dataM$GroupC <- as.numeric(dataM$Group)+rnorm(sampSize*cur_Ngroups, sd = .5*width_of_things)

    # anova plot #####
    aov_plot <- ggplot(dataM, aes(x = GroupC, y = Score)) +
      # individual points
      geom_point(aes(colour = Group, pch = Group)) +
      # group means
      geom_segment(
        data = mean_df, aes(
          x = (GroupC - (1.5 * width_of_things)),
          xend = (GroupC + (1.5 * width_of_things)),
          y = groupmean,
          yend = groupmean,
          colour = Group),
        linetype = 1,
        size = 1)  +
      # grand mean
      geom_segment(
        data = mean_df, aes(
          x = 1 - (1.5 * width_of_things),
          xend = cur_Ngroups + (1.5 * width_of_things),
          y = grandmean,
          yend = grandmean),
        colour = "black",
        linetype = 1,
        size = 1) +
      # longer reference group line
      geom_segment(
        data = mean_df,
        aes(
          x = 1 - (1.5 * width_of_things),
          xend = cur_Ngroups + (1.5 * width_of_things),
          y = refmean,
          yend = refmean
        ),
        colour = colcode[cur_refGroupF],
        linetype = 2,
        size = 1
      ) +
      # difference Scores
      geom_segment(
        data = mean_df,
        aes(
          x = GroupC,
          xend = GroupC,
          y = groupmean,
          yend = grandmean
        ),
        colour = "black",
        linetype = 2,
        size = 1
      ) +
      #colour scale
      scale_color_manual(values = colcode) +
      # theme labels and axes
      theme_bw() +
      scale_x_continuous(breaks = 1:cur_Ngroups,
                         labels = cur_allGroups) +
      theme(axis.title.x = element_blank())

    # regression plot #####
    reg_plot <- ggplot(dataM, aes(x = dummC, y = Score)) +
      # points
      geom_point(aes(color = Group, pch = Group)) +
      # group means
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
      # regression lines
      geom_abline(intercept = refmean, slope = plotmean,
                  linetype = 1,
                  size = 1, na.rm = TRUE) +
      geom_text(aes(x = 0.5, y= refmean[1] + .5*plotmean[1]), label=round(plotmean[1], 2), vjust=-1)+
      geom_abline(intercept = refmean, slope = dummmeans,
                  linetype = 2,
                  size = 1, na.rm = TRUE) +
      # scale color
      scale_color_manual(values = colcode) +
      # theme labels and axes
      theme_bw() +
      scale_x_continuous(breaks = 0:1,
                         labels = c("0", "1")) +
      theme(axis.title.x = element_blank())
    # arrange grid ####
    grid.arrange(reg_plot, aov_plot, ncol = 2)
  })
  # 
  # 
  # ##### Output tab #####

  # dataR <- reactive({if(length(refGroupF())==1){within(data(), Group <- relevel(Group, ref = refGroupF()))}})
  # regmodel <- reactive({lm(Score~Group, data = dataR())})

  linMod <- reactive({
    cur_refGroupF <- input$refGroup
    if(length(cur_refGroupF)==1){
      dataR <- within(data(), Group <- relevel(Group, ref = cur_refGroupF))
      eval(parse(text = paste("lm(", DV, "~Group, dataR)")))
    }
  })

  output$AOVout <- renderTable(rownames = TRUE, caption = "F-table",
                               caption.placement = getOption("xtable.caption.placement", "top"),
                               caption.width = getOption("xtable.caption.width", NULL), {
                                 return(anova(lm(Score ~ Group, data = data())))
                                 # return(anova(AOVmodel()))
                               })
  output$AOVmeans <- renderTable(rownames = TRUE, caption = "Means",
                                 caption.placement = getOption("xtable.caption.placement", "top"),
                                 caption.width = getOption("xtable.caption.width", NULL), {
                                   return(summary(lm(Score ~ -1+Group, data = data()))$coefficients)
                                   # return(summary(AOVmodelmeans())$coefficients)
                                 })

  output$regout <- renderTable(rownames = TRUE, caption = "F-table",
                               caption.placement = getOption("xtable.caption.placement", "top"),
                               caption.width = getOption("xtable.caption.width", NULL), {
                                 # cur_refGroupF <- input$refGroup
                                 # if(length(cur_refGroupF)==1){
                                 #   dataR <- within(data(), Group <- relevel(Group, ref = cur_refGroupF))
                                 #   return(anova(lm(Score ~ Group, data = dataR)))
                                 # }
                                 return(anova(linMod()))
                               })

  output$regest <- renderTable(rownames = TRUE, caption = "Estimates",
                               caption.placement = getOption("xtable.caption.placement", "top"),
                               caption.width = getOption("xtable.caption.width", NULL), {
                                 # cur_refGroupF <- input$refGroup
                                 # if(length(cur_refGroupF)==1){
                                 #   dataR <- within(data(), Group <- relevel(Group, ref =cur_refGroupF))
                                 #   return(summary(lm(Score ~ Group, data = dataR))$coefficients)
                                 # }
                                 return(summary(linMod())$coefficients)
                               })
  # 
  # ##### Equations #####
  # 
  output$modelR <- renderText({
    cur_allButRef <- groupsShort[[as.numeric(input$Ngroups)]][-which(groups[[as.numeric(input$Ngroups)]] == input$refGroup)]
    cur_Ngroups <- as.numeric(input$Ngroups)
    reg <- paste("b", 1:(cur_Ngroups-1), "* D", cur_allButRef, sep = "", collapse = " + ")
    paste0("Pred = b0 + ", reg, sep = "")
  })

  output$modelA <- renderText({
    cur_allGroups <- groupsShort[[as.numeric(input$Ngroups)]]
    
    mus <- paste("Mean*D.", cur_allGroups, sep = "", collapse = " + ")
    paste0("Pred = ", mus, sep = "")
  })



  output$modelRnum <- renderText({
    cur_allButRef <- groupsShort[[as.numeric(input$Ngroups)]][-which(groups[[as.numeric(input$Ngroups)]] == input$refGroup)]
    if(length(linMod()) > 0){
      reg <- paste(round(linMod()$coefficients[-1], 2), "* D.", cur_allButRef, sep = "", collapse = " + ")

      paste("Pred = ", round(linMod()$coefficients[1], 2)," + ", reg, sep = "")
    }
  })

  aovMod <- reactive({
    eval(parse(text = paste("lm(", DV, "~Group-1, data())")))
  })

  output$modelAnum <- renderText({
    cur_allGroups <- groupsShort[[as.numeric(input$Ngroups)]]
    mus <- paste(round(aovMod()$coefficients, 2), "* D.", cur_allGroups, sep = "", collapse = " + ")
    paste0("Pred = ", mus, sep = "")
  })
  
  # output$modelAref <- renderText({
  #   "Pred = Predicted score \n D.x = dummy coding group x \n Mean = group mean" 
  # })
  # 
  # output$modelRref <- renderText({
  #   "Pred = Predicted score \n b = beta coefficient" 
  # })
  
}

