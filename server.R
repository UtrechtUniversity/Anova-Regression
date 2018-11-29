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
# colour palette
colcode <- brewer.pal(4, "Dark2")
# sample size per group
sampSize <- 15
DV <- "Score"

###########################
# Server
###########################

server <- function(input, output) {
  
# obtain input ####
  Ngroups <- reactive({as.numeric(input$Ngroups)})
  Ntotal <- reactive({sampSize * Ngroups()})
  
  output$refGroupUI <- renderUI({
    radioButtons("refGroup", "Reference group", choices = groups[[Ngroups()]], selected = groups[[Ngroups()]][1])
  })
  
  refGroupF <- reactive({input$refGroup})
  allGroups <- reactive({groups[[Ngroups()]]})
  allButRef <- reactive({allGroups()[-which(allGroups() == refGroupF())]})
  
  output$dataFormUI <- renderUI({
    if(input$tabselected == 1){
      radioButtons("dataForm", "Data format", choices = c("Group coding" = 1, "Dummy coding" = 2))
    } else {
      if(input$tabselected == 2){
        radioButtons("selectReg", "Show the regression line for...", choices = allGroups()[-which(allGroups() == refGroupF())])
      } else{
        NULL
      }
    }
  })
  
# sample data #####  
  data <- reactive({
    set.seed(123 + input$sample)
    means <- rep(sample(efsizes, Ngroups(), replace = F), each = sampSize)
    Score <- rnorm(Ntotal(), means, 1)
    x <- data.frame(Score = Score, Group = factor(rep(paste(allGroups()), each = sampSize)))
    colnames(x) <- c(DV, "Group")
    x
  })
  
  dataDumm <- reactive({
    x <- cbind("Score" = data()[,1], model.matrix(~Group-1, data()))
    colnames(x) <- c(DV, paste0("D.", allGroups()))
    data.frame(x)
  })
  
##### Data tab #####
  dataFormat <- reactive({as.numeric(input$dataForm)})
  
  output$dataTab <- renderUI({
    set.seed(10)
    rowSelect <- sample(1:(Ntotal()), 10, replace = FALSE)
    if(identical(dataFormat(), 2)){
      dataT <- dataDumm()[, -which(names(dataDumm()) %in% paste0("D.", refGroupF()))]
    } else {
      dataT <- data()
    }
    tab <- dataT[rowSelect,]
    tab[1] <- round(tab[1],2)
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
  plotReg <- reactive(input$selectReg)
  
  output$Plots <- renderPlot({
    # data manipulation #####
    width_of_things <- .05*Ngroups()    # jitter
    mean_df <- data.frame(aggregate(data()$Score, by = list(data()$Group), FUN=mean), grandmean = mean(data()$Score))
    names(mean_df) <- c("Group", "groupmean", "grandmean")
    mean_df$GroupC <- as.numeric(mean_df$Group) # numeric group for axis
    mean_df$refmean <- mean_df$groupmean[which(mean_df$Group == refGroupF())[1]] # reference mean (intercept)
    mean_df$dumm <- ifelse(mean_df$groupmean == mean_df$refmean, 0, 1) # dummy coding reference group or not
    refmean <- mean_df$refmean[1]
    plotmean <- mean_df$groupmean[mean_df$Group == plotReg()][1]-refmean
    dummmeans <- mean_df$groupmean[mean_df$dumm==1]-refmean
    # create dataset merged with the group and grand mean and reference coding
    dataM <- merge(data(), mean_df, by = "Group")
    dataM$dumm <- ifelse(dataM$Group == refGroupF(), 0, 1) # column indicating whether case is in reference group or not
    set.seed(1305)
    dataM$dummC <- as.numeric(dataM$dumm)+rnorm(sampSize*Ngroups(), sd = .5*width_of_things)
    set.seed(1305)
    dataM$GroupC <- as.numeric(dataM$Group)+rnorm(sampSize*Ngroups(), sd = .5*width_of_things)
    
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
          xend = Ngroups() + (1.5 * width_of_things),
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
          xend = Ngroups() + (1.5 * width_of_things),
          y = refmean,
          yend = refmean
        ),
        colour = colcode[refGroupF()],
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
      scale_x_continuous(breaks = 1:Ngroups(),
                         labels = allGroups()) + 
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
  
  
##### Output tab #####
  output$AOVout <- renderTable(rownames = TRUE, caption = "F-table",
                               caption.placement = getOption("xtable.caption.placement", "top"), 
                               caption.width = getOption("xtable.caption.width", NULL), {
                                 return(anova(lm(Score ~ Group, data = data())))
                               })
  output$AOVmeans <- renderTable(rownames = TRUE, caption = "Means",
                                 caption.placement = getOption("xtable.caption.placement", "top"), 
                                 caption.width = getOption("xtable.caption.width", NULL), {
                                   return(summary(lm(Score ~ -1+Group, data = data()))$coefficients)
                                 })
  
  output$regout <- renderTable(rownames = TRUE, caption = "F-table",
                               caption.placement = getOption("xtable.caption.placement", "top"), 
                               caption.width = getOption("xtable.caption.width", NULL), {
                                 if(length(refGroupF())==1){
                                   dataR <- within(data(), Group <- relevel(Group, ref = refGroupF()))
                                   return(anova(lm(Score ~ Group, data = dataR)))
                                 }
                               })
  
  output$regest <- renderTable(rownames = TRUE, caption = "Estimates",
                               caption.placement = getOption("xtable.caption.placement", "top"), 
                               caption.width = getOption("xtable.caption.width", NULL), {
                                 if(length(refGroupF())==1){
                                   dataR <- within(data(), Group <- relevel(Group, ref = refGroupF()))
                                   return(summary(lm(Score ~ Group, data = dataR))$coefficients)
                                 }
                               })
  
##### Equations #####
  
  output$modelR <- renderText({
    reg <- paste("b", 1:(Ngroups()-1), "* D.", allButRef(), sep = "", collapse = " + ")
    paste0("Pred.", DV, " = b0 + ", reg, sep = "")
  })
  
  output$modelA <- renderText({
    mus <- paste("Mean.", allGroups(), "* D.", allGroups(), sep = "", collapse = " + ")
    paste0("Pred.", DV, " = ", mus, sep = "")
  })
  
  linMod <- reactive({
    if(length(refGroupF())==1){
      dataR <- within(data(), Group <- relevel(Group, ref = refGroupF()))
      eval(parse(text = paste("lm(", DV, "~Group, dataR)")))
    }
  })
  
  output$modelRnum <- renderText({
    if(length(linMod()) > 0){
      reg <- paste(round(linMod()$coefficients[-1], 2), "* D.", allButRef(), sep = "", collapse = " + ")
      
      paste("Pred.", DV, " = ", round(linMod()$coefficients[1], 2)," + ", reg, sep = "")
    }
  })
  
  aovMod <- reactive({
    eval(parse(text = paste("lm(", DV, "~Group-1, data())")))
  })
  
  output$modelAnum <- renderText({
    mus <- paste(round(aovMod()$coefficients, 2), "* D.", allGroups(), sep = "", collapse = " + ")
    paste0("Pred.", DV, " = ", mus, sep = "")
  })
  
}

