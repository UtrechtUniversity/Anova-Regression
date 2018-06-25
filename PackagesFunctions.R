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
library(datasets)
library(base)
library(utils)
library(stats)
library(graphics)
library(grDevices)
library(methods)


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
DV <- "Score"

###########################
# Colour
###########################

uu_color <- " #ffcd00"