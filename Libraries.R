#Load libs
packs <- c("haven",  "broom", "forcats", "gtools",  "ggbeeswarm", "ggsci", "splines", "gridExtra", "gt", "bstfun", "Hmisc",
           "stringr", "doParallel", "tidyr", "ggplot2", "flextable", "ggpubr", "officer", "mmtable2", "patchwork", "data.table",
           "tidyverse", "dplyr", "aod", "fastsurvey", "qgam", "rsconnect")
lapply(packs, require, character.only = TRUE)
options(survey.lonely.psu = "adjust")
# source("C:/Ahmed's Stuff/ResearchStuff/Multi-purpose R code/Multi-purpose functions.R")

#Function to format P-values
p <- function(p) {
  ifelse(p < 0.001, "<0.001", ifelse(round(p, 2) == 0.05 | round(p, 2) == 0, round(p, 3) %>% format(nsmall = 3), round(p, 2) %>% format(nsmall = 2))) %>% as.character
}


#Insert a line for a 2-by-2 grid
grid_2by2 <- function() {
  
  hrzl_coords = c(0.5, 0.5, 0, 1)
  vert_coords = c(0, 1, 0.5, 0.5)
  line_id = c(1, 1, 2, 2)
  grid.polygon(hrzl_coords, vert_coords, line_id, gp = gpar(lwd = 2))
  }

#Insert 3 lines for a 3-by-2 grid
grid_3by2 <- function() {
  
  hrzl_coords = c(0.5, 0.5, 0, 1, 0, 1)
  vert_coords = c(0, 1, 0.333333, 0.333333, 0.666666, 0.666666)
  line_id = c(1, 1, 2, 2, 3, 3)
  grid.polygon(hrzl_coords, vert_coords, line_id, gp = gpar(lwd = 2))
  }


#Perform some aesthetic modifications for flextables
flextable_aes <- function(table, table_title) {
  
  #Table font
  table <- flextable::font(table, fontname = "Times New Roman", part = "all")
  
  #Align everything centrally
  table <- flextable::align(table, align = "center", part = "all")
  
  #Table heading
  table <- flextable::set_caption(table, caption = table_title,
                                  fp_p = fp_par(text.align = "left"))
  
  #Align footer to the left
  table <- flextable::align(table, align = "left", part = "footer")
  
  #Bold header
  table <- flextable::bold(table, part = "header")
  
  #Set Table width
  table <- flextable::width(table, width = 1.5, unit = "in")
  
  #Bold
  table <- flextable::bold(table, part = "header")
  
  #Print
  table
}

#Function to insert footnote symbol in the correct order
footnote_symbol <- function(i) {
  footnote_symbols <- c("\U002A", "\U2020", "\U2021", "\U00A7")
  footnote_symbols[i]
}

#Function to round to convert to % and round (TABLE FORMAT)
pct_table <- function(x) {
  x %>%
    {. * 100} %>%
    format(digits = 1, nsmall = 1, trim = TRUE)
}
