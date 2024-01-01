#Load libraries
library(shiny)
library(dplyr)
library(qgam)
library(ggplot2)
library(scales)
library(data.table)
library(R.utils)
library(ggpubr)
library(shinythemes)
library(reactable)
#Multiply by 100 and round to 1 decimal place
pr2 <- function(x) {
  x %>%{. * 100} %>% round(2)
}
#Multiply by 100 and round to 0 decimal place
pr0 <- function(x) {
  x %>%{. * 100} %>% round(0)
}

# Load models/apo_b_cdf
load("Calculator pre-requisities.RData") #This loads the needed models

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("darkly"),
 # Application title
 tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
          tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 
                  'What level of apoB should I expect based on my LDL-C (or non-HDL-C)?'),
          htmltools::p('This calculator shows the distribution of apolipoprotein B (apoB) for a given measurement of low-density lipoprotein cholesterol (LDL-C) or non-high-density lipoprotein cholesterol (non-HDL-C)')
 ),
    
 selectInput("measure", HTML("Select type of lipid measurement"), choices = c("LDL-C (Martin-Hopkins)", "LDL-C (Friedewald)", "LDL-C (Sampson)", "non-HDL-C")),
    
    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
          
            uiOutput("slider")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          htmlOutput("prediction"),
          htmlOutput("table_description"),
          reactableOutput("percentiles_table"),
          htmlOutput("line_explanation"),
          plotOutput("plot", width = "100%", height = "600px"),
          htmlOutput("apo_b_target_msg"),
          htmlOutput("caution"),
        )
        
        
    )

    )

# Define server logic to produce predictions
server <- function(input, output) {
  
  #make dynamic slider
  output$slider <- renderUI({
    sliderInput(input = "cholesterol",
                label = "Measured LDL-C or non-HDL-C value:",
                min = slider_min(), max = slider_max(), 
                value = 100,
                ticks = FALSE,
                width = "100%")
  })
  
  

  model <- reactive({switch(input$measure,
                  "LDL-C (Martin-Hopkins)" = qreg_list$ldl_mh,
                  "LDL-C (Sampson)" = qreg_list$ldl_sampson,
                  "LDL-C (Friedewald)" = qreg_list$ldl,
                  "non-HDL-C" = qreg_list$non_hdl)
  })
  
  lipid_name <- reactive({switch(input$measure,
                            "LDL-C (Martin-Hopkins)" = "LDL-C",
                            "LDL-C (Sampson)" = "LDL-C",
                            "LDL-C (Friedewald)" = "LDL-C",
                            "non-HDL-C" = "non-HDL-C")
  })
  
  lipid_var <- reactive({switch(input$measure,
                                     "LDL-C (Martin-Hopkins)" = "ldl_mh",
                                     "LDL-C (Sampson)" = "ldl_sampson",
                                     "LDL-C (Friedewald)" = "ldl",
                                     "non-HDL-C" = "non_hdl")
  })
  
  slider_min <- reactive({switch(input$measure,
                                "LDL-C (Martin-Hopkins)" = 40,
                                "LDL-C (Sampson)" = 40,
                                "LDL-C (Friedewald)" = 40,
                                "non-HDL-C" = 60)
  })
  
  slider_max <- reactive({switch(input$measure,
                                 "LDL-C (Martin-Hopkins)" = 220,
                                 "LDL-C (Sampson)" = 220,
                                 "LDL-C (Friedewald)" = 220,
                                 "non-HDL-C" = 240)
  })
  
  pred_value_df <- reactive({switch(input$measure,
                                    "LDL-C (Martin-Hopkins)" = tibble(ldl_mh = ifelse(input$cholesterol %>% is.null, 100, input$cholesterol)),
                                    "LDL-C (Sampson)" = tibble(ldl_sampson = input$cholesterol),
                                    "LDL-C (Friedewald)" = tibble(ldl = input$cholesterol),
                                    "non-HDL-C" = tibble(non_hdl = input$cholesterol)
                                    )
  })
  
  output$prediction <- renderUI({
    HTML(paste0("At this level of ", lipid_name(), ", the expected apoB is <b>", pred_50(), " mg/dL</b>, ", "which is the 50th percentile of apoB at ", input$cholesterol, "mg/dL of ", lipid_name(), ". However, apoB values could plausibly range from <b>", pred_2.5(), " mg/dL to ", pred_97.5(), " mg/dL</b> (the 95% population distribution of apoB at this value of " , lipid_name(),")."))
  })
  
  
  # reactive({lipid_var})
  pred_2.5 <- reactive({qdo(model(), predict, qu = 0.025, newdata = pred_value_df()) %>% round(0) %>% as.numeric})
  pred_25 <- reactive({qdo(model(), predict, qu = 0.25, newdata = pred_value_df()) %>% round(0) %>% as.numeric})
  pred_50 <- reactive({qdo(model(), predict, qu = 0.5, newdata = pred_value_df()) %>% round(0) %>% as.numeric})
  pred_75 <- reactive({qdo(model(), predict, qu = 0.75, newdata = pred_value_df()) %>% round(0) %>% as.numeric})
  pred_97.5 <- reactive({qdo(model(), predict, qu = 0.975, newdata = pred_value_df()) %>% round(0) %>% as.numeric})
    
  
  output$table_description <- renderUI({
    HTML(paste0("<br><br>The following table shows the population distribution of apoB at ", input$cholesterol, " mg/dL of ", lipid_name(), "."))
  })
  
  
  table_df <- reactive(
    data.frame("Percentile" = c("2.5th percentile", "25th percentile", "50th percentile (median)", "75th percentile", "97.5th percentile"), 
           "ApoB value (mg/dL)" = c(pred_2.5(), pred_25(), pred_50(), pred_75(), pred_97.5()) %>% format(nsmall = 0))
  )
  
  
  #Create Table
  output$percentiles_table <- renderReactable({
    
    #Lets use options to style our reactable to match our dark theme.
    options(reactable.theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))
    
    reactable(
    #Build table
    tibble("Percentile" = c("2.5th percentile", "25th percentile", 
                            "50th percentile (median)", 
                            "75th percentile", "97.5th percentile"), 
           "ApoB value (mg/dL)" = c(pred_2.5(), pred_25(), 
                                    pred_50(), 
                                    pred_75(), pred_97.5()) %>%
             format(nsmall = 0)),
    defaultColDef = colDef(align = "center")
  )})
    
    # renderTable(table_df())
  
  
    #Function to draw figure
    fig_df <- reactive({
      
      #Set prediction sequence
      pred_seq <-  seq(0, 250, 1)
      #Create  dataframe on which to make predictions
      df_for_preds <- tibble(!!as.symbol(lipid_var()) := pred_seq)
      # df_for_preds <- tibble(ldl_mh := pred_seq)
      
      #Create dataframe to place predictions
      fig_df <- data.frame(qdo(model(), predict, qu = c(0.025, 0.25, 0.5, 0.75, 0.975), newdata = df_for_preds),
                           indep_var = pred_seq)
      # fig_df <- data.frame(qdo(qreg_list$ldl_mh, predict, qu = c(0.025, 0.25, 0.5, 0.75, 0.975), 
      #                      newdata = df_for_preds),
      #                      indep_var = pred_seq)
      #Rename
      fig_df <- fig_df %>% rename_all(., ~ c("p2.5", "p25", "p50", "p75", "p97.5", "indep_var"))
      #Return
      fig_df
      
      
    })
    output$plot <- renderPlot({
      #Set titles/breaks etc. accordingly
      xtitle <- paste0(input$measure, " (mg/dL)")
      xlimits <- if(lipid_var() == "non_hdl") { c(60, 240) } else { c(40, 220) }
      first_col_name <- ifelse(lipid_var() == "non_hdl", "non-HDL cholesterol", "low-density lipoprotein cholesterol")
      xbreaks <- if(lipid_var() == "non_hdl") { seq(60, 240, 20) } else { seq(40, 220, 20) }
      ytitle <- paste0("ApoB (mg/dL)")
      ylimits <- c(NA, NA)
      ybreaks <- seq(0, 1000, 20)
      #Line types
      linetypes <- c("solid" = "solid", "dashed" = "dashed", "dotted" = "dotted")
      
      #Set line_color and width
      line_color = "maroon"
      line_width = 1.5
      
      #Plot
      ggplot(data = fig_df(),
                      aes(x = indep_var,
                          y = p50)) +
        #Ribbon
        geom_ribbon(aes(ymin = p25, ymax = p75),
                    fill = "lightblue",
                    alpha = 0.4) +
        geom_ribbon(aes(ymin = p75, ymax = p97.5),
                    fill = "lightblue",
                    alpha = 0.2) +
        geom_ribbon(aes(ymin = p2.5, ymax = p25),
                    fill = "lightblue",
                    alpha = 0.2) +
        #Add line segments
        geom_segment(aes(x = ifelse(input$cholesterol %>% is.null, 100, input$cholesterol),
                         xend = ifelse(input$cholesterol %>% is.null, 100, input$cholesterol),
                         y = pred_2.5(),
                         yend = pred_97.5()),
                     size = 1.5) +
        #Add Lines
        geom_smooth(aes(y = p50,
                        x = indep_var,
                        linetype = "solid"),
                    color = line_color,
                    lwd = line_width) +
        geom_smooth(aes(y = p2.5,
                        x = indep_var,
                        linetype = "dotted"),
                    color = line_color,
                    lwd = line_width) +
        geom_smooth(aes(y = p97.5,
                        x = indep_var,
                        linetype = "dotted"),
                    color = line_color,
                    lwd = line_width) +
        geom_smooth(aes(y = p25,
                        x = indep_var,
                        linetype = "dashed"),
                    color = line_color,
                    lwd = line_width) +
        geom_smooth(aes(y = p75,
                        x = indep_var,
                        linetype = "dashed"),
                    color = line_color,
                    lwd = line_width) +
        #Add linetype legend
        scale_linetype_manual(values = linetypes,
                              breaks = c("solid", "dashed", "dotted"),
                              name = "",
                              labels = c("Median (50th percentile)",
                                         "25th and 75th percentiles",
                                         "2.5th and 97.5th percentiles")) +
        #Add scales
        scale_x_continuous(
          name = capitalize(xtitle),
          breaks = xbreaks,
          expand = c(0.0, 0.0)
        ) +
        scale_y_continuous(
          name = ytitle,
          breaks = ybreaks
        ) +
        theme_pubclean() +
        theme(text = element_text(size = 23),
              plot.title=element_text(face = "bold",hjust = 0.5, size = 16.5),
              axis.text.x = element_text(size = 20, face = "bold"),
              axis.text.y = element_text(size = 20, face = "bold"),
              axis.title.x = element_text(size = 20, face = "bold"),
              axis.title.x.top = element_text(size = 15, face = "bold"),
              axis.text.x.top = element_text(size = 15, face = "bold"),
              axis.title.y = element_text(size = 20, face = "bold"),
              axis.title.y.right = element_text(size = 15, face = "bold"),
              axis.text.y.right = element_text(size = 15, face = "bold"),
              axis.line = element_line(colour = "black", size = 1.2),
              panel.border = element_rect(colour = "black", fill=NA, size=1.2),
              plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
              legend.position = "bottom",
              legend.key.width = unit(2, "cm"),
              legend.key = element_rect(fill = "transparent"),
              legend.text = element_text(size = 10)
              ) +
        #Remove fill in legend
        guides(linetype = guide_legend(override.aes = list(fill = NA))) +
        #Add cartesian coords
        coord_cartesian(xlim = xlimits,
                        ylim = fig_df() %>% filter(indep_var %between% xlimits) %>% select(p50) %>% pull %>% range)
      
      

      
      
    
    }
    
    
    
    )
    # 
    # 
    

    
    output$caution <- renderUI({ 
      HTML(
      "<br>Please note that this describes the data for the <i>middle 95% (2.5th to 97.5th percentiles)</i> of the distribution. As such, a proportion of individuals (~2.5%) are expected to have apoB values exceeding this range while others (~2.5%) are expected to have apoB values below this range.<br>
      <br>Also note that these calculations are based on models trained on the National Health and Nutrition Examination Survey (NHANES), which is a nationally representative sample of US adults. Additionally, patients using statins or with a triglyceride level > 400 mg/dL were excluded. Users should exercise due caution when using the tool outside the settings in which it was developed."
      )
    })
    
    output$line_explanation <- renderUI({HTML(paste0("<br><br>The below graph describes the distribution of apoB values according to the value of ", lipid_name(), " The black solid line represents the corresponding 95% range of apoB values at the value you entered."))})
}

# Run the application 
shinyApp(ui = ui, server = server)
