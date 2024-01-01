#Select baseline variables for S_table 2
outcome_vars <- c("age", "gender", "race", 
                  "bmi", "obese", "hba1c", 
                  "diabetes", "high_tg",
                  "met_health",
                  "tc", "tg", "hdl",
                  "ldl_mh", "ldl_sampson", "ldl", "non_hdl",
                  "apo_b")
outcome_titles <- c("Age (years)", "Gender (%)", "Race/Ethnicity (%)",
                   "Body mass index (kg/m2)", "Obesity (%)", "HbA1c (%)",
                   "Diabetes (%)", "Elevated triglycerides (%)",
                   "Metabolic health (%)",
                   "Total cholesterol (mg/dL)", "Trigylcerides (mg/dL)",
                   "High density lipoprotein cholesterol (mg/dL)", 
                   "Low-density lipoprotein cholesterol using the Martin-Hopkins equation (mg/dL)",
                   "Low-density lipoprotein cholesterol using the Sampson equation (mg/dL)",
                   "Low-density lipoprotein cholesterol using the Friedewald equation (mg/dL)",
                   "Non-High density lipoprotein cholesterol (mg/dL)",
                   "Apolipoprotein B (mg/dL)")
rm(baseline_df)
outcome_var = outcome_vars[9]
outcome_title = outcome_titles[9]
if(!exists("baseline_df")) {
  baseline_df <- foreach(outcome_var = outcome_vars,
                    outcome_title = outcome_titles,
                    .packages = packs,
                    .combine = "rbind") %do% {
                      options(survey.lonely.psu = "certainty")
                      
                      
                      #Get the median/IQR of the variable of choice
                      if(!(outcome_title %>% str_detect("%") & outcome_var %>% str_detect("hba1c", negate = TRUE)) %>% all) {
                      median_var <- svyquantile(~ get(outcome_var), design = sx, na.rm = TRUE,
                                              quantiles = 0.5) %>% .[[1]] %>% .[1] %>% round(1) %>% format(nsmall = 1) %>% data.frame
                      iqr_var <- c(
                        (svyquantile(~ get(outcome_var), design = sx, na.rm = TRUE,
                                                quantiles = 0.75) %>% .[[1]] %>% .[1]) ,
                        (svyquantile(~ get(outcome_var), design = sx, na.rm = TRUE,
                                    quantiles = 0.25) %>% .[[1]] %>% .[1])
                      ) %>% round(1) %>% format(nsmall = 1)
                      
                      avg_var <- paste0(median_var, " (", iqr_var[2], " to ", iqr_var[1], ")") %>% data.frame
                      } else {#Get the mean of the variable of choice
                        avg_var <- svymean(~ get(outcome_var), design = sx, na.rm = TRUE) %>% pct_table %>% data.frame
                        
                      }
                      
                      #Remove se column
                      avg_var <- avg_var %>% select(1)
                      
                      #Add variable name column
                      avg_var <- avg_var %>% mutate(Variable = outcome_title)
                      
                      #Add extra header if categorical variable
                      if((outcome_title %>% str_detect("%") & outcome_var %>% str_detect("hba1c", negate = TRUE)) %>% all) {avg_var <- avg_var %>% add_row(Variable = outcome_title,
                                                                                              .before = 1)}
                      
                      #Return it
                      colnames(avg_var) <- c("avg_var", "Variable")
                      avg_var
                    }
}


#Convert rownames to additional column
baseline_df <- baseline_df %>% 
  tibble::rownames_to_column() %>% 
  rename("Status" = "rowname")


#Remove the (get(baseline) part)
baseline_df$Status <- str_remove_all(string = baseline_df$Status, pattern = "get\\(outcome_var\\)")
baseline_df$Status <- baseline_df$Status %>% str_remove("[0-9]")

baseline_df$Variable <- ifelse(!is.na(baseline_df$avg_var) & baseline_df$Variable %>% str_detect("%") & baseline_df$Variable %>% str_detect("HbA1c", negate = TRUE),
                               baseline_df$Status, baseline_df$Variable)

#Replace NA
baseline_df$avg_var <- baseline_df$avg_var %>% str_replace("NA", "")


#Insert indentation column
baseline_df$indent <- ifelse(baseline_df$Variable %>% str_detect("Men|Women|Hispanic|Other|Absent|Present|Metabolically"),
                   TRUE, 
                   FALSE)


#Indent relevant rows
baseline_df$Variable <- ifelse(baseline_df$indent == TRUE,
                       paste0("      ", baseline_df$Variable), 
                       baseline_df$Variable)


#Insert headers column
baseline_df$head <- ifelse(baseline_df$Variable %>% str_detect("\\("),
                             TRUE, 
                             FALSE)


#Flextable
baseline_table <- flextable(baseline_df %>% 
                              rename(Value = avg_var) %>%
                              dplyr::select(-head, -indent, -Status) %>% relocate("Variable"))

#Add borders
baseline_table <- flextable::border(baseline_table, i = baseline_df$head,
                             border.bottom = fp_border(color = "black"))
baseline_table <- flextable::border(baseline_table, i = c(baseline_df$head[-1], TRUE),
                             border.bottom = fp_border(color = "black"))
#Bold text
baseline_table <- bold(baseline_table, j = 1, i = baseline_df$head, bold = TRUE)
baseline_table <- bold(baseline_table, part = "header", bold = TRUE)
#Add padding
baseline_table <- padding(baseline_table, i = baseline_df$indent, j = 1, padding.left = 15)
#Add caption
baseline_table <- set_caption(baseline_table,
                       caption = "Supplementary Table 2. Baseline Characteristics of Included Respondents.")
#Add footnote
baseline_table <- footnote(baseline_table,
                           j = 2,
                           part = "header",
                           ref_symbol = footnote_symbol(1),
                           value = as_paragraph("For categorical variables, percentages are presented. For continuous variables, the median and interquartile range are presented."))
baseline_table <- footnote(baseline_table,
                           j = 1,
                           i = which(baseline_df$Variable == "Metabolic health (%)"),
                           ref_symbol = footnote_symbol(2),
                           value = as_paragraph("A metabolically healthy profile was defined as a BMI between 18.5 and 24.9 kg/m2, triglycerides less than 150 mg/dL, and no diabetes."))



#Set width
baseline_table <- width(baseline_table, j = 1, width = 5, unit = "in")
baseline_table <- width(baseline_table, j = 2, width = 1, unit = "in")
#Set font
baseline_table <- flextable::font(baseline_table, fontname = "Times New Roman", part = "all")
#Set alignment
baseline_table <- flextable::align(baseline_table, j = 2, align = "center", part = "all")
baseline_table <- flextable::align(baseline_table, j = 1, align = "center", part = "header")
#Set border
baseline_table <- flextable::border(baseline_table, border = fp_border(color = "black"), part = "all")
#Save
baseline_table %>% save_as_docx(path = paste0(output_folder, "Supplementary Table 2.docx"))

