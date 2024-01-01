#Table of residuals by categorical variables
cat_resids_table <- bind_rows(
  cat_resids_list$gender[[1]] %>% mutate(var = "Gender"),
  cat_resids_list$race[[1]] %>% mutate(var = "Race/Ethnicity"), 
  cat_resids_list$obese[[1]] %>% mutate(var = "Obesity"), 
  cat_resids_list$diabetes[[1]] %>% mutate(var = "Diabetes"),
  cat_resids_list$met_health[[1]]  %>% mutate(var = "Metabolic health"), 
  cat_resids_list_statin[[1]][[1]]  %>% mutate(var =  "Statin use"),
) %>% 
  remove_rownames() %>%
  relocate(var)


#Rename headings and convert to flextable
fcat_table <- cat_resids_table %>%
  rename_all(., ~ c("Variable", "Category",
                "2.5th percentile of discordance (mg/dL)",
                "25th percentile of discordance (mg/dL)",
                "50th percentile of discordance (mg/dL)",
                "75th percentile of discordance (mg/dL)",
                "97.5th percentile of discordance (mg/dL)",
                "P-value")) %>%
  flextable

#Modify aesthetics
fcat_table <- flextable_aes(fcat_table, table_title = "Supplementary Table 3. Discordance according to categorical variables of interest.")

#Modify first column to bold/merge it
fcat_table <- fcat_table %>% 
  merge_v(j = 1) %>%
  bold(j = 1, part = "body")

#Merge P-values for the same variable
for(variable_index in 1:length(unique(cat_resids_table$var))) {
  fcat_table <- fcat_table %>%
    merge_at(j = length(cat_resids_table),
             i = which(cat_resids_table$var == cat_resids_table$var %>% unique %>% {.[variable_index]})
)
}

#Footnote
fcat_table <- fcat_table %>% footnote(j = length(cat_resids_table),
                        part = "header",
                        ref_symbols = footnote_symbol(1),
                        value = as_paragraph("P-values were obtained from linear regression models with discordance between predicted and actual apoB as the outcome variable and the categorical variable of interest as the predictor variable. This is equivalent to conducting a t-test for categorical variables composed of 2 categories and to using ANOVA for categorical variables composed of more than 2 categories."))
#Borders
fcat_table <- flextable::border(fcat_table, border = fp_border(color = "black"), part = "all")

#Width
fcat_table <- width(fcat_table, width = 0.98, unit = "in")
fcat_table <- width(fcat_table, j = length(cat_resids_table), width = 0.7, unit = "in")

#Save
fcat_table %>% save_as_docx(path = paste0(output_folder, "Supplementary Table 3.docx"))
