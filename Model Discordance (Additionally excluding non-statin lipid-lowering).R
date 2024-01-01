#Get Table Values according to Dr. Navar's recommendations
table_values_ldl <- c(55, 70, 100, 130, 160, 190)
table_values_non_hdl <- c(85, 100, 130, 160, 190, 220)

##Declare variables to loop over
#Name of lipid variable in dataframe
lipid_vars <- c("ldl", "non_hdl", "ldl_mh", "ldl_sampson")
#Title
lipid_titles <- c("low-density lipoprotein cholesterol (using the Friedewald equation)", "non-HDL cholesterol", 
                  "low-density lipoprotein cholesterol (using the Martin-Hopkins equation)",
                  "low-density lipoprotein cholesterol (using the Sampson equation)")
#Abbreviation
lipid_caps <- c( "LDL-C", "non-HDL-C", "LDL-C", "LDL-C")
#Values to use in tables
tables_values <- list(table_values_ldl, table_values_non_hdl, table_values_ldl, table_values_ldl)


#Start loop
qreg_list_no_llt <- foreach(lipid_var = lipid_vars,
                     lipid_title = lipid_titles,
                     lipid_cap = lipid_caps,
                     table_values = tables_values,
                     .packages = packs) %do% {
                       
                       #Declare formula to be used for model (only varies in the lipid variable used for prediction)
                       reg_formula <- formula(paste("apo_b ~ s(", lipid_var, ")") )
                       
                       #Modify upper bound on error for Friedewald/Martin-Hopkins models (runs into convergence issues otherwise)
                       err <- if(lipid_var %in% c("ldl", "ldl_mh")) {0.03} else {NULL}
                       
                       
                       #Build models to predict ApoB by LDL
                       m <- mqgam(data = x_no_llt, #Data
                                  form = reg_formula, #Formula
                                  err = err, #Modify upper bound on error for Friedewald/Martin-Hopkins models (runs into convergence issues otherwise)
                                  multicore = TRUE, #Use several cores 
                                  ncores = detectCores() - 1, #Set N of cores to be used 
                                  qu = c(0.025, 0.25, 0.50,  0.75, 0.975), #Quantiles to predict
                                  argGam = list(weights = x_no_llt$weights/mean(x_no_llt$weights, na.rm = TRUE)) #Apply survey Weights
                       )
                       
                       #Set prediction sequence
                       pred_seq <-  seq(0, 250, 1)
                       
                       #Create  dataframe on which to make predictions
                       df_for_preds <- data.frame(pred_var = pred_seq) %>%
                         rename(!!as.symbol(lipid_var) := pred_var)
                       
                       #Create dataframe to place predictions
                       fig_df <- data.frame(qdo(m, predict, qu = c(0.025, 0.25, 0.5, 0.75, 0.975), newdata = df_for_preds),
                                            indep_var = pred_seq)
                       #Rename columns of this dataframe
                       fig_df <- fig_df %>% rename_all(., ~ c("p2.5", "p25", "p50", "p75", "p97.5", "indep_var"))
                       
                       
                       
                       #Table
                       table_df <- data.frame(
                         qdo(m, predict, qu = c(0.025, 0.25, 0.5, 0.75, 0.975), newdata = tibble(!!as.symbol(lipid_var) := table_values)),
                         indep_var = table_values)
                       table_df <- table_df %>% rename_all(., ~ c("p2.5", "p25", "p50", "p75", "p97.5", "indep_var"))
                       table_df <- table_df %>% round(0)
                       
                       #Graph it
                       source("Discordance Figures (Excluding additional non-statin lipid-lowering treatment).R", local = TRUE)
                       
                       #Return model
                       m
                     }

#Set names for qreg_list
names(qreg_list_no_llt) <- lipid_vars

