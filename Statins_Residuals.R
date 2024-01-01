#Build model to predict median ApoB by LDL
m50_statins <- qgam(data = x_statins,
            form = apo_b ~ s(ldl_mh), 
            qu = 0.5,
            argGam = list(weights = x_statins$weights/mean(x_statins %>% pull(weights), na.rm = T)))

x_statins$resids <- residuals(m50_statins, type = "response")
sx_statins$variables$resids <- x_statins$resids
#Set var strings
resids_reg_vars <- c("statin")
resids_reg_titles <- c("Statin use")

#Start loop
cat_resids_list_statin <- foreach(resids_reg_var = resids_reg_vars,
                           resids_reg_title = resids_reg_titles,
                           .packages = packs) %do% {
                             
                             
                             
                             #Declare type of variable
                             var_type <- "categorical"
                             
                             svyglm_formula <- paste0("resids ~ ", resids_reg_var) %>% formula
                             
                             #Calculate quantiles by group
                             strat_quantiles <- svyby(~ resids, paste0("~ ", resids_reg_var) %>% formula, design = sx_statins, svyquantile, na.rm = TRUE, 
                                                      quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
                             
                             #Use svyglm to get P-values for relation
                             pval <- svyglm(design = sx_statins, formula = svyglm_formula) %>% anova(., svyglm(design = sx_statins, formula = resids ~ 1)) %>% {.$p} %>% p
                             
                             #Create sequence for predictions
                             pred_seq <- x_statins[, resids_reg_var] %>% levels
                             
                             #Create  dataframe containing quantiles by group
                             resids_fig_df <- strat_quantiles %>% data.frame %>% dplyr::select(!contains("se.")) %>%
                               rename_all(funs(str_replace(., "resids.0.", "p"))) %>%
                               rename(p50 = p5,
                                      p2.5 = p025,
                                      p97.5 = p975,
                                      indep_var = !!as.symbol(resids_reg_var))
                             
                             #Produce figure
                             source("Statins Figure.R")
                             
                             #Get data for results paragraph (round to 0 decimal places to portray original accuracy of LDL-C/ApoB)
                             resids_fig_df <- resids_fig_df %>% mutate(across(where(is.numeric), ~ round(., 0)))
                             
                             #Bind Pval to save it to the list
                             list(cbind(resids_fig_df, pval), resids_fig)
                             
                           }
