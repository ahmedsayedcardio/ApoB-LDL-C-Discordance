#Build model to predict median ApoB by Martin-Hopkins LDL (this will serve as the basis on which to calculate discordance)
m50 <- qgam(data = x, #Data
            form = apo_b ~ s(ldl_mh), #Regression formula
            qu = 0.5, #50th percentile (median)
            argGam = list(weights = x$weights/mean(x$weights, na.rm = TRUE))) #Survey weights

#Create a column for residuals in x and sx
x$resids <- residuals(m50, type = "response")
sx$variables$resids <- x$resids
#Describe the residuals in terms of quantiles
resids_qs <- get_qs("resids", quantiles_to_get)


# Continuous  -------------------------------------------------------------


#Set var strings
resids_reg_vars <- c("age", "tg", "hba1c", "bmi")
resids_reg_titles <- c("age", "triglycerides", "HbA1c", "body mass index")
units <- c("years", "mg/dL", "%", paste0("kg/m\U00B2"))
intervals <- c(1, 1, 0.05, 0.1)

#Start loop
cont_resids_list <- foreach(resids_reg_var = resids_reg_vars,
                            resids_reg_title = resids_reg_titles,
                            unit = units,
                            interval = intervals,
                            .packages = packs) %do% {

              #Declare type of variable
              var_type <- "continuous"

              #Declare formula
              reg_resids_formula <- paste0("resids ~ s(", resids_reg_var, ")") %>% formula
              svyglm_formula <- paste0("resids ~ ", resids_reg_var) %>% formula

              #Modify the upper bound on error of the estimated quantile curve for HbA1c (runs into convergence issues otherwise)
              err <- if(resids_reg_var == "hba1c") {0.03} else {NULL}

              #Build models to predict variable by discordance
              m <- mqgam(data = x %>% filter(!is.na(!!as.symbol(resids_reg_var))), #Data
                         form = reg_resids_formula,  #Formula
                         err = err, #Lower  error rate slightly if Hba1c to avoid convergence issues
                         multicore = TRUE, #Use several cores
                         ncores = detectCores() - 1, #Set N of cores to be used
                         qu = c(0.025, 0.25, 0.50,  0.75, 0.975), #Desired percentiles
                         argGam = list(weights = x$weights[!is.na(x[, resids_reg_var])]/mean(x$weights[!is.na(x[, resids_reg_var])])) #Apply survey weights
              )

              #Use svyglm to get P-values for relation
              pval <- svyglm(design = sx, formula = svyglm_formula) %>% tidy %>% filter(term == resids_reg_var) %>% pull(p.value) %>% p

              #Get predictor variable quantiles
              var_qs <- get_qs(resids_reg_var, desired_qs = c(0.05, 0.95))

              #Get 5th and 95th percentile of predictor
              pred_range <-  var_qs$value %>% range

              #Create sequence for predictions
              pred_seq <- seq(pred_range[1], pred_range[2], interval)

              #Create  dataframe on which to make predictions
              df_for_preds <- data.frame(pred_var = pred_seq) %>%
                rename(!!as.symbol(resids_reg_var) := pred_var)

              #Create dataframe to place predictions
              resids_fig_df <- data.frame(qdo(m, predict, qu = c(0.025, 0.25, 0.5, 0.75, 0.975), newdata = df_for_preds),
                                          indep_var = pred_seq)
              resids_fig_df <- resids_fig_df %>% rename_all(., ~ c("p2.5", "p25", "p50", "p75", "p97.5", "indep_var"))

              #Produce figure
              source("Residuals Figure.R")

              #Store it (in the list created by the foreach loop)
              list(pval, resids_fig)
                            }

#Name list
names(cont_resids_list) <- resids_reg_vars

#Plot 2 by 2 figure
ggarrange(cont_resids_list$age[[2]], cont_resids_list$tg[[2]],
          cont_resids_list$bmi[[2]], cont_resids_list$hba1c[[2]],
          labels = c("A", "B", "C", "D") %>% paste0("[", ., "]"),
          ncol = 2, nrow = 2,
          align = "v",
          legend = "bottom",
          common.legend = FALSE,
          font.label = list(size = 12)) %>% print
grid_2by2()
cont_residuals_fig <- grid.grab()
ggsave(cont_residuals_fig,
       filename = paste0(output_folder, "Continuous Residuals.pdf"),
       width = 16,
       height = 9,
       dpi = 600)


# Categorical ------------------------------------------------------------------

#Set var strings
resids_reg_vars <- c("gender", "race", "obese", "diabetes", "met_health")
resids_reg_titles <- c("Gender", "Race", "Obesity", "Diabetes", "Metabolic health")

#Start loop
cat_resids_list <- foreach(resids_reg_var = resids_reg_vars,
                           resids_reg_title = resids_reg_titles,
                           .packages = packs) %do% {
                             
                             
                             
                             #Declare type of variable
                             var_type <- "categorical"
                             
                             svyglm_formula <- paste0("resids ~ ", resids_reg_var) %>% formula
                             
                             #Calculate quantiles by group
                             strat_quantiles <- svyby(~ resids, paste0("~ ", resids_reg_var) %>% formula, design = sx, svyquantile, na.rm = TRUE, 
                                                      quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
                             
                             #Use svyglm to get P-values for relation
                             pval <- svyglm(design = sx, formula = svyglm_formula) %>% anova(., svyglm(design = sx, formula = resids ~ 1)) %>% {.$p} %>% p
                             
                             #Create sequence for predictions
                             pred_seq <- x[, resids_reg_var] %>% levels
                             
                             #Create  dataframe containing quantiles by group
                             resids_fig_df <- strat_quantiles %>% data.frame %>% dplyr::select(!contains("se.")) %>%
                               rename_all(funs(str_replace(., "resids.0.", "p"))) %>%
                               rename(p50 = p5,
                                      p2.5 = p025,
                                      p97.5 = p975,
                                      indep_var = !!as.symbol(resids_reg_var))
                             
                             #Produce figure
                             source("Residuals Figure.R")
                             
                             #Get data for results paragraph (round to 0 decimal places to portray original accuracy of LDL-C/ApoB)
                             resids_fig_df <- resids_fig_df %>% mutate(across(where(is.numeric), ~ round(., 0)))
                             
                             #Create a list containing the data, p-values, and figure
                             list(cbind(resids_fig_df, pval), resids_fig)
                             
                           }

#Assign variable names to the list
names(cat_resids_list) <- resids_reg_vars

#Plot a figure for statins (called separately since statin users are kept in a separate dataframe)
source("Statins_Residuals.R")

ggarrange(cat_resids_list$gender[[2]], cat_resids_list$race[[2]], 
          cat_resids_list$obese[[2]], cat_resids_list$diabetes[[2]],
          cat_resids_list$met_health[[2]], statins_resids_fig,
          labels = c("A", "B", "C", "D", "E", "F") %>% paste0("[", ., "]"),
          ncol = 2, nrow = 3,
          align = "v",
          font.label = list(size = 10)) %>% print
grid_3by2()
cat_residuals_fig <- grid.grab()
ggsave(cat_residuals_fig,
       filename = paste0(output_folder, "Categorical Residuals.pdf"),
       width = 16,
       height = 9,
       dpi = 600)






