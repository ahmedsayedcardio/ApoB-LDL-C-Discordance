#Declare df_names (this is shorthand to refer to a certain NHANES file containing a data for a given set of variables)
df_names <- c("ldl_tg_measure", "tc_measure", 
              "apo_b", "hba1c", "fasting_glucose", "bp_measure", "bp_chol", 
              "hdl_measure", "BMI",
              "demo", "hc_util", "med_conds", 
              "dm_df", "meds")
##The NHANES data is stored in several folders, each containing a certain type of data (e.g., a folder for apoB, a folder for demographics, etc.)
#The "folders" variable constitute the folder names for each of the NHANES datasets. Either create folders that have identical names or change these names accordingly
folders <- c("LDL_TG_Measure", "TC_measure", 
             "ApoB", "Glycohemoglobin", "Fasting_Glucose", "BP_Measure", "BP_Chol", 
             "HDL_measure", "bmi",
             "Demographics", "HC_Utilization", "Medical_Conditions", 
             "Diabetes",  "Meds")


#Foreach loop to import the needed data
foreach(df_name = df_names,
        folder = folders,
        .packages = packs) %do% {
          
          #Declare df
          assign(df_name, data.frame())
          
          #State directory of files
          files <- dir(paste0(nhanes_folder, folder))
          
          #Import files
          foreach(file = files,
                  .packages = packs) %do% {
                    
                    #Read the file
                    data <- read_xpt(file = paste0(nhanes_folder, folder, "/", 
                                                   file))
                    #If meds, start command to join medications together
                    if(df_name == "meds") {
                      
                      #Join medications together
                      if("RXDDRUG" %in% colnames(data)) {
                        data <- data %>%
                          select("SEQN", "RXDDRUG") %>%
                          group_by(SEQN) %>%
                          summarise(drug_name = paste(RXDDRUG, collapse = "; "))
                        
                      } else if("RXD240B" %in% colnames(data)) {
                        
                        data <- data %>%
                          select("SEQN", "RXD240B") %>%
                          group_by(SEQN) %>%
                          summarise(drug_name = paste(RXD240B, collapse = "; "))
                      }
                    }
                    
                    #Assign data
                    assign(df_name, bind_rows(data, get(df_name)))
                    rm(data)
                    
                  }
          
          #Print dataframe
          get(df_name) %>% print()
        }

#Give a specific name to ApoB weights (stored in "ldl_tg_measure" for 2005-2006
#and in "apo_b" for other yeaars)
apo_b$WTSAF2YR.apo_b <- apo_b$WTSAF2YR
ldl_tg_measure$WTSAF2YR.apo_b_y1 <- ldl_tg_measure$WTSAF2YR

#Create a list containing all included dataframes
included_dfs <- foreach(df_name = df_names) %do% {
  get(df_name)
}

#Join the dataframes
x <- included_dfs %>% 
  reduce(full_join, by = "SEQN")

#Add ApoB where it's missing (For the 2005-2006 file, ApoB was in the LDL/TG file)
x$apo_b <- ifelse(is.na(x$LBXAPB.x), x$LBXAPB.y, ifelse(is.na(x$LBXAPB.y), x$LBXAPB.x, NA))


#Remove now-redundant dataframes
rm(list = df_names)

#Rename columns
x <- x %>% rename(any_of(c(
  "bmi" = "BMXBMI",
  "fasting_glucose" = "LBXGLU",
  "hba1c" = "LBXGH",
  "apo_b" = "LBXAPB",
  "ldl" = "LBDLDL",
  "ldl_mh" = "LBDLDLM",
  "ldl_nih" = "LBDLDLN",
  "tg" = "LBXTR",
  "hdl" = "LBDHDD",
  "tc" = "LBXTC",
  "race" = "RIDRETH1",
  "survey_cycle" = "SDDSRVYR",
  "age" = "RIDAGEYR",
  "gender" = "RIAGENDR",
  "salt_adv_take" = "MCQ371C",
  "chol_meds_adv" = "BPQ090D",
  "chol_meds_take" = "BPQ100D",
  "htn" = "BPQ020",
  "dyslipid" = "BPQ080",
  "dm" = "DIQ010",
  "pre_dm" = "DIQ160"
)))


#Correctly label gender
x$gender <- ifelse(x$gender == 1, "Men", ifelse(x$gender == 2, "Women", NA)) %>%
  factor


#Correctly label race/ethnicity
x <- x %>%
  mutate(across(any_of("race"), ~ case_when(. == 1 ~ "Hispanic",
                                            . == 2 ~ "Hispanic",
                                            . == 3 ~ "Non-Hispanic White",
                                            . == 4 ~ "Non-Hispanic Black",
                                            . == 5 ~ "Other") %>% factor)) 
#Relevel to make white individuals the reference
x$race <- x$race %>% fct_relevel("Non-Hispanic White")


#Correct labeling of yes/no diabetes variables (coded as 1 or 2 for Yes/No respectively and 7/9 for missings)
vars1279 <- c("pre_dm", "dm")
x <- x %>%
  mutate(across(any_of(vars1279), ~ case_when(. == 1 ~ "Yes",
                                              . == 2 ~ "No",
                                              . == 7 ~ NA_character_,
                                              . == 9 ~ NA_character_) %>%
                  factor))


#Correctly label obesity
x$obese <- ifelse(x$bmi >= 30, "Present", "Absent")
x$diabetes <- ifelse(x$hba1c >= 6.5 | x$fasting_glucose >= 126 | x$dm == "Yes", "Present", "Absent")



#Create "high triglycerides" variable
x$high_tg <- ifelse(x$tg >= 150, "Present", "Absent")

#Create metabolically well/unwell variable
x$met_health <- ifelse(x$tg < 150 & x$diabetes == "Absent" & x$bmi >= 18.5 & x$bmi <= 24.9, 
                       "Metabolically healthy",
                       "Metabolically unhealthy") %>% factor


#Build a string for Lipid-lowering drugs
lipid_lowering_string <- c("LOVASTATIN|PRAVASTATIN|FLUVASTATIN|ROSUVASTATIN|PITAVASTATIN|ATORVASTATIN|SIMVASTATIN|CERIVASTATIN|EVOLOCUMAB|ALIROCUMAB|EZETIMIBE|CHOLESTYRAMINE|COLESTIPOL|COLESEVELAM|NIACIN|BEZAFIBRATE|FENOFIBRATE|GEMFIBROZIL")

#Build a string for statins
statin_string <- c("LOVASTATIN|PRAVASTATIN|FLUVASTATIN|ROSUVASTATIN|PITAVASTATIN|ATORVASTATIN|SIMVASTATIN|CERIVASTATIN")


#Mark lipid_lowering as Yes if pt. takes any of these drugs
x <- x %>% 
  mutate(ldl_drug = case_when(
    str_detect(drug_name, lipid_lowering_string) ~ "Yes",
    str_detect(drug_name, lipid_lowering_string, negate = TRUE) ~ "No"
  ) %>% factor)

#Mark statin as Yes if pt. takes any of these drugs
x <- x %>% 
  mutate(statin = case_when(
    str_detect(drug_name, statin_string) ~ "Yes",
    str_detect(drug_name, statin_string, negate = TRUE) ~ "No"
  ) %>% factor)

#Create non-HDL variable
x$non_hdl <- x$tc - x$hdl

#Get sample size for adults with available apoB levels
# x <- x %>% filter(!is.na(apo_b))
apo_b_ss <- nrow(x %>% filter(!is.na(apo_b) & age >= 18))

#Get present LDL and non-HDL
ldl_present <- x %>% filter(!is.na(ldl) & age >= 18) %>% nrow
non_hdl_present <- x %>% filter(!is.na(non_hdl) & age >= 18) %>% nrow

#Create "total_weights" object (the denominator for the combined survey weights) which
#is equal to the number of different survey cycles
total_weight <- x[!is.na(x$apo_b), "survey_cycle"] %>% unique %>% pull %>% length

#New weights 
x <- x %>% mutate(weights = case_when(
  survey_cycle != 4 ~ WTSAF2YR.apo_b * (2/total_weight), #All but the 4th survey cycle have the ApoB weights stored in the WTSAF2yr.apo_b variable
  survey_cycle == 4 ~ x$WTSAF2YR.apo_b_y1 * (2/total_weight) #The 4th had it stored in a different variable (this was the result of NHANES storing it in a separate set of files)
))


#Create LDL calcualted 
source("Get MH, Sampson, and Friedewald Estimates.R")

#Filter out those with missing weights
x <- x %>% filter(!is.na(weights))

#Get the number of adults who were excluded due to high trigylcerides
x_tg_excluded <- x %>% filter(tg > 400 & age >= 18) %>% nrow

#Get the number of adults who were additionally excluded due to statins
x_statin_excluded <- x %>% filter(tg <= 400 & statin == "Yes" & age >= 18) %>% nrow


#Calculate sample sizes
adults_with_apo_b <- x %>% filter(age >= 18 & !is.na(apo_b)) %>% nrow
tg_high_or_missing <- x %>% filter(age >= 18 & !is.na(apo_b) & (tg > 400 | is.na(tg)) ) %>% nrow
statin_users <- x %>% filter(age >= 18 & !is.na(apo_b) & tg <= 400 & statin == "Yes") %>% nrow
final_ss <- x %>% filter(age >= 18 & !is.na(apo_b) & tg <= 400 & statin == "No") %>% nrow

#Make survey object out of x
sx <- svydesign(data = x,
                ids = x$SDMVPSU,
                strata = x$SDMVSTRA,
                weights = x$weights,
                nest = TRUE)

#Create a survey object that includes statin users (to be used for figure comparing discordance in statin vs non-statin users)
sx_statins <- subset(x = sx, subset = tg <= 400 & age >= 18)
x_statins <- x %>% filter(tg <= 400 & age >= 18)
#Create a survey object removing anyone using lipid-lowering therapy (not just statins) to be used for a sensitivity analysis
sx_no_llt <- subset(x = sx, subset = ldl_drug == "No" & tg <= 400 & age >= 18)
x_no_llt <- x %>% filter(ldl_drug == "No" & tg <= 400 & age >= 18)
#Finally, restrict the main analytic sample to adult non-statin users with a TG =< 400
sx <- subset(x = sx, subset = statin == "No" & tg <= 400 & age >= 18)
x <- x %>% filter(statin == "No" & tg <= 400 & age >= 18)

#Calculate N &  proportion of participants (weighted) in the "No LLT" analysis
n_no_llt <- nrow(x) - (nrow(x_no_llt))
prop_no_llt <- 1 - (sum(x_no_llt$weights) / sum(x$weights))

###We will now retrieve quantiles of certain variables to get a better idea of the distribution of our data
#Set the desired quantiles
quantiles_to_get <- c(0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99)

#Create function to get quantiles of a specified variable and convert to a dataframe
get_qs <- function(variable, desired_qs) {

#Get quantiles
qs <- svyquantile(design = sx, na.rm = TRUE, x = paste0("~ ", variable) %>% as.formula, quantiles = desired_qs)
  
#Create function to convert to dataframe
q_to_df <- function(x) {
  x[[1]] %>% data.frame %>% rownames_to_column() %>% rename(q = rowname, value = quantile)
}


#Convert to dataframe
qs_df <- q_to_df(qs)

#Round(1) to get around floating decimal points issue
qs_df$value <- qs_df$value %>% round(1)

assign(x = paste0(variable, "_qs"), value = qs_df, envir = globalenv())
}

#Get cumulative distribution for ApoB (useful to plot to get a sense of how apoB is distributed)
apo_b_cdf <- svycdf(design = sx, formula = ~ apo_b)
