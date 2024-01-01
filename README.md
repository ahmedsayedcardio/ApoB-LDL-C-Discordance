# ApoB-LDL-C-Discordance
This is a deposit of the code used for a paper assessing apoB-LDL-C discordance in the United States.

The following is an elaboration on the structure of this deposit and how it can be used to replicate the analysis:

1. "Master File.R" will call all of the necessary analytical files in the correct order. These include:
    1. Loading the packages and functions needed to run the analysis ("Libraries.R").
    2. Importing NHANES data ("Import.R"). To download the data, please visit: https://wwwn.cdc.gov/nchs/nhanes/.
        1. This will call the "Get MH, Sampson, and Friedewald Estimates.R" file to create an LDL-C variable estimated using the 3 methods. 
    4. Running the analysis to obtain apoB-LDL discordance across the spectrum of lipid variables ("Model Discordance.R").
        1. This will additionally call the "Discordance Figures.R" file to create the related figure.
        2. This will additionally call the "Statin_Residuals.R" file to conduct a separate analysis comparing statin users versus no users and the "Statins Figure.R" file to create the related figure.
    5. Obtaining discordance per subgroup ("Model residuals.R").
        1. This will additionally call the "Residuals Figure.R" file to create the related figure. 
    7. Creating a table describing baseline characteristics ("Supp Table 2 (baseline characteristics).R") and residuals by categorical variables in a table format ("Supp Table 3 (Categorical Residuals Table).R").
    8. Conducting a sensitivity analysis excluding additional (non-statin) lipid-lowering treatment ("Model Discordance (Additionally excluding non-statin lipid-lowering).R").
        1. This will call the "Discordance Figures (Excluding additional non-statin lipid-lowering treatment).R" to create the related figure.


2. The data and code needed for the app ("ApoB-LDL_Discordance ShinyApp" Folder):
    1. This includes the stored models ("Calculator pre-requisities.RData").
    2. This includes the code for the Shinyapp ("app.R").
