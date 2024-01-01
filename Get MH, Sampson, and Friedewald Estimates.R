#Create empty x column
x$mh_factor <- NA

#Convert stata code to R (to create the script necessary to create the Martin Hopkins factor)
mh_factor_script <- readLines("statacode.do") %>%
  str_replace("if", "x$mh_factor[") %>% #Replace if
  {.[. != ""]} %>% #Remove empty lines
  {.[c(-1, -length(.) )]} %>% #Remove first and last line
  paste0("]") %>% #Put bracket at end of each line
  str_replace_all("nonhdl", "x$non_hdl") %>% #Replace nonhdl by appropriate symbol
  str_replace_all("tg", "x$tg") %>% #Replace tg by appropriate symbol
  str_remove_all("replace|factor=") %>% #Remove replace and factor= 
  paste("<-", word(., 2))  %>% #Relocate conversion factor where appropriate
  str_remove(word(., 2)) %>% #Remove the number at the beginning
  str_trim %>% #Remove unnecessary space at beginning
  str_replace("\\[ ", "\\[") #Remove unnecccessary space after "["

#Run script (which is now written in a format usable in R)
eval(parse(text = mh_factor_script))

#Calculate LDL using 3 different formulas
#Martin-Hopkins
x$ldl_mh <- x$tc - (x$hdl + (x$tg/x$mh_factor))
#Sampson
x$ldl_sampson <- (x$tc/0.948) - (x$hdl/0.971) - 
  ( (x$tg/8.56) + ( (x$tg * x$hdl)/2140 ) - ( (x$tg^2)/16100 ) ) -
  9.44
#Friedewald
x$ldl <- x$tc - (x$hdl + (x$tg/5))
