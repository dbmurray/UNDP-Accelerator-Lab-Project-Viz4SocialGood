# This script imports in the data supplied for the UNDP Accelerator Labs Network project for the Viz 4 Social Good imitative.
# The data is modified and 'tidied' into a more usuable format for visualisation in other platforms (e.g Tableau, PowerBI)
# Output data is structured in so it can be easily linked together in a normalised form.
# More information on this porject is here: https://www.vizforsocialgood.com/join-a-project/2022/9/15/undp-accelerator-labs-network
# 
# Author: Darragh Murray (@dbfmurray on twitter if you have questions)

# PACKAGES (if needed)

install.packages("tidyverse")
install.packages("realxl")

# LIBRARIES

library(dplyr) # for general data preparation
library(readxl) # for importing excel documents
library(stringr) # for string manipulation
library(lubridate) # for date manipulation
library(tidyr)
library(openxlsx) # for output of the data
library(purrr) 
library(countrycode) # for nice country names


# IMPORT DATA

# IMPORT MAIN DATASET
# import the date using read_excel. We can skip 'contribution date' as it seems to be a copy of the 
# nicer formatted 'new_date'. We'll convert this into a proper date field below.

undp_source_data <- read_excel("input data/Viz4SocialGood_submissionV1.xlsx", 
                               col_types = c("skip", "text", "numeric", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text"))


# SDG GOAL DESCRIPTION
# import a custom-made data set with SDG goal numbers and descriptions.
sdg_goals <- read_excel("input data/sdg_goals.xlsx")


# IMPORT UNDP OFFICE DATA
# First we get the sheet names from the file and we loop throwugh these sheets to read in the data. Then we prep them :)
undp_sheet_names = excel_sheets("input data/Viz4SocialGood_submissionV1.xlsx")

# import data
import_undp_excel_sheets = lapply(setNames(undp_sheet_names, undp_sheet_names), 
                    function(x) read_excel("input data/Viz4SocialGood_submissionV1.xlsx", sheet=x))

# We now union the sheets together using bind_rows - 
# then use the handy pivot function to get the data into the right column structure. 
undp_offices = bind_rows(import_undp_excel_sheets$RB_Africa, 
                         import_undp_excel_sheets$RB_Asia_Pacific, 
                         import_undp_excel_sheets$`RB_ArabStates`, 
                         import_undp_excel_sheets$RB_Europe_CIS, 
                         import_undp_excel_sheets$`RB_Latin America`, 
                        .id="undp_sheet_names") %>%
  pivot_longer(!c(Country, undp_sheet_names), values_to = "undp_office", values_drop_na = TRUE) %>%
  
  # select only the columns we need and rename to lower case (as I'm a stickler for variable name consistency)
  
  select(Country, undp_office) %>% 
  rename(original_country_field = Country) %>%
  
  # some of the country records actually contain multuple countries - so I'm going to recode them then add the other countries as rows.
  mutate(original_country_field = recode(original_country_field, 
                                         "Samoa (& Cook Islands, Niue, Tokelau)" = "Samoa",
                                         "Trinidad & Tobago (Guyana & Suriname)" = "Trinidad & Tobago", 
                                         "Mauritius (& Seychelles)" = "Mauritius"))
  
  # We need to add in those countries we recoded out of our office data set. We do this by createing a new data frame and then
  # row-binding it back to the original offices datasetp
  
new_countries = data.frame(original_country_field = c("Seychelles", "Cook Islands","Niue","Tokelau","Guyana","Suriname"), 
                   undp_office = c("RBA", "RBAP","RBAP","RBAP","RBLAC","RBLAC"))
  
# let's bind the above countries into our offices dataset

undp_offices <- undp_offices %>%
  rbind(undp_offices, new_countries) %>% # add rows to the end of the data
  
  # the last step in cleaning our offices is to fix up some dodgy names. so we'll leverage
  # the awesomeness of the countrycode() package to 'guess' the correct names and put them in a new variable
  
  mutate(country = countrycode(original_country_field, origin= "country.name", destination = "country.name")) %>%
  select(undp_offices, country)


# CORE DATA PREPARATION
undp_source_data <- undp_source_data %>%
  
  # RENAMING THE ID FIELD
  # I want something a bit more descriptive than just _id_, so I'm going to rename the field in the source data frame to 'project_id'.
  # This will be used to join other related data.
  
  rename(project_id = id)

# TIDY PROJECT DATA
# Now let's organise a centralised and tidy data table describing each UNDP project. We'll create a new data frame from the main
# improted data source.

undp_project_data <- undp_source_data %>%
  
  # 5. RENAMING COLUMNS/VARIABLES
  # There are a large amount of columns with names that are a bit difficult to deal with/read/inconsistent, 
  # so we're going to rename them en-masse. I'm a stickler for nice variable names!
  # note - not all columns have been renamed - this is because we're going to split out some data into different tables. 
  
  rename(contribution_date = new_date, 
         latitude = Latitude,
         longitude = Longitude,
         energy_source = `Energy source`,
         clean_cooking_flag = `Clean cooking application (yes)`,
         project_title = title,
         accelerator_lab = Mapper,
         contributor = `Contributor anonymized`,
         solution_description = `What is the purpose of the solution? (brief problem & solution description)`,
         solution_url = `Please insert a link to the solution`,
         unit_cost = `What is the unit cost of this Solution along with any additional cost for maintenance and training?`,
         open_source_flag = `This solution is Do it Yourself / open source`,
         trl_status = `What is the Technological Readiness Level (TRL) of this solution?`,
         ip_flag = `This solution is protected by Intellectual Property`,
         training_flag = `The solution holder is able to train others (including end-users) in using or replicating the solution`,
         prototype_flag = `This solution is a Prototype`,
         product_flag = `This solution is a Product`,
         in_market_flag = `If this solution is a product, is it available in market?`,
         advance_order_flag = `If this solution is a product, does advance order has to be given?`,
         solution_delivery_status = `How much has this solution already been diffused? Is there potential feedback from end-users available?`,
         end_user_feedback_link = `Please upload a link of end-user feedback`,
         efficiency_outcomes = `Are there any efficiency benchmarks for this solution (eg. how much energy does it save; how much cheaper does it produce energy than current market rates/ current household expenditure / cost per kW h)?`,
         potential_challanges = `Are there any other potential bottlenecks affecting cross-border or in country diffusion of this solution?`) %>%
  
  # FIXING UP THE DATES 
  # Next we are going to deal with the "contribution_date" field - we want nice proper dates. We can do this using
  # str_extract from stringr and regex. The awesome-ness of lubridate() will take care of converting date
  # in string format to dates in date format - we just supply the date pattern (month,day, year = mdy)
  # mutate is the function that creates a new column in the dataset
  
  mutate(contribution_date = mdy(str_extract(contribution_date, regex("\\w+\\s+\\d{1,2}\\s+\\d{4}")))) %>%
  
  # CONSISTENT PROJECT TITLES
  # I've noticed that some of the project titles are in random case - some in sentence
  # case, some in title case, some in upper case. 
  # I'd like nicer looking project titles - all in sentence case. Stringr to the rescue (again). 
  
  mutate(project_title = str_to_sentence(project_title, locale = "en")) %>%
  
  # CREATE A COUNTRY VARIABLE
  # The datasource uses a field called 'mapper' to indicate country, but it does have a string in front 'AccLab'. 
  # I'm just going to create a new field called country based off this field - just because its a bit easier. 
  # The code below splits the Mapper field by the words and discards the first word
  
  mutate(country = word(accelerator_lab, 2, str_count(accelerator_lab, '\\s')+1)) %>%

  # looks like the accelerator_lab variable had some strings that don't translate to countries nicely, so we'll manually do them
  mutate(country = if_else(country == "Bee Network (India)", "India", if_else(country == "Pacific", "Fiji", country))) %>%
  
  # now I want to make sure all the country names are consistent. We could do this in the line above
  # but I'll seperate out just to demo
  
  mutate(country = countrycode(country, origin= "country.name", destination = "country.name")) %>%
  
  
  # CREATE PROPER FLAG VARIABLES
  # There are a bunch of variables in the dataset that should be boolean flag with various values set to TRUE and FALSE.
  # We can do this easily mutate() and across() - which iterates the same if_then_else rule across all inputted columns
  
  mutate(across(c(clean_cooking_flag, open_source_flag, ip_flag, training_flag, prototype_flag, product_flag, in_market_flag, advance_order_flag),
                ~ (if_else(.x== "x", TRUE, FALSE, missing = FALSE)), .names = '{col}')) %>%
  
  # RETAIN THE REQUIRED DATA
  # I'm going to split out the the SDG and thematic data into their own dataframe/table, so I don't need these specific fields in the main
  # dimension table so I can remove them
  
  select(-c(`What Sustainable Development Goal is this Solution addressing? Tag 1`,
            `What Sustainable Development Goal is this Solution addressing? Tag 2`,
            `What Sustainable Development Goal is this Solution addressing? Tag 3`,
            `What Sustainable Development Goal is this Solution addressing? Tag 4`,
            `What Sustainable Development Goal is this Solution addressing? Tag 5`,
            `What thematic tags apply to this solution? Tag 1`,
            `What thematic tags apply to this solution? Tag 2`,
            `What thematic tags apply to this solution? Tag 3`,
            `What thematic tags apply to this solution? Tag 4`,
            `What thematic tags apply to this solution? Tag 5`,
            image_1,
            image_2,
            image_3,
            image_4,
            image_5,
            image_6,
            image_7,
            image_8,
            image_9))

# PUT SDG IN TIDY FORMAT
# You'll note that the Sustainable Development Goals data is spread across five columns.
# This is unnecessary - and we can transpose this data into tidy columns using pivot_longer.
# We then fill out the table using the sdg_goals data we imported earlier.


undp_project_sdg <- undp_source_data %>%
  
  # SELECT REQUIRED COLUMNS
  # We don't need everything! Just the SDG data and the project_id
  select(project_id, 
         `What Sustainable Development Goal is this Solution addressing? Tag 1`,
         `What Sustainable Development Goal is this Solution addressing? Tag 2`,
         `What Sustainable Development Goal is this Solution addressing? Tag 3`,
         `What Sustainable Development Goal is this Solution addressing? Tag 4`,
         `What Sustainable Development Goal is this Solution addressing? Tag 5`) %>%
  
  # PIVOT DATA INTO TIDY FORMAT
  # He we de-pivot all columns in the the data (except project_id) from a wide data frame to a narrow one. We only need
  # the values here, hence no 'names' parameter. We also want to get rid of all null rows.
  
  pivot_longer(!project_id, values_to = "sdg_goal", values_drop_na = TRUE) %>%
  
  # JOIN IN SDG DESCRIPTIONS AND THEN SELECT APPROPRIATELY
  # This is where we join in that SDG data. R is smart enough to join on a common field
  # Then we select only the fields required for output
  
  inner_join(sdg_goals) %>%
  select(project_id, sdg_goal, sdg_goal_description)

# PUT THEMES IN TIDY FORMAT
# This is basically the same as above but without joining in SDG data.
# One difference is that we correct the themes values to be in Title Case

undp_project_themes <- undp_source_data %>%
  select(project_id, 
         `What thematic tags apply to this solution? Tag 1`,
         `What thematic tags apply to this solution? Tag 2`,
         `What thematic tags apply to this solution? Tag 3`,
         `What thematic tags apply to this solution? Tag 4`,
         `What thematic tags apply to this solution? Tag 5`) %>%
  pivot_longer(!project_id, values_to = "project_themes", values_drop_na = TRUE) %>%
  mutate(project_themes = str_to_title(project_themes, locale = "en")) %>%
  select(project_id, project_themes)

# PUT IMAGE DATA IN TIDY FORMAT
# This is basically the same as above but without joining in SDG data.
# One difference is that we correct the themes values to be in Title Case

undp_project_images <- undp_source_data %>%
  select(project_id, 
         image_1,
         image_2,
         image_3,
         image_4,
         image_5,
         image_6,
         image_7,
         image_8,
         image_9) %>%
  pivot_longer(!project_id, values_to = "image_number", values_drop_na = TRUE) %>%
  select(project_id, image_number)

# Output tidy tables as worksheets.
list_of_datasets <- list("UNDP Project Data" = undp_project_data, 
                         "UNDP Project SDGs" = undp_project_sdg, 
                         "UNDP Project Themes" = undp_project_themes,
                         "UNDP Project Images"= undp_project_images,
                         "UNDP Offices" = undp_offices)

# write worksheet names to an excel workbook
write.xlsx(list_of_datasets, file = "output data/UNDP_Tidy_Data.xlsx")



