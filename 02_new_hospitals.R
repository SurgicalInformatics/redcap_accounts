#################################################
# Our REDCap registration form lists all these hospitals:
# https://github.com/SurgicalInformatics/world_hospitals
# They are kept in a separate REDCap project 
# and displayed using a Dynamic SQL pull
##################################################
# If a collaborator elects: "My hospital is not listed"
# and types in their hospital name,
# an administrator will double check that
# 1. whether this really is a new hospital, 
# or if the collaborator just couldn't find it from the list
# (since it's an international project, it's not always obvious if/how
# a hospital name is translated into English
# 2. If it is a new hospital, the administrator will check that the
# name is spelled correctly by finding it on Google maps and checking its website.
# This curation is assisted by a Shiny app and it also involves
# making up a DAG code in the form of
# ISO2_city_hospname, with a maximum of 15 characters including the _s
#################################################

library(tidyverse)
library(RCurl)
library(jsonlite)

countries_official_list = read_csv("countries_gs_wb_lookup_06032019.csv")
new_batch = TRUE # change this to FALSE when editing hospitals that are already uploaded
# (otherwise record IDs increase every time the script is run)


file_timestamp = format(Sys.Date(), "%Y%m%d")

# Pull all Complete registrations with "Enter new hospital" and no duplicated ORCID IDs: -------------
# hospital = 1 is "Enter new hospital"
# duplicated = 0 means that these ORCID IDs have not been entered before
result <- postForm(
  uri=redcap_uri_api,
  token=Sys.getenv("gs3_registration"),
  content='record',
  format='csv',
  type='flat',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json'
)
new_hospitals_original = read.csv(textConnection(result), stringsAsFactors = FALSE) %>% 
  filter(globalsurg_3_data_entry_accounts_complete == 2 & hospital == 1 & duplicated == 0) %>% 
  select(record_id, contains("new"), contains("curated"))
rm(result)

# Not checked yet
###########################
# for new records, status will default to 0 (Not checked), but records entered before I changed it on 30/30/2018
# will have it as NA
not_checked = new_hospitals_original %>% 
  filter(is.na(hospital_new_status) | hospital_new_status != 1)

# Checked and not a new hospital
###############################
# new_confirm=0 - "Hospital exists, but collaborator didn't find it", we have selected it at hospital_new_curated (although not really new)
# hospital_new_status: 0, Not checked | 1, Checked | 2, Collaborator contacted
already_existed = new_hospitals_original %>% 
  filter(!is.na(new_confirm) & (new_confirm == 0 & hospital_new_status == 1))

# Actually a new hospital
#############################
new_hospitals = new_hospitals_original %>% 
  filter(new_confirm == 1 & hospital_new_status == 1 & is.na(hospital_new_curated)) %>% 
  left_join(countries_official_list, by = c("country_curated" = "record_id")) %>% 
  select(record_id.registration = record_id, country, iso2, city = city_curated, gs3_name = hospital_curated, gs3_dag = dag_new) %>% 
  mutate(include = 1)

new_hospitals_previously_added = new_hospitals_original %>% 
  filter(new_confirm == 1 & hospital_new_status == 1 & ! is.na(hospital_new_curated)) %>% 
  left_join(countries_official_list, by = c("country_curated" = "record_id")) %>% 
  select(record_id.registration = record_id, country, iso2, city = city_curated, gs3_name = hospital_curated, gs3_dag = dag_new) %>% 
  mutate(include = 1)


# Check that filters add up, throw an Error if not
if ((nrow(not_checked) + nrow(already_existed) + nrow(new_hospitals) + nrow(new_hospitals_previously_added)) != nrow(new_hospitals_original)){
  stop("Number of rows in the 3 dataframes (not checked, checked and already existed, checked and confirmed new) do not add up!")
} else{
  print("ALL GOOD")
}

if (new_batch){
# Pull all current hospitals
result <- postForm(
  uri=redcap_uri_api,
  token=Sys.getenv("gs3_hospitals"),
  content='record',
  format='csv',
  type='flat',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json'
)

hospitals_offical_list = read.csv(textConnection(result), stringsAsFactors = FALSE) %>% 
  filter(country != ".") # dummy record for "Enter New Hospital"

# figure out what the last record_id in each country currently is (and then add new hospitals after that)
last_hospital = hospitals_offical_list %>% 
  group_by(country) %>% 
  filter(record_id == max(record_id)) %>% 
  select(last_hospital = record_id, country)

new_hospitals_numbers = new_hospitals %>% 
  left_join(last_hospital) %>% 
  arrange(last_hospital) %>% 
  mutate(dummy = 1) %>% 
  group_by(country) %>% 
  mutate(add_record = cumsum(dummy)) %>% 
  mutate(record_id = last_hospital+add_record) %>% 
  ungroup() %>% 
  select(-last_hospital, -dummy, -add_record) %>% 
  select(record_id.hospital = record_id, everything()) %>% 
  mutate(hospital_name_complete = 2)

new_hospitals_numbers %>% 
  select(record_id = record_id.hospital, everything(), -record_id.registration) %>% 
  write_csv(paste0("new_hospitals/", file_timestamp, "_redcap_upload_new_hospitals.csv"), na = "")

new_batch = FALSE
}

 
# update hospital look-up
new_hospitals_numbers.hospitals = new_hospitals_numbers %>% 
  select(record_id = record_id.hospital, everything(), -record_id.registration)

# update Registration form ------------
new_hospitals_numbers.registration = new_hospitals_numbers %>% 
  ungroup() %>% 
  select(record_id = record_id.registration, hospital_new_curated = record_id.hospital)


# The curation is done in a Shiny app used by administrators
# This code checks that they've finished the process
# And finalises if they haven't:
if (nrow(new_hospitals_numbers) == 0){
  rm(list = ls())
  print("ALL GOOD: New hospital curation completed.")
} else{
  added.hospitals <- postForm(
    uri=redcap_uri_api,
    token=Sys.getenv("gs3_hospitals"),
    content='record',
    format='json',
    type='flat',
    overwriteBehavior='normal',
    forceAutoNumber='false',
    data=toJSON(new_hospitals_numbers.hospitals),
    returnContent='count',
    returnFormat='csv'
  )
  
  updated.registration = postForm(
    uri=redcap_uri_api,
    token=Sys.getenv("gs3_registration"),
    content='record',
    format='json',
    type='flat',
    overwriteBehavior='normal',
    forceAutoNumber='false',
    data=toJSON(new_hospitals_numbers.registration),
    returnContent='count',
    returnFormat='csv'
  )
  
  print("ALL GOOD: New hospital curation now completed.")
  rm(list = ls())
}









