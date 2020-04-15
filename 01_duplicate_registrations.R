###########################################
# This script runs as a cron job to check and mark duplicate registrations
# based on ORCIDs, it uploads the fact that they've been identified as a duplicate
# back into REDCap so study administrators can look-up and 
# communicate with collaborators.
###########################################
# Definition of duplicate: the 3-person team includes one or more members
# who have already registered as part of another team.
# As each person can only be in a single DAG, this is an issue.
# Registration forms marked as duplicates do not get processed until
# the duplicate member is removed.
###########################################


library(tidyverse)
library(RCurl)
library(jsonlite)

file_timestamp = format(Sys.time(), "%Y%m%d-%H:%M")
# since cron runs as root, this needs a full path/can't be my environment variable:
token_secret_path = "/home/user/redcap_accounts/token.secret.registration"

# Pull all Complete registrations -------------
result <- postForm(
  uri=redcap_uri_api,
  token=as.character(source(token_secret_path))[1],
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
registrations_original = read.csv(textConnection(result), stringsAsFactors = FALSE) %>% 
  filter(globalsurg_3_data_entry_accounts_complete == 2)

already_marked = registrations_original %>% 
  select(record_id.marked = record_id, already_marked = duplicated) %>% 
  filter(! is.na(already_marked))

# Gather the ORCID IDs from 3 columns to one (to check for duplication)
registrations_gathered = registrations_original %>% 
  select(record_id, starts_with("orcid_"), -orcid_field_note) %>% 
  gather(variable, orcid_id, -record_id) %>% 
  filter(orcid_id != "") %>% 
  select(-variable) %>% 
  arrange(record_id)

# Select distinct registrations
unique_registrations = registrations_gathered %>% 
  distinct(orcid_id, .keep_all = TRUE)
# Select duplicate registrations
duplicated_registrations = registrations_gathered %>% 
  filter(duplicated(orcid_id)) %>% 
  mutate(duplicated = 1)

# Check that the two dataframe above add up to all registrations
# Reasons they might not: collaborators ignored our big red warnings and submitted duplicates within the same form
if ((nrow(unique_registrations) + nrow(duplicated_registrations)) != nrow(registrations_gathered)){
  stop("Number of rows in unique_registrations and duplicated_registrations do not add up to registrations_gathered!")
}

# This pipeline creates the variable duplicate_records which groups duplicate record IDs together
# as well as sets duplicated = 1
mark_duplicate_records = duplicated_registrations %>% 
  left_join(registrations_gathered, by = "orcid_id", suffix = c(".duplicate", ".matched")) %>% 
  group_by(record_id.duplicate) %>% 
  distinct(record_id.matched, .keep_all = TRUE) %>% 
  summarise(duplicate_records = paste(record_id.matched, collapse = ",")) %>% 
  ungroup() %>% 
  mutate(duplicated = 1) %>% 
  rename(record_id = record_id.duplicate) %>% 
  left_join(already_marked, by = c("record_id" = "record_id.marked"))

# This takes the list of record_ids with distinct ORCID IDs and marks them duplicated=0 (default is 9 - Not checked)
mark_unique_records = registrations_original %>% 
  select(record_id) %>% 
  filter(! record_id %in% mark_duplicate_records$record_id) %>% 
  mutate(duplicated = 0) %>% 
  left_join(already_marked, by = c("record_id" = "record_id.marked"))

if ((nrow(mark_duplicate_records) + nrow(mark_unique_records)) != nrow(registrations_original)){
  stop("Number of rows in unique and duplicated subsets do not add up!")
} else{
  print("Duplicate registrations (based on ORCID IDs) sucessfully identified.")
}

# createtables for updating data on REDCap:
mark_duplicate_records.newbatch = mark_duplicate_records %>% 
  filter(is.na(already_marked)) %>% 
  select(record_id, duplicate_records, duplicated)

mark_unique_records.newbatch = mark_unique_records %>% 
  filter(is.na(already_marked)) %>% 
  select(record_id, duplicated) %>% 
  mutate(moved_authorship = 0)

redcap_upload = bind_rows(mark_duplicate_records.newbatch, mark_unique_records.newbatch)
if (nrow(redcap_upload)>0){
postForm(
  uri=redcap_uri_api,
  # since cron runs as root, this needs a full path/can't be my environment variable:
  token=as.character(source(token_secret_path))[1],
  content='record',
  format='json',
  type='flat',
  overwriteBehavior='normal',
  forceAutoNumber='false',
  data=toJSON(redcap_upload),
  returnContent='count',
  returnFormat='csv'
)
}

# Before setting up the API update, wrote out the CSV for a few times
# to double-check everything works:
#write_csv(redcap_upload, paste0("/home/user/redcap_accounts/check_duplicates/", file_timestamp, "_redcap_upload.csv"), na = "")


