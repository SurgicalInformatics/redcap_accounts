#################################################
# This script generates the SQL INSERT statements
# that will be inserted using phpMyAdmin
# to create the new DAGS.
#################################################
# At the moment, it is not possible to create new DAGs
# using the API, or by uploading a CSV file.
# The only two ways to create new DAGs is to:
# 1. Insert them by hand/on-by-one in the REDCap project
# 2. Use SQL INSERT in the back-end/phpMyAdmin
#################################################

library(tidyverse)
library(RCurl)
library(glue)

# read in SQL insert statement glue templates:
# note the project_ids (17/25) are hardcoded
sql_add_dag_AUTHORSHIP = read_file("03_add_dag_sql_glue_AUTHORSHIP.sql")
sql_add_dag_DATA       = read_file("03_add_dag_sql_glue_DATA.sql")


dags_teams = postForm(
  uri=redcap_uri_api,
  token=Sys.getenv("gs3_data"),
  content='user',
  format='csv',
  returnFormat='json') %>% 
  read_csv() %>% 
  distinct(data_access_group) %>% 
  rename(redcap_data_access_group.team = data_access_group)

dags_hospitals = postForm(
  uri=redcap_uri_api,
  token=Sys.getenv("gs3_authorship"),
  content='user',
  format='csv',
  returnFormat='json') %>% 
  read_csv() %>% 
  distinct(data_access_group) %>% 
  rename(redcap_data_access_group = data_access_group)

# Pull all Complete registrations ------------
# Have to check for NAs or otherwise the filter won't work as expected
# This could be fixed by setting defaults (probably in 01_duplicate_registrations.R)
registrations_original = postForm(
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
  returnFormat='json') %>% 
  read_csv() %>% 
  filter(globalsurg_3_data_entry_accounts_complete == 2) %>% 
  filter((! moved_authorship %in% c(1, 2)) | is.na(moved_authorship)) %>% # 1 - Moved, 2 - Moved but later removed
  filter(pending != 1 | is.na(pending))


# Subset registrations that have been checked (not duplicates, and if new hospital curated and added to the lokk-up project) ---------
# Have to check for NAs or otherwise the filter won't work as expected
# This could be fixed by setting defaults (probably in 01_duplicate_registrations.R)
registrations_checked = registrations_original %>% 
  filter(duplicated == 0) %>% 
  filter(hospital != 1 | (hospital_new_status == 1 & ! is.na(hospital_new_curated))) %>% 
  rename(record_id.registration = record_id)



# TEST 1 ------
# test that if hospital == 1 (add new), a new hospital has been added
# and at the same time, that if a hospital was already selected, we haven't accidentally changed it (existing)
registrations_checked.new = registrations_checked %>% 
  select(hospital, hospital_new_curated) %>% 
  filter(hospital == 1 & ! is.na(hospital_new_curated))

registrations_checked.existing = registrations_checked %>% 
  select(hospital, hospital_new_curated) %>% 
  filter(hospital != 1 & is.na(hospital_new_curated))

if ((nrow(registrations_checked.new) + nrow(registrations_checked.existing)) != nrow(registrations_checked)){
  stop("Number of rows in the 3 dataframes (checked new, checked existed) do not add up!")
} else{
  print("Test 1: ALL GOOD - existing, checked (new), and not checked (new) registrations add up")
  rm(registrations_checked.new, registrations_checked.existing)
}



# get hospital info (country and DAG) from the Hospitals | Country lists and DAGs project -----------
hospitals_offical_list = postForm(uri=redcap_uri_api,
  token=Sys.getenv("gs3_hospitals"),
  content='record',
  format='csv',
  type='flat',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='true',
  exportDataAccessGroups='false',
  returnFormat='json') %>% 
  read_csv(na = "") %>% # don't want "NA" as NA because it's Namibia. this line doesn't work as expected but I can't reproduce the issue
  mutate(iso2 = ifelse(country == "Namibia", "NA", iso2)) %>% #temporary fix until I figure out why the read_csv(na = "") doesn't work as expected
  filter(country != "." & city != ".")  %>% # dummy record for "Enter New Hospital"
  select(hospital.code = record_id, country, iso2, city, hospital = gs3_name, gs3_dag)


# Pull all teams currently moved to Authorship to continue team IDs from there  ------------
# neither using record_ids or complete dates is not good enough as some people start the registration (which is when the record_id gets assigned)
# but complete it, for example, 4 days later.
# Or communication pending (e.g. us checking the hospital name with the collaborator)
# is another reason for team IDs not to follow 
current_authorship = postForm(
  uri=redcap_uri_api,
  token=Sys.getenv("gs3_authorship"),
  content='record',
  format='csv',
  type='flat',
  'fields[0]'='record_id',
  'fields[2]'='team_id',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='true',
  returnFormat='json') %>% 
  read_csv() %>% 
  mutate(dag.existing = paste(redcap_data_access_group, team_id, sep = "_")) %>% 
  rename(record_id.authorship = record_id, team_id.authorship = team_id, dag.hospital = redcap_data_access_group)

current_last_team = current_authorship %>% 
  group_by(dag.hospital) %>% 
  filter(dag.existing == max(dag.existing)) %>% 
  rename(team_id.existing = team_id.authorship)

#current_authorship$record_id.authorship[na.omit(current_authorship$record_id.authorship) %in% na.omit(registrations_checked$record_id.registration)]
# Test 2: if any of the "new" registrations record IDs are already in authorship
if (any(na.omit(current_authorship$record_id.authorship) %in% na.omit(registrations_checked$record_id.registration))){
  stop("A 'new' registration ID is already in authorship. Check that it has been marked 'Moved' in the Registration project.")
} else{
  print("Test 2: ALL GOOD - no duplication in new Registrations record IDs and current Authorship record IDs")
}

# team_id.dummy is 0 for the first new time
new_teams = registrations_checked %>% 
  select(hospital, hospital_new_curated, record_id.registration) %>% 
  mutate(hospital.code = ifelse(hospital == 1, hospital_new_curated, hospital)) %>% 
  select(hospital.code, record_id.registration) %>% 
  left_join(hospitals_offical_list, by = "hospital.code") %>% 
  mutate(dag.hospital = paste0(tolower(iso2), "_", gs3_dag)) %>% 
  select(-gs3_dag) %>% 
  mutate(n_dag15 = nchar(dag.hospital)) %>% 
  arrange(record_id.registration) %>% 
  left_join(current_last_team, by = "dag.hospital") %>% 
  mutate(team_id.numeric = ifelse(is.na(team_id.existing), 0, as.numeric(team_id.existing))) %>% 
  mutate(dummy = 1) %>% 
  group_by(dag.hospital) %>% 
  mutate(team_id.newtoday = cumsum(dummy)) %>% 
  mutate(team_id = (team_id.numeric + team_id.newtoday) %>% formatC(width=2, flag="0")) %>% 
  mutate(dag.team = paste(dag.hospital, team_id, sep = "_")) %>% 
  ungroup() %>% 
  select(-dummy, -team_id.newtoday, -team_id.existing, -team_id.numeric, -record_id.authorship, -dag.existing)

# Test 3: check that DAG length (max 15!) ------------
dags_too_long = new_teams %>% 
  filter(n_dag15 > 15)
# check that the number of distinct DAGs (dag.hospital) matches the number of distinct hospitals
# i.e. that there is a DAG for each hospital
distinct_dag_hospitals      = new_teams %>% distinct(hospital.code, dag.hospital)
distinct_hospitals          = new_teams %>% distinct(hospital.code)
if (nrow(dags_too_long) != 0){
  stop("Found a DAG name longer than 15 characters, check dags_too_long!")
} else if(nrow(distinct_dag_hospitals) != nrow(distinct_hospitals)){
  stop("Duplicate DAG codes, check distinct_dag_hospitals")
}else{
  print("Test 3: ALL GOOD - DAG codes less than 15 characters, no duplicate DAG codes for different hospitals found")
  rm(dags_too_long, distinct_dag_hospitals, distinct_hospitals)
}




dags_sql = new_teams %>% 
  mutate(sql.hospital  = glue(sql_add_dag_AUTHORSHIP)) %>% 
  mutate(sql.team      = glue(sql_add_dag_DATA))

add_hospital_sql = dags_sql %>% 
  distinct(dag.hospital, sql.hospital) %>% 
  filter(! dag.hospital %in% dags_hospitals$redcap_data_access_group) %>% 
  select(dag = dag.hospital, sql = sql.hospital)

add_team_sql = dags_sql %>% 
  arrange(team_id) %>% 
  distinct(dag.team, sql.team) %>% 
  #mutate(already_exist = dag.team %in% dag_ids_teams$redcap_data_access_group.team) %>% 
  filter(! dag.team %in% dags_teams$redcap_data_access_group.team) %>% 
  select(dag = dag.team, sql = sql.team)

add_dags_sql =  suppressWarnings(bind_rows(add_hospital_sql, add_team_sql))

# I've left an example file in the new_dags folder
writeLines(add_dags_sql$sql, paste0("new_dags/", file_timestamp, "_sql_create_dags.sql"))

new_teams %>% 
  filter(! dag.team %in% dags_teams$redcap_data_access_group.team) %>% 
  write_csv(paste0("new_dags/", file_timestamp, "_new_teams.csv"))

save(new_teams, file = paste0(file_timestamp, "_new_teams.rda"))




