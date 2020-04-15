#################################################
# This script generates the SQL INSERT statements
# that will be inserted using phpMyAdmin
# to create User Rights for the new users.
# We add User Rights before we create the User Accounts.
# The script also creates the CSV file to be used for
# "Create users (bulk upload)" in REDCap's Control Center
#################################################
# Yes, User Rights can be added before the user even exists -
# their username will show up in the User Rights section of
# the REDCap project. This allows us to double-check that
# everything worked as expected.
# And ensures the user has access to the project as soon
# as their account gets created.
#################################################
# After ensuring the scripts have worked alright and
# all new yet non-existing users have been assigned to a DAG
# - this is important as people without a DAG will be able to access
# all the data - 
# we will use the Create users (bulk upload) interface
# on REDCap to upload the CSV file this script created
#################################################


library(tidyverse)
library(RCurl)
library(stringr)
library(glue)
library(jsonlite)

# load new teams as created in 03_new_dags.R ---------
load(paste0(file_timestamp, "_new_teams.rda"))

# Can add temporary exclusions here - e.g. for duplicates not identified for some reason
new_teams = new_teams %>% 
  filter(record_id.registration != 385) # duplicate used fake ORCID IDs originally

#file_timestamp = format(Sys.Date(), "%Y%m%d")

same_day = FALSE

# load full registration info -------
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
  select(record_id.registration = record_id,
         info_takepart___1,info_protocol___1,info_followup___1,info_approvals___1, info_team,
         orcid_1, email_1 = email_1_reg, firstname_1 = firstname_1_reg, lastname_1 = lastname_1_reg,
         orcid_2, email_2 = email_2_reg, firstname_2 = firstname_2_reg, lastname_2 = lastname_2_reg,
         orcid_3, email_3 = email_3_reg, firstname_3 = firstname_3_reg, lastname_3 = lastname_3_reg,
         registration_notes = notes,
         moved_authorship)

all_usernames = registrations_original %>% 
  filter(moved_authorship == 1) %>% 
  select(record_id = record_id.registration, contains("email")) %>% 
  gather(collaborator, email, -record_id) %>% 
  na.omit() %>% 
  separate(email, into = c("username", "other_half"), remove = FALSE, sep = "@") %>% 
  select(-other_half) %>% 
  mutate(username = paste0(tolower(username), ".gs3"))

# Reformat new registrations to be uploaded to the Authorship project ------------------
upload_authorship_project = new_teams %>% 
  left_join(registrations_original, by = "record_id.registration") %>% 
  select(record_id = record_id.registration,
         redcap_data_access_group = dag.hospital,
         hospital_code = hospital.code,
         everything(), -iso2, -n_dag15, -dag.team, -moved_authorship) %>% 
  mutate(registration_readonly_complete = 2) %>% 
  mutate(info_team_ch = info_team,
         orcid_1_ch = orcid_1, email_1_ch = email_1, firstname_1_ch = firstname_1, lastname_1_ch = lastname_1,
         orcid_2_ch = orcid_2, email_2_ch = email_2, firstname_2_ch = firstname_2, lastname_2_ch = lastname_2,
         orcid_3_ch = orcid_3, email_3_ch = email_3, firstname_3_ch = firstname_3, lastname_3_ch = lastname_3,
         changes_complete = 2)

upload_authorship_project %>% 
  write_csv(paste0("upload_authorship/", file_timestamp, "_redcap_upload_authorship.csv"), na = "")

upload_registration_project = upload_authorship_project %>% 
  select(record_id) %>% 
  mutate(moved_authorship = 1, admin_complete = 2)

upload_registration_project %>% 
  write_csv(paste0("upload_authorship/", file_timestamp, "_redcap_upload_registration.csv"), na = "")



# Create User Rights (create SQL) ------------

sql_add_user_AUTHORSHIP = read_file("04_user_rights_AUTHORSHIP.sql")
sql_add_user_DATA = read_file("04_user_rights_DATA.sql")

add_users = upload_authorship_project %>% 
  select(redcap_data_access_group,
         team_id,
         contains("email"), contains("orcid"), contains("firstname"), contains("lastname"),
         -contains("_ch")) %>% 
  gather(variable, value, -redcap_data_access_group, -team_id) %>% 
  separate(variable, into = c("variable", "member_id"), sep = "_") %>% 
  filter(value != "") %>% 
  spread(variable, value) %>% 
  separate(email, into = c("username", "other_half"), remove = FALSE, sep = "@") %>% 
  select(-other_half) %>% 
  mutate(username = paste0(tolower(username), ".gs3")) %>% 
  #left_join(dag_ids_hospitals) %>% 
  mutate(sql.authorship = glue(sql_add_user_AUTHORSHIP)) %>% 
  mutate(redcap_data_access_group.team = paste(redcap_data_access_group, team_id, sep = "_")) %>%
  #left_join(dag_ids_teams) %>% 
  mutate(sql.data = glue(sql_add_user_DATA))

add_users$email    %>%  duplicated() %>% mean()
add_users$email[add_users$email    %>%  duplicated()]


add_users$username %>%  duplicated() %>% mean()

existing_usernames = all_usernames %>% 
  filter(! record_id %in% new_teams$record_id.registration)

username_in_use = inner_join(existing_usernames, add_users, by = c("username"))

# Test 1: ---------------
if (nrow(username_in_use) == 0){
  print("Test 1: ALL GOOD, all usernames are new.")
} else{
  stop("Test 1: Username in use, look in the username_in_use tibble.
       Investigate the record_id in GlobalSurg 3 | Registration, set record to Hold off and ask someone to investigate further.")
}


# Test 2: ---------------
if ((add_users$username %>%  duplicated() %>% mean()) == 0){
  print("Test 2: ALL GOOD, no duplicate usernames within the new batch.")
} else{
  stop("Test 2: Duplicate username in the new batch (probably someone using the same email for different members.
       A quick work-around is to filter it out by record_id at the very top of 04_new_users.R")
}

# this is already done but slightly differently above - using moved=1 from Registration project. 
# Reason for using a different one above is sometimes a record gets moved and then moved back but the username remains
# (not possible to remove a username that has not been created yet...but is possible to add that username to User Rights)
data_userrights = postForm(uri=redcap_uri_api,
                           token=Sys.getenv("gs3_data"),
                           content='user',
                           format='csv',
                           returnFormat='json') %>% 
  read_csv()

add_userrights_hospital_sql = add_users %>% 
  filter(! username %in% data_userrights$username) %>% 
  select(dag = redcap_data_access_group, sql = sql.authorship)

add_userrights_team_sql = add_users %>% 
  filter(! username %in% data_userrights$username) %>% 
  select(dag = redcap_data_access_group.team, sql = sql.data)

sql_update_user_AUTHORSHIP = read_file("04_user_rights_AUTHORSHIP_update.sql")
sql_update_user_DATA       = read_file("04_user_rights_DATA_update.sql")

update_userrights_hospital_sql.existing = add_users %>% 
  filter(username %in% data_userrights$username) %>% 
  select(-sql.authorship, -sql.data)   %>% 
  mutate(sql = glue(sql_update_user_AUTHORSHIP)) %>% 
  select(dag = redcap_data_access_group, sql)

update_userrights_team_sql.existing = add_users %>% 
  filter(username %in% data_userrights$username) %>% 
  select(-sql.authorship, -sql.data) %>% 
  mutate(sql = glue(sql_update_user_DATA)) %>% 
  select(dag = redcap_data_access_group.team, sql)

# Test 3: ---------------
if (nrow(update_userrights_team_sql.existing) == 0){
  print("Test 3: ALL GOOD, further check for uniqueness and duplicates passed.")
} else{
  stop("Test 3: Username reassigned, admin check required.")
}

add_userrights_sql  = suppressWarnings(bind_rows(add_userrights_hospital_sql, add_userrights_team_sql,
                                                 update_userrights_hospital_sql.existing,
                                                 update_userrights_team_sql.existing))

writeLines(add_userrights_sql$sql, paste0("new_users/", file_timestamp, "_sql_user_rights.sql"))

# Create CSV to adding users ------------------

create_accounts = add_users %>% 
  mutate(`Sponsor username` = NA, Expiration = NA) %>% 
  select(Username = username,
         `First name` = firstname, `Last Name` = lastname,
         `Email address` = email,
         `Institution ID` = redcap_data_access_group,
         `Sponsor username`,
         Expiration,
         Comments = orcid) %>% 
  mutate(Comments = paste("ORCID ID:", Comments))


write_csv(create_accounts, paste0("new_users/", file_timestamp, "_create_accounts.csv"), na = "", append = same_day)

# copy all files to one convenient folder ----------
batch_folder = paste0("batch_", file_timestamp)
system(paste0("mkdir ", batch_folder))
system(paste0("cp ", "new_dags/",  file_timestamp, "_sql_create_dags.sql ", batch_folder))
system(paste0("cp ", "new_users/", file_timestamp, "_sql_user_rights.sql ", batch_folder))
system(paste0("cp ", "new_users/", file_timestamp, "_create_accounts.csv ", batch_folder))

