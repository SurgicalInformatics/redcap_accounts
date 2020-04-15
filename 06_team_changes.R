#################################################
# This script is used to add new members to
# existing teams.
# Once collaborators have access to REDCap
# The can use the Team Changes instrument in
# the Authorship project to request changes to their team.
#################################################

library(tidyverse)
library(RCurl)
library(stringr)
library(glue)

file_timestamp = format(Sys.Date(), "%Y%m%d")
same_day = FALSE

# pull Authorship forms including current team and changes ------------
authorship_current = postForm(
  uri=redcap_uri_api,
  token=Sys.getenv("gs3_authorship"),
  content='record',
  format='csv',
  type='flat',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='true',
  returnFormat='json') %>% 
  read_csv() %>% 
  filter(changes_complete %in% c(0, 1)) %>% # 0 Incomplete (urgent), 1 Unverified 
  select(record_id, info_team_ch, dag_authorship = redcap_data_access_group, team_id,
         orcid_1, email_1, firstname_1, lastname_1,
         orcid_2, email_2, firstname_2, lastname_2,
         orcid_3, email_3, firstname_3, lastname_3,
         contains("_ch"), -notes_ch, -internal_check, -overlap_check) %>% 
  mutate(dag_data = paste0(dag_authorship, "_", team_id)) %>% 
  gather(variable, value, -record_id, -dag_authorship, -team_id, -dag_data, -info_team_ch) %>% 
  na.omit() %>% 
  mutate(change = ifelse(str_detect(variable, "_ch"), "change", "original"))


# check that team members are being added not removed: ---------------------------
check_additions_only = authorship_current %>% 
  separate(variable, into = c("variable", "discard1", "discard2"), sep = "_") %>% 
  select(-discard1, -discard2) %>% 
  filter(variable == "orcid") %>% 
  arrange(record_id, change, variable)  %>% 
  group_by(record_id, change) %>% 
  summarise(orcids_combined_ordered = paste(value, collapse = "/")) %>% 
  spread(change, orcids_combined_ordered) %>% 
  mutate(all_included = str_detect(change, original)) %>% 
  filter(record_id != 1540) # known/expected removal

if(! all(check_additions_only$all_included)){
  stop("Team member being removed, ADMIN check required!")
}else{
  print("ALL GOOD: additions only, no removals.")
}


# if the removal is expected, just continue running the script from here to the bottom

sql_add_user_AUTHORSHIP = read_file("04_user_rights_AUTHORSHIP.sql")
sql_add_user_DATA = read_file("04_user_rights_DATA.sql")

user_rights_current = postForm(
  uri=redcap_uri_api,
  token=Sys.getenv("gs3_authorship"),
  content='user',
  format='csv',
  returnFormat='json') %>%
  read_csv() %>% 
  select(username, data_access_group)

# existing_users = authorship_current %>% 
#   filter(change == "change") %>% 
#   filter(variable %in% c("email_1_ch", "email_2_ch", "email_3_ch"))  %>% 
#   rename(email = value) %>% 
#   separate(email, into = c("username", "other_half"), remove = FALSE, sep = "@") %>% 
#   select(-other_half) %>% 
#   mutate(username = paste0(tolower(username), ".gs3"))

authorship_newusers = authorship_current %>% 
  filter(change == "change") %>%
  mutate(variable = str_remove(variable, "_ch")) %>% 
  separate(variable, into = c("variable", "member_id"), sep = "_") %>% 
  filter(value != "") %>% 
  spread(variable, value) %>% 
  separate(email, into = c("username", "other_half"), remove = FALSE, sep = "@") %>% 
  select(-other_half) %>% 
  mutate(username = paste0(tolower(username), ".gs3")) %>% 
  rename(redcap_data_access_group = dag_authorship, redcap_data_access_group.team = dag_data) %>% 
  mutate(sql.authorship = glue(sql_add_user_AUTHORSHIP)) %>% 
  mutate(sql.data = glue(sql_add_user_DATA))

add_users = authorship_newusers %>% 
  filter(username %in% c("fostino.gs3", "mvukipaul.gs3", "ebram.salama.gs3"))
  filter(! username %in% user_rights_current$username)

add_userrights_hospital_sql = add_users %>% 
  select(dag = redcap_data_access_group, sql = sql.authorship)

add_userrights_team_sql = add_users %>% 
  select(dag = redcap_data_access_group.team, sql = sql.data)

add_userrights_sql = bind_rows(add_userrights_hospital_sql, add_userrights_team_sql)

writeLines(add_userrights_sql$sql, paste0("team_changes/", file_timestamp, "_sql_user_rights.sql"))


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


write_csv(create_accounts, paste0("team_changes/", file_timestamp, "_create_accounts_new_members.csv"), na = "", append = same_day)


confirm_done = authorship_newusers %>% 
  distinct(record_id) %>% 
  mutate(changes_complete = 2)

write_csv(confirm_done, paste0("team_changes/", file_timestamp, "_confirm_done_authorship_upload.csv"), na = "", append = same_day)











