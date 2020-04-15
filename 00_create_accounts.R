##########################################################################################
# This script calls the other relevant scripts in this folder to create new DAGs and users
# from the registration process.
##########################################################################################

# 01_duplicate_registrations.R runs as a cron job - 
# no need to source it here.

redcap_uri_api = 'https://redcap.xx.xx.xx/api/'

#############
# Step 1:   #
#############
# Run the 4 lines below to create the SQL and CSV files. ------------------
# Expected result: ALL GOOD
source("02_new_hospitals.R")
file_timestamp = format(Sys.Date(), "%Y%m%d")
source("03_new_dags.R")
source("04_new_users.R")

#############
# Step 2:   #
#############
# Files pane: go to the folder called batch_2018... (today's date)
# Step 2a: Copy the contents of        2018...dags.sql to phpmyadmin-globalsurg-SQL
# Step 2b: Copy the contents of 2018...user_rights.sql to phpmyadmin-globalsurg-SQL
# Reminder: sudo ufw allow 443, sudo ufw delete allow 443

#############
# Step 3:   #
#############
# Run the line below (04b...) to copy teams from the Registration project to the Authorship project 
# (this script won't work before the new DAGs have been created - Step 2a.)
source("05_update_redcap_records.R")
# Error: Bad Request means something's wrong with the redcap_data_access_group variable


#############
# Step 4:   #
#############
# Create new accounts via CSV upload
# Still in the batch_2018... folder:
# Step 4a: export the 2018...create_accounts.csv
# Step 4b: upload to REDCap (Control Centre - Add Users)

#############
# Step 5:   #
#############
# Commit
system("git add -A")
system("git commit -m 'batch'")
system("git push")

