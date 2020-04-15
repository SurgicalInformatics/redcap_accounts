# Creating REDCap accounts programmatically

We use a combination of REDCap, R, Shiny, and SQL to manage registrations and data entry for our international collaborative projects.

These scripts in this repository were used to create collaborator accounts for GloballSurg 3: Quality and outcomes after global cancer surgery: a prospective, international cohort study (https://globalsurg.org/gs3).

The number of countries, hospitals, collaborators (mostly surgeons), and patients can be viewed here: http://data.globalsurg.org/numbers/

![](https://argonaut.is.ed.ac.uk/public/gs3_data_map.png)


# Instructions

After making sure each of the numbered R scripts work as expected, we use `00_create_accounts.R` to source and action them in order.

Instructions are included in `00_create_accounts.R` as well as at the top of each script.

# System
We use RStudio Server on a Ubuntu virtual machine to run these scripts. Furthermore, `01_duplicate_registrations.R` is set-up as a cron job on the same system.
We've not used `here::here` in these scripts so the file paths do not work cross-platform. The (very few) file paths involving directories will have to be changed from `/` to `\` if running on a Windows computer.
