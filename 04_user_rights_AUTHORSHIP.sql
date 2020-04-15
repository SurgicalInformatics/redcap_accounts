INSERT INTO redcap_user_rights (
  project_id
 ,username
 ,role_id
 ,group_id
) SELECT
  25
 ,'{username}'
 ,7
 ,group_id
FROM  redcap_data_access_groups
  WHERE project_id = 25 
  AND group_name = '{redcap_data_access_group}'
;