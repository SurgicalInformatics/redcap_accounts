INSERT INTO redcap_user_rights (
  project_id
 ,username
 ,role_id
 ,group_id
) SELECT
  25
 ,'new_username.gs3'
 ,7
 ,group_id
FROM  redcap_data_access_groups
  WHERE project_id = 25 
  AND group_name = 'it_naple_istpas'
;
INSERT INTO redcap_user_rights (
  project_id
 ,username
 ,role_id
 ,group_id
) SELECT
  17
 ,'new_username.gs3'
 ,4
 ,group_id
FROM  redcap_data_access_groups
  WHERE project_id = 17 
  AND group_name = 'it_naple_istpas_03'
;