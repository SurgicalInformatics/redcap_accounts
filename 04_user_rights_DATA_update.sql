UPDATE `redcap_user_rights` SET
`group_id`= (SELECT group_id
FROM  redcap_data_access_groups
  WHERE project_id = 17 
  AND group_name = '{redcap_data_access_group.team}')
WHERE (`username`='{username}' AND `project_id`=17)
;