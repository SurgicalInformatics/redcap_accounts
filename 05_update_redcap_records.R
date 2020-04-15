# run this after submitting the DAGs SQL
# this needs to have the same Environment as 04_new_users.R so rerun that one if in a new session

# update Registration -----
RCurl::postForm(
  uri=redcap_uri_api,
  token=Sys.getenv("gs3_registration"),
  content='record',
  format='json',
  type='flat',
  overwriteBehavior='normal',
  forceAutoNumber='false',
  data=toJSON(upload_registration_project),
  returnContent='count',
  returnFormat='csv'
)

# update Authorship -----
RCurl::postForm(uri=redcap_uri_api,
  token=Sys.getenv("gs3_authorship"),
  content='record',
  format='json',
  type='flat',
  overwriteBehavior='normal',
  forceAutoNumber='false',
  data=toJSON(upload_authorship_project),
  returnContent='count',
  returnFormat='csv')
