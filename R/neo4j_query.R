# Requirements
#sudo apt-get install libcurl4-gnutls-dev # for RCurl on linux
#install.packages('RCurl')
#install.packages('RJSONIO')
#install.packages("glue")
#install.packages("config")

library('RCurl')
library('RJSONIO')

footr_setup = function() {
  ip = Sys.getenv("FOOTR_NEO_IP")
  if (ip == "") {
    stop('Need to setup FOOTR_NEO_IP in .Renviron before using Footr! If set, try restarting R or checking name.')
  }
  
  password = Sys.getenv("FOOTR_NEO_PASSWORD")
  if (password == "") {
    stop('Need to setup FOOTR_NEO_PASSWORD in .Renviron before using Footr! If set, try restarting R or checking name.')
  }
  
  username = 'neo4j'
  
  return(list(ip=ip, password=password, username=username))
}

footr_config = footr_setup()

neo_query <- function(query_string) {
  h = basicTextGatherer()

  auth_token = base64(glue::glue('{footr_config$username}:{footr_config$password}'))
  httpheader <- list(
    'Authorization'=glue::glue('Basic {auth_token}'),
    'Content-Type'='application/json'
  )

  query_url = glue::glue('http://{footr_config$ip}:7474/db/neo4j/tx')
  
  curlPerform(
    url=query_url,
    postfields=query_string,
    writefunction=h$update,
    httpheader=httpheader,
    verbose=FALSE
  )
  
  result <- fromJSON(h$value())
  
  raw_data = result$results[[1]]$data
  
  return(raw_data)
}

get_home_team = function(match_id) {
  query_string = glue::glue('{{"statements" : [ {{ "statement" : "MATCH (match:Match {{uid: \'{match_id}\'})-[:HOME_TEAM]-(home_team:Team) RETURN home_team"}} ]}}')
  raw = neo_query(query_string)
  return(raw[[1]]$row[[1]][[2]])
}

get_away_team = function(match_id) {
  query_string = glue::glue('{{"statements" : [ {{ "statement" : "MATCH (match:Match {{uid: \'{match_id}\'})-[:AWAY_TEAM]-(away_team:Team) RETURN away_team"}} ]}}')
  raw = neo_query(query_string)
  return(raw[[1]]$row[[1]][[2]])
}

get_home_goals = function(match_id) {
  query_string = glue::glue('{{"statements" : [ {{ "statement" : "MATCH (:Match {{uid: \'{match_id}\'})-[goal:GOAL]-(:Goal) WHERE goal.team_loc = \'H\' RETURN count(*)"}} ]}}')
  raw = neo_query(query_string)
  return(raw[[1]]$row)
}

get_away_goals = function(match_id) {
  query_string = glue::glue('{{"statements" : [ {{ "statement" : "MATCH (:Match {{uid: \'{match_id}\'})-[goal:GOAL]-(:Goal) WHERE goal.team_loc = \'A\' RETURN count(*)"}} ]}}')
  raw = neo_query(query_string)
  return(raw[[1]]$row)
}

get_matches = function(max_n=10) {
  query_string = '{"statements" : [ { "statement" : "MATCH (n:Match) RETURN n'

  if (is.null(max_n)) {
    query_string = glue::glue('{query_string} LIMIT {max_n}"}} ]}}')
  } else {
    query_string = glue::glue('{query_string}} ]}}')
  }
  print(query_string)
  raw_data = neo_query(query_string)

  n_matches = length(raw_data)

  date = vector(mode="character", length=n_matches)
  home_team = vector(mode="character", length=n_matches)
  away_team = vector(mode="character", length=n_matches)
  home_goals = vector(mode="integer", length=n_matches)
  away_goals = vector(mode="integer", length=n_matches)
  result = vector(mode="character", length=n_matches)

  for (match in 1:n_matches) {
    match_data = raw_data[[match]][[1]][[1]]
    
    match_id = match_data['uid']
    date[match] = match_data['date']

    home_team[match] = get_home_team(match_id)
    home_goals[match] = get_home_goals(match_id)

    away_team[match] = get_away_team(match_id)
    away_goals[match] = get_away_goals(match_id)
    result[match] = match_data['result']
  }

  return(data.frame(date, home_team, home_goals, away_team, away_goals, result))
}