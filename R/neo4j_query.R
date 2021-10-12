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
  
  config = list(
    ip=ip, 
    password=password, 
    username=username, 
    max_active_queries=700 # default is 1000, so give some slack
  )

  return(config)
}

footr_config = footr_setup()

#' Title Get the number of active queries currently running on the DB
#'
#' @return Integer count of the active queries
#' @export
#'
#' @examples
get_active_queries = function() {
  active_query_count = 'CALL dbms.listTransactions() YIELD transactionId RETURN count(transactionId)'
  active_queries = neo_query(active_query_count)
  return(active_queries[[1]]$row - 1) # -1 as this query is included.
}

#' Title Execute a cypher (https://neo4j.com/developer/cypher/) query safely.
#'
#' @param cypher_query A string of the cypher_query
#' @param verbose Boolean flag, wether or not verbose output should be provided.
#'
#' @return List datastructure containing the raw output of the response.
#' @export
#'
#' @examples
#' raw_result = execute_cypher('MATCH (match_node:Match) RETURN match_node LIMIT 5')
execute_cypher = function(cypher_query, verbose=FALSE) {
  max_retry = 100
  retry = 0
  while(get_active_queries() > footr_config$max_active_queries) {
    if (retry > max_retry) {
      error_msg = glue::glue('Exceeded max retry for cypher query! Please check DB for potential errors.')
      stop(error_msg)
    }
    Sys.sleep(10) # Wait for 10s as naive rate limit
    retry = retry + 1
  }
  return(neo_query(cypher_query, verbose=verbose))
}

neo_query <- function(cypher_query, verbose=FALSE) {
  if (verbose) {
    print(glue::glue('Executing: {cypher_query}'))
  }
  
  query_string = glue::glue('{{"statements" : [ {{ "statement" : "{cypher_query}"}} ]}}')
  h = RCurl::basicTextGatherer()

  auth_token = RCurl::base64Encode(glue::glue('{footr_config$username}:{footr_config$password}'))
  httpheader <- list(
    'Authorization'=glue::glue('Basic {auth_token}'),
    'Content-Type'='application/json'
  )

  query_url = glue::glue('http://{footr_config$ip}:7474/db/neo4j/tx')
  
  RCurl::curlPerform(
    url=query_url,
    postfields=query_string,
    writefunction=h$update,
    httpheader=httpheader,
    verbose=FALSE
  )

  response = h$value()

  result <- RJSONIO::fromJSON(h$value())
  
  if (length(result$errors) > 0) {
    print('Received error response!')
    print(response)
  } else {
    raw_data = result$results[[1]]$data
    return(raw_data)
  }
}

get_home_team = function(match_id) {
  cypher_query = glue::glue('MATCH (match:Match {{uid: \'{match_id}\'})-[:HOME_TEAM]-(home_team:Team) RETURN home_team')
  raw = execute_cypher(cypher_query)
  return(raw[[1]]$row[[1]][[2]])
}

get_away_team = function(match_id) {
  cypher_query = glue::glue('MATCH (match:Match {{uid: \'{match_id}\'})-[:AWAY_TEAM]-(away_team:Team) RETURN away_team')
  raw = execute_cypher(cypher_query)
  return(raw[[1]]$row[[1]][[2]])
}

#' Get the home goals for a particular match, selected by match id
#'
#' @param match_id 
#'
#' @return
#' @export
#'
#' @examples
get_home_goals = function(match_id) {
  cypher_query = glue::glue('MATCH (:Match {{uid: \'{match_id}\'})-[goal:GOAL]-(:Goal) WHERE goal.team_loc = \'H\' RETURN count(*)')
  raw = execute_cypher(cypher_query)
  return(raw[[1]]$row)
}

get_away_goals = function(match_id) {
  query_string = glue::glue('MATCH (:Match {{uid: \'{match_id}\'})-[goal:GOAL]-(:Goal) WHERE goal.team_loc = \'A\' RETURN count(*)')
  raw = execute_cypher(query_string)
  return(raw[[1]]$row)
}

#' Get matches from the DB
#'
#' @param max_n The maximum number of games to return, for now limit set to NULL
#'
#' @return A data.frame of matches which matches query
#' @export
#'
#' @examples
#' get_matches(max_n=NULL) # get all matches
get_matches = function(max_n=10) {
  cypher_query = 'MATCH (match_node:Match) RETURN match_node'

  if (!is.null(max_n)) {
    cypher_query = glue::glue('{cypher_query} LIMIT {max_n}')
  }
  
  raw_data = execute_cypher(cypher_query,  verbose=TRUE)

  n_matches = length(raw_data)

  date = vector(mode="character", length=n_matches)
  home_team = vector(mode="character", length=n_matches)
  away_team = vector(mode="character", length=n_matches)
  home_goals = vector(mode="integer", length=n_matches)
  away_goals = vector(mode="integer", length=n_matches)
  result = vector(mode="character", length=n_matches)
  
  progress_bar = txtProgressBar(max=n_matches)
  print(glue::glue('Found {n_matches} matches, building match data...'))
  for (match in 1:n_matches) {
    match_data = raw_data[[match]][[1]][[1]]
    
    match_id = match_data['uid']
    date[match] = match_data['date']

    home_team[match] = get_home_team(match_id)
    home_goals[match] = get_home_goals(match_id)

    away_team[match] = get_away_team(match_id)
    away_goals[match] = get_away_goals(match_id)
    result[match] = match_data['result']
    
    setTxtProgressBar(progress_bar, match)
  }

  return(data.frame(date, home_team, home_goals, away_team, away_goals, result))
}