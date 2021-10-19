source('./R/match_queries.R') # Provides options for expanding match rows
source('./R/config_manager.R') # Provides configuration

#' Manage config for the footr connection
#'
#' @export
#'
#' @examples
#' footr_config.logon('ip', 'password')
footr_config = FootrConfig$new()

#' Get the number of active queries currently running on the DB
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

#' Execute a cypher (https://neo4j.com/developer/cypher/) query safely.
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
  while(get_active_queries() > footr_config$get_max_active_queries()) {
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

  auth_token = RCurl::base64Encode(glue::glue('{footr_config$get_username()}:{footr_config$get_password()}'))
  httpheader <- list(
    'Authorization'=glue::glue('Basic {auth_token}'),
    'Content-Type'='application/json'
  )

  query_url = glue::glue('http://{footr_config$get_ip()}:7474/db/neo4j/tx')
  
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

#' Get matches from the DB
#' @param expansions A list of wanted match expansions as defined in match_queries
#' @param date A tuple of from->to dates in format c('yyyy-mm-dd', 'yyyy-mm-dd'), filters matches inclusively
#' @param team  A string name, if passed will return matches for only this team
#' @param max_n The maximum number of games to return, for now limit set to NULL
#' @return A data.frame of matches which matches query
#' @export
#'
#' @examples
#' basic_expansions = c('home_team', 'away_team', 'home_goals', 'away_goals')
#' get_matches(basic_expansions, max_n=NULL) # get all matches
#' get_matches(basic_expansions, team='Arsenal') # get Arsenal matches (max 10)
get_matches = function(expansions, date=NULL, team=NULL, max_n=10) {
  cypher_query = 'MATCH (match_node:Match)'
  
  if(!is.null(team)) {
    cypher_query = glue::glue('{cypher_query}-[:HOME_TEAM|AWAY_TEAM]-(team:Team) WHERE team.name = \'{team}\'')
  }
  
  if(!is.null(date)) {
    start_date = date[1]
    end_date = date[2]
    date_condition = glue::glue('match_node.date > \'{start_date}\' AND match_node.date < \'{end_date}\'')
    if(!is.null(team)) {
      cypher_query = glue::glue('{cypher_query} AND {date_condition}')
    } else {
      cypher_query = glue::glue('{cypher_query} WHERE {date_condition}')
    }
  }
  
  cypher_query = glue::glue('{cypher_query} RETURN match_node')

  if (!is.null(max_n)) {
    cypher_query = glue::glue('{cypher_query} LIMIT {max_n}')
  }
  
  raw_data = execute_cypher(cypher_query,  verbose=TRUE)

  n_matches = length(raw_data)
  
  expanded_data = data.frame(
    'date'=vector(mode="character", length=n_matches),
    'result'=vector(mode="character", length=n_matches),
    stringsAsFactors=FALSE
  )
  
  for (expansion in expansions) {
    if(!(expansion %in% names(match_queries))) {
      stop(glue::glue('Match expansion {expansion} is not defined! Check match_queries.R'))
    }
    expanded_data[expansion] = vector(match_queries[[c(expansion, 'type')]], length=n_matches)
  }
  
  progress_bar = txtProgressBar(max=n_matches)
  print(glue::glue('Found {n_matches} matches, building match data...'))
  for (match in 1:n_matches) {
    match_data = raw_data[[match]][[1]][[1]]
    match_id = match_data['uid']

    expanded_data[['date']][match] = match_data['date'][[1]]
    expanded_data[['result']][match] = match_data['result'][[1]]
    
    for (expansion in expansions) {
      expanded_data[[expansion]][match] = match_queries[[c(expansion, 'fcn')]](match_id)
    }

    setTxtProgressBar(progress_bar, match)
  }
  return(expanded_data)
}