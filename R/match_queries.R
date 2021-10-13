match_queries = list(
  #' Expands a match_id to the home team participating in the match
  #'
  #' @param match_id Unique ID of the match to expand
  #'
  #' @return String of home team
  #' @export
  #'
  #' @examples
  #' match_queries['home_team']['fcn'](match_id)
  'home_team'=list(
    'fcn'=function(match_id) {
      cypher_query = glue::glue(
        'MATCH (match:Match {{uid: \'{match_id}\'})-[:HOME_TEAM]-(home_team:Team) RETURN home_team'
      )
      raw = execute_cypher(cypher_query)
      return(raw[[1]]$row[[1]][[2]])
    },
    'type'='character'
  ),
  #' Expands a match_id to the away team participating in the match
  #'
  #' @param match_id Unique ID of the match to expand
  #'
  #' @return String of away team
  #' @export
  #'
  #' @examples
  #' match_queries['away_team']['fcn'](match_id)
  'away_team'=list(
    'fcn'=function(match_id) {
      cypher_query = glue::glue('MATCH (match:Match {{uid: \'{match_id}\'})-[:AWAY_TEAM]-(away_team:Team) RETURN away_team')
      raw = execute_cypher(cypher_query)
      return(raw[[1]]$row[[1]][[2]])
    },
    'type'='character'
  ),
  #' Expands a match_id to the goals scored by the home team
  #'
  #' @param match_id Unique ID of the match to expand
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' match_queries['home_goals']['fcn'](match_id)
  'home_goals'=list(
    'fcn'=function(match_id) {
      cypher_query = glue::glue('MATCH (:Match {{uid: \'{match_id}\'})-[goal:GOAL]-(:Goal) WHERE goal.team_loc = \'H\' RETURN count(*)')
      raw = execute_cypher(cypher_query)
      return(raw[[1]]$row)
    },
    'type'='integer'
  ),
  #' Expands a match_id to the goals scored by the away team
  #'
  #' @param match_id Unique ID of the match to expand
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' match_queries['away_goals']['fcn'](match_id)
  'away_goals'=list(
    'fcn'=function(match_id) {
      query_string = glue::glue('MATCH (:Match {{uid: \'{match_id}\'})-[goal:GOAL]-(:Goal) WHERE goal.team_loc = \'A\' RETURN count(*)')
      raw = execute_cypher(query_string)
      return(raw[[1]]$row)
    },
    'type'='integer'
  )
)





