library(methods)

FootrConfig <- setRefClass(
  "FootrConfig",
  fields=c(
    ip="character", 
    username="character", 
    password="character",
    max_active_queries="numeric"
  )
)

FootrConfig$methods(
  initialize = function() {
    .self$username = 'neo4j'
    .self$ip = ''
    .self$password = ''
    .self$max_active_queries = 800
    tryCatch(
      expr = {
        .self$load_from_env()
      },
      error = function(e){ 
        warning('Could not load footr config from env, use footr_config$logon("ip", "password")')
      }
    )
  },
  load_from_env = function() {
    ip = Sys.getenv("FOOTR_NEO_IP")
    if (ip == "") {
      stop('Need to setup FOOTR_NEO_IP in .Renviron before using Footr! If set, try restarting R or checking name.')
    }
    .self$ip = ip
    
    password = Sys.getenv("FOOTR_NEO_PASSWORD")
    if (password == "") {
      stop('Need to setup FOOTR_NEO_PASSWORD in .Renviron before using Footr! If set, try restarting R or checking name.')
    }
    .self$password = password
  },
  logon = function(ip, password) {
    .self$ip = ip
    .self$password = password
  },
  get_max_active_queries = function() {
    return(.self$max_active_queries)
  },
  get_ip = function() {
    if (.self$ip == "") {
      stop('Need to setup neo ip before using Footr! Set up in .Renviron or run logon.')
    }
    return(.self$ip)
  },
  get_password = function() {
    if (.self$password == "") {
      stop('Need to setup neo password before using Footr! Set up in .Renviron or run logon.')
    }
    return(.self$password)
  },
  get_username = function() {
    return(.self$username)
  }
)