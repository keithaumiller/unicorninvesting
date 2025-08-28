library(DBI)
library(bcrypt)

source('constant.R')
source('util/utils.R')

source('data/db.R')
source('entity/portfolio.R')

user.get_by_username <- function (username) {
  database  <- db.connect()
  table     <- join(c(db.PREFIX, 'users'))

  statement <- join(c("SELECT * FROM ", table, " WHERE username = '", username, "'"))

  log.info('user', paste('Executing statement:', statement))

  result    <- dbGetQuery(database, statement)

  db.disconnect(database)

  if ( is.empty(result) ) { result <- NA }

  return(result)
}

user.exists   <- function (username) {
  result      <- user.get_by_username(username)

  exists      <- !is.na(result)

  return(exists)
}

user.get      <- function (username, password) {
  result      <- user.get_by_username(username)

  if ( !is.na(result) ) {
    hashpass  <- result$password
    validated <- checkpw(password, hashpass)

    if ( is.true(validated) ) {
      log.success('user', paste('Validation successful of User:', username))
    } else {
      log.danger('user', paste('Validation unsucessful of User:', username))

      result  <- NULL
    }
  }

  return(result)
}

user.register <- function (username, firstname, lastname, email, password, dob, gender) {
  gender      <- assign.if.na(gender, gender.NA)
  hashpass    <- hashpw(password, gensalt(db.PASSWORD_SALT))
  values      <- list(
    username   = username,
    firstname  = firstname,
    lastname   = lastname,
    email      = email,
    password   = hashpass,
    dob        = dob,
    gender     = gender
  )

  log.info('user', paste('Registering User with values:', join(values, ', ')))

  if ( is.true(db.insert('users', values)) ) {
    user      <- user.get(username, password)
  } else {
    user      <- NULL
  }

  return(user)
}
