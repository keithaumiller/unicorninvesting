source('util/utils.R')

# General Environment Variables
REQUIRED_PACKAGES <<- readLines('assets/dependencies.txt')
PACKAGE_MIRROR    <<- Sys.getenv('UNIQUANT_PACKAGE_MIRROR', 'http://cran.us.r-project.org')

# Gender Codes
# For codes, visit https://en.wikipedia.org/wiki/ISO/IEC_5218
gender.MALE       <<- 1
gender.FEMALE     <<- 2
gender.UNKNOWN    <<- 3
gender.NA         <<- 9 # Not Applicable

# Database Environment Variables
db.NAME           <<- Sys.getenv('UNIQUANT_DB_NAME', 'uniquant')
db.HOSTNAME       <<- Sys.getenv('UNIQUANT_DB_HOST', '127.0.0.1')
db.PORT           <<- as.numeric(Sys.getenv('UNIQUANT_DB_PORT', 0))
db.USERNAME       <<- Sys.getenv('UNIQUANT_DB_USER', 'root')
db.PASSWORD       <<- Sys.getenv('UNIQUANT_DB_PASS', '')
db.PASSWORD_SALT  <<- as.numeric(Sys.getenv('UNIQUANT_PASSWORD_SALT', 10))
db.PREFIX         <<- Sys.getenv('UNIQUANT_DB_PREFIX', join(c(db.NAME, '_')))

# Path Constants
path.CACHE        <<- Sys.getenv('UNIQUANT_CACHEDIR', file.path(path.expand("~"), "_uniquant"))
path.ROOT         <<- normalizePath(".")
path.ASSETS       <<- file.path(path.ROOT, "assets")
path.DATA         <<- file.path(path.ASSETS, "data")
path.SCRAPERS     <<- file.path(path.ROOT, "data", "scrapers")

# Currency Codes
currencies         <- readLines(file.path(path.DATA, "currencies.txt"))
for (i in 1:length(currencies)) {
  code <- currencies[i]

  assign(join(c('forex.', code)), code, envir = .GlobalEnv)
}