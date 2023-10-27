library(dotenv)
# You can control your local app development via environment variables.
# You can define things like input-data, app-configuration etc.
# Per default your environment is defined in `/.env`
load_dot_env()

# provide common stuff
source("src/common/logger.R")
source("src/common/runtime_configuration.R")
clearRecentOutput()
# This will parse a JSON file containing the concrete configuration of
# the app run. Per default the file `/app-configuration.json` will be parsed.
args <- list() # if your function has no arguments, this line still needs to be active
# Add all your arguments of your r-function here
args[["start"]] = "05-19"
args[["end"]] = "07-07"
args[["nfixes"]] = 1
args[["dayloss"]] = 10
args[["restrictive"]] = FALSE
args[["int"]] = 3
args[["kcons_min"]] = 5
args[["kcons_max"]] = 21
args[["models"]] = "calfonly"

# Lets simulate running your app on MoveApps
source("src/moveapps.R")
simulateMoveAppsRun(args)
