# amlrDatabases 0.7.0.9000 (dev)

* Removed the `parent` argument from `mod_output_server`. The needed information (i.e., the string of the parent namespace ID) is now determined using `session`

* Removed the `plot.res` argument from `mod_output_server`, and made this a hard-coded value within the function.

* Updated `mod_output` to more consistently display plots, tables, and associated widgets and messages, through the use of `reactiveVal` and `renderUI`/`uiOutput` functions

* Updated `mod_output` so that users can specify ggplot plot height and width

* Added `round_logical` to always round up values of 5


# amlrDatabases 0.6.1

* Updated `mod_output` so that validate messages actually get passed through and displayed in table and plot output

* Added `db.selected` argument to `mod_database_server` to allow users to pre-select a database connection if passing connection(s) via `pool.list`. Default value is `NULL`, so as to not change default behavior


# amlrDatabases 0.6.0

* Changed `amlr_season` to `amlr_season_from_date` for clarity

* Added function `amlr_date_from_season` for generating date from season name, month, and date


# amlrDatabases 0.5.0

* `mod_output` can now generate ggplot or plotly output plots


# amlrDatabases 0.4.2

* Added `amlr_dbConnect` function for easily connecting to AMLR SQL databases using recommended parameters

* Added `amlr_season` function for easily constructing a string with the season name from date(s)

* Added `tableNA` function, commonly used in AMLR database scripts

# amlrDatabases 0.4.1

* Added default arguments to `mod_database` for widget defaults so that developers can customize as necessary


# amlrDatabases 0.4.0

* `mod_database` now takes in a list of any number of pool objects, whose names are displayed in the UI as options for the user


# amlrDatabases 0.3.1

* Move full shiny app to R folder


# amlrDatabases 0.3.0

* Added a basic Shiny app for trying the shiny modules in this package

* Update `mod_database` to allow user to specify other database with user/pwd/port login

* `mod_output` now takes parent's session object as input, rather than ID, for more robust way of generating output IDs and defualt download names


# amlrDatabases 0.2.0

* Added 'Encrypt' as an argument to `amlr_dbPool` and changed teh default driver to ODBC Driver 18 for SQL Server


# amlrDatabases 0.1.0

* Added 'Trusted_Connection' as an argument to `amlr_dbPool`

* Changed default driver for `amlr_dbPool` to 'SQL Server Native Client 11.0'

* The mod_database module now lets the user specify the server and database name, and has many fewer required arguments


# amlrDatabases 0.0.1

* Initial version
