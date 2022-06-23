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
