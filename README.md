# R scripts cakn-peregrine-field-import-utility

Provides a set of functions to import collected field data from a
qField geopackage and insert into a target MS Access database.

# Usage

Generally, the file `example_import.R` is an example of how this
utilitly is used. And this file, or a file similar to it, is then
"sourced" at the R prompt to load all relevant source files and
functions necessary for the import. It has one `main` function that
controls the import process. In this function, the user must replace
the fake source geopackage and target MS Access paths with proper
paths before running the script.

When run, the `main` function will create connections to the source
and target databases, and insert into the target database tables with
records from the corresponding tables in the source database. Primary
keys from the source tables will be replaced with new key values
before being inserted into the target database.
