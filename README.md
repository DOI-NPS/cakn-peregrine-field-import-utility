# R scripts cakn-peregrine-field-import-utility

Provides a set of functions to import collected field data from a
peregrine qField geopackage and insert into a target peregrine MS
Access database.

# Usage

Generally, the file `example_import.R` is an example of how this
utilitly is used. And this file, or a file similar to it, is then
"sourced" at the R prompt to load all relevant source files and
functions necessary for the import. The `main` function is then
executed from the R-prompt. This function executes an interactive user
prompt, where the user is asked to input an option string from a list
of options and press the enter button to execute this option.

## `example_import.R` setup

The global variables `SRC_CONN_STR`, `TGT_DB_SRC_PATH`,
`CORRECTIONS_CSV`, and `LOG_DIR` are assigned values by the script
user before the file `example_import.R` is sourced.

When file `example_import.R` is sourced, the source target file path
stored in variable `TGT_DB_SRC_PATH` is immediately renamed by
injecting a date/time stamp string into the name of the file; and this
renamed file path is then saved to the global variable
`NEW_TGT_DB_PATH`. This variable now stores the path of the new target
db that will be updated.

**NOTE:** If the file path stored in variable `TGT_DB_SRC_PATH`
already has a date/time stamp in its name of the format
`%Y-%m-%dT%H.%M.%S`, this date/time stamp is then merely substituted
with a new date/time stamp by the date/time string injection function,
and stored in the variable `NEW_TGT_DB_PATH`. Otherwise, a new
date/time stamp string is appended to the file path retrieved from
variable `TGT_DB_SRC_PATH`.

Global variables `CORRECTIONS_CSV` and `LOG_DIR` can either be
assigned (independently) a `NULL` or a *valid* file path name. If
neither of these conditions are met; the program will stop when the
function `main` is executed.

## `main()` execution

Once the file `example_import.R` has been sourced, the function `main`
is then executed from the R-prompt. Before `main` enters the user's
interactive options loop, it first copies the target database referred
to by global variable `TGT_DB_SRC_PATH` to the file referred to by
global variable `NEW_TGT_DB_PATH`. It then establishes databased
connections to the source and target databases indicated by variables
`SRC_CONN_STR` and `TGT_CONN_STR` (which uses the value in
`NEW_TGT_DB_PATH` for its database).

It then creates a logger environment variable that is used to log the
qlobal variable settings and the append (insert) operations of the
`append_peregrine_data` function. This log is then written to a log
file in the directory `LOG_DIR`. **NOTE:** this log is written only
for options `ac`, `aw`, and `dr`.

It then enters the interactive prompt for user options.

# User options

User options, from the example script, are typcially divided amongst
*append* and *diff* operations, and how these operations are either
displayed to the user or logged to a file; that is, so-called
*print* or *log* operations.

## `lp` and `lw`

Options `lp` and `lw` take the diffs between source and target *lookup*
tables, and respectively print or write this log to a file; if the
file path exists and the `LOG_DIR` variable is not set to `NULL`.

**NOTE:** The log file is automatically generated. Each log file name has
a unique date/time stamp.

## `ew` and `ewa`

Options `ew` and `ewa` create a diff on the source and target event
tables. The difference between them is in what keys are used to
perform the diff. The `ewa` option uses the alternate keys from on the
table `survey_event`. The `ew` just does a default comparison via
rows.

By default, it writes a file to the directory `LOG_DIR`.

**NOTE:** these options need more work.

## `ow`

Option `ow` performs a default row-wise diff on the source and target
observation tables.

By default, it writes a file to the directory `LOG_DIR`.

**NOTE:** this option need more work.

## `ac` and `aw` and `dr`

Option `ac` ("append WITH corrections") appends (inserts) database
records from the source database tables `survey_event` and
`survey_observation` into the same named tables of the target database
indictated by the variable `NEW_TGT_DB_PATH`, *with a correction's
file* that indicates substitute values necessary for the lookup table
values of the source database table records before they can be
properly inserted into the target database tables. **NOTE:** recall,
however, that the variable `CORRECTIONS_CSV` can be assigned `NULL`;
so it would still attempt to append the data in this case; but no
lookup table corrections would be made to the source databse tables.

Option `aw` ("append WITHOUT corrections") performs the same operation
as option `ac` *without* a corrections file; that is, regardless if a
correction's file is assigned to the global variable
`CORRECTIONS_CSV`.

Option `dr` ("DRY-RUN") *begins* the above mentioned append
process but then bails after the source table lookup values have been
*potentially* updated (no update would happen if variable
`CORRECTIONS_CSV` is set to `NULL`). A diff is then performed between
the source tables before this lookup update and after the lookup table
update. This diff is then written to a diff log in the directory
`LOG_DIR`. **NOTE:** no attempt is made to insert any records into the
target database.

The append operation conducts the following steps:

- Update the source tables (as data frames) with corrected lookup
  table values.
- Determine which records in the source event's table already exist in
  the target events table. Subtract these records from the source
  events and observation records (where the events ID are equal); the
  remaining source event and observation records will be inserted.
- Determine if duplicate records exist in the source events table. If
  they do, append the table of duplicates to the logger log. **NOTE:**
  this utility will still attempt to append the source records to the
  target database; this will generate an error, and ultimately, the
  append operation will be rolled back. This error will also be logged
  to the logger and written to the log file.
- Append source event and observation records to to target tables
  (without mult-valued columns).
- Update multi-valued-field values in the target with (possibly
  corrected) multi-value-field values from the source. **NOTE:** this
  is *counterintuitively* an *insert* operation due to how MS Access
  stores multi-valued-fields.
- Write logger log to log file in directory `LOG_DIR`.

**NOTE:** if either the append or multi-valued-field update (insert)
fail the update operation is rolled back in its entirety.

## `q`

Quit the utility.
