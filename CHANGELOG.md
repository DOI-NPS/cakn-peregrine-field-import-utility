# ChangeLog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/).

## [Unreleased]

### Added

- Add files: `CHANGELOG.md`, `DESCRIPTION.txt`, and `code.jon`.
- Add `else` clause to function `has_path` to explicitly return
  `NULL`, rather than implicitly.
- Add `else` clause to option `lw`; print to screen that log directory
  not provided.

### Removed
### Changed

- Refactor. Improve option `lw` wording.
- Update the `README.md` file to document this utility's operation.
- Refactor. Make the order of the correction and log file tests first.
  Don't copy database if these tests are goint to fail and stop the
  program.

### Fixed

## [1.0.0] 2026-10-08

### Added

- Add function `xdf_not_in_ydf` to filter on records that are only in
  the source event table and not in the target event table.
  - This function filters the source event records that don't already
    exist in the target event table. These filtered records are then
    the potential (if no other issues are present; i.e. source
    duplicates) records to be inserted into the target event table.
    - This also means that source event records can exist for multiple
      field trip visits from one geopackage db.
- Add function `get_df_dups` to find duplicate records in the source
  event df.
  - The field geopackage might have duplicates on the alternate
    primary key; these need to be eliminated before the import can
    succeed to the target db. These duplicates are written to the
    logger file.
    - TODO(?): Implement alternate primary key in geopackage?
  - Currently, if there are source event duplicates, this fact, and
    the tibble df string, are appended to the logger file, and the
    attemmpted insert of these event records into the target event
    table *continues*. This attempt will *fail* (and cause a rollback)
    (since the target event table has an alternate key index on these
    columns); this error will be written to the logger as well.
- Add function `append_log`.
  - This function appends the log string to the `logger$log`
    environment variable.
- Add more logging messages.
- Add source event and observation and target event counts to log;
  before and after filtering.

### Changed
- Move call to function `get_log_infor` from function
  `append_peregrine_data` to function `main`.

## 2026-09-28

### Added
### Removed
### Changed
### Fixed

## 2026-09-25

### Added
### Removed
### Changed
### Fixed

## 2026-09-23

### Added
### Removed
### Changed
### Fixed

## 2026-09-18

### Added
### Removed
### Changed
### Fixed

## 2026-09-10

### Added
### Removed
### Changed
### Fixed

## 2026-08-27

### Added
### Removed

### Changed
- Rename `gpkg_` to `src_` and `db_` to `tgt_`, and `_observation` to
  `_obs`.
- Reorder source and target arguments in function `append_peregrine_data`.
- Revise max survey event and observation id retieval code to return only
  one max value each.

### Fixed

## 2026-08-26

Initial commit.

### Added

- Add import utilitiy R script files.
  - `db_insert.R`
  - `db_tools.R`
  - `example_import.R`
  - Incomplete at this time: file `db_update.R` is missing.
    - This file needs revision before it is added to repo.

- Authored by: Liam Smith, CAKN
- With revisions by: Nick Bywater, OCIO

### Removed
### Changed
### Fixed
