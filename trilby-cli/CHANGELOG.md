# Revision history for trilby-cli

## 25.5.0 -- 2025-05-25

* Update to GHC 9.8.4

## 25.1.0 -- 2024-10-09

* Set up dependencies on startup

## 24.8.0 -- 2024-08-01

* Add Darwin support to Install and Update

## 24.7.0 -- 2024-07-24

* Print version when running verbosely
* Use correct PATH when running in Nix sandbox
* Update flakes recursively
* Add Infect command
* Switch from TH to callstack-based logging
* Remove library, build only executable
* Switch from FilePath to Path
* Allow editing files during Install

## 24.4.0 -- 2024-04-04

* Set profile on update
* Introduce [CalVer](https://calver.org/) versioning (YY.MM.MICRO)
* Add `--version` flag
* Replace `--verbosity` with `--quiet`, `--verbose`, and `--debug`
* Drop direct dependency on `terminal`

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
