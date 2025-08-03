# Revision history for trilby-cli

## 25.8.0 -- 2025-08-02

* Better locale and keyboard layout selection during Install

## 25.7.0 -- 2025-07-26

* Install passes flake inputs as system and user args

## 25.5.0 -- 2025-05-10

* Update to GHC 9.8.4
* Install adds home-manager to flake
* Add Clean command

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
