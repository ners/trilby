# Revision history for trilby-cli

## 25.12 -- 2025-12-02

* Read optional TRILBY env var to locate Trilby flake
* Relax searching for keyboards

## 25.10.1 -- 2025-10-29

* Only update git-tracked flake locks

## 25.10.0 -- 2025-10-26

* Remove broken SSH ControlPersist

## 25.8.0 -- 2025-08-02

* Migrate from mtl to effectful
* Migrate from turtle to typed-process
* Migrate from monad-logger to log-base with full callstacks
* Better locale and keyboard layout selection during Install
* Move Prelude to Trilby.Prelude to unbreak autogen modules
* Use Polkit instead of sudo
* Use correct kexec format in Infect

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
