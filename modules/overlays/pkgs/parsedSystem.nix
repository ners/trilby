{ lib, stdenv, ... }: lib.systems.parse.mkSystemFromString stdenv.hostPlatform.system
