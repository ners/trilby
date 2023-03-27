{ lib, ... }:

with builtins;
with lib;
{
  # Splits the string by newlines into a list of strings
  lines = splitString "\n";

  # Merges a list of string by newline into a single multiline string
  unlines = concatStringsSep "\n";

  # Splits the string by spaces into a list of strings
  words = splitString " ";

  # Merges a list of string by space into a single string
  unwords = concatStringsSep " ";

  # Read the list of files and concatenate their contents by the given separator
  concatFilesSep = sep: pipef [
    (map readFile)
    (concatStringsSep sep)
  ];

  # Read the list of files and concatenate their contents by newline
  concatFiles = concatFilesSep "\n";

  # Prepend one string to another
  addPrefix = prefix: str: prefix + str;

  # Append one string to another
  addSuffix = suffix: str: str + suffix;

  charactersToString = concatStringsSep "";

  # Make the first letter of the string uppercase.
  capitalise = pipef [
    stringToCharacters
    (cs: prepend (toUpper (head cs)) (tail cs))
    charactersToString
  ];
}
