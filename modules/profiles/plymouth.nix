{ ... }:

{
  # Plymouth presents a graphic animation (also known as a bootsplash) while the system boots.
  # It provides eye-candy and a more professional presentation for scenarios where the default
  # high-information text output might be undesirable.
  # It also handles boot prompts, such as entering disk encryption passwords.
  boot.plymouth = {
    enable = true;
    theme = "breeze";
  };
}
