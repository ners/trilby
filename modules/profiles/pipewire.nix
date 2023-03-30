{ lib, ... }:

{
  services.pipewire = {
    alsa.enable = true;
    alsa.support32Bit = true;
    enable = true;
    jack.enable = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };
  sound.enable = true;
  hardware.pulseaudio.enable = lib.mkForce false;
}
