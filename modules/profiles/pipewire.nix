{ trilby, lib, ... }:

{
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    jack.enable = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };
} //
(if (lib.versionAtLeast trilby.release "25.05") then {
  services.pulseaudio.enable = false;
} else {
  hardware.pulseaudio.enable = false;
})
