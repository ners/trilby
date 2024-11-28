{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.rhythmbox
  ];

  xdg.mime.inverted.defaultApplications."org.gnome.Rhythmbox3.desktop" = [
    "x-content/audio-cdda"
    "x-content/audio-dvd"
    "x-content/audio-player"
    "application/ogg"
    "application/x-ogg"
    "audio/mp4"
    "audio/mpeg"
    "audio/x-flac"
    "audio/x-it"
    "audio/x-mod"
    "audio/x-mp3"
    "audio/x-mpeg"
    "audio/x-mpegurl"
    "audio/x-s3m"
    "audio/x-scpls"
    "audio/x-stm"
    "audio/x-vorbis"
    "audio/x-vorbis+ogg"
    "audio/x-xm"
  ];
}
