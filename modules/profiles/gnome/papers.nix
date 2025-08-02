{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.papers
  ];

  xdg.mime.inverted.defaultApplications."org.gnome.Papers.desktop" = [
    "application/illustrator"
    "application/pdf"
    "application/vnd.comicbook+zip"
    "application/vnd.comicbook-rar"
    "application/x-bzpdf"
    "application/x-cb7"
    "application/x-cbr"
    "application/x-cbt"
    "application/x-cbz"
    "application/x-ext-cb7"
    "application/x-ext-cbr"
    "application/x-ext-cbt"
    "application/x-ext-cbz"
    "application/x-ext-djv"
    "application/x-ext-djvu"
    "application/x-ext-pdf"
    "application/x-gzpdf"
    "application/x-xzpdf"
    "image/tiff"
    "image/vnd.djvu"
    "image/vnd.djvu+multipage"
  ];
}
