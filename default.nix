# This file was generated by the {rix} R package v0.9.0 on 2024-08-09
# with following call:
# >rix::rix(r_ver = "019f5c29c5afeb215587e17bf1ec31dc1913595b",
#  > r_pkgs = c("pagedown",
#  > "qrcode",
#  > "stringi",
#  > "stringr"),
#  > ide = "rstudio",
#  > project_path = ".",
#  > overwrite = TRUE,
#  > print = TRUE)
# It uses nixpkgs' revision 019f5c29c5afeb215587e17bf1ec31dc1913595b for reproducibility purposes
# which will install R version 4.3.3.
# Report any issues to https://github.com/b-rodrigues/rix
let
 pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/019f5c29c5afeb215587e17bf1ec31dc1913595b.tar.gz") {};
 
  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages) 
      cli
      digest
      dplyr
      fs
      glue
      jsonlite
      purrr
      readr
      testthat
      tibble
      tidyr
#       emo
      R6
      checkmate
      devtools
      usethis;
  };
    
  system_packages = builtins.attrValues {
    inherit (pkgs) 
      R
      glibcLocales
      nix;
  };
 
  wrapped_pkgs = pkgs.rstudioWrapper.override {
    packages = [  rpkgs  ];
  };
 
in

pkgs.mkShell {
  LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux" then  "${pkgs.glibcLocales}/lib/locale/locale-archive" else "";
  LANG = "en_US.UTF-8";
   LC_ALL = "en_US.UTF-8";
   LC_TIME = "en_US.UTF-8";
   LC_MONETARY = "en_US.UTF-8";
   LC_PAPER = "en_US.UTF-8";
   LC_MEASUREMENT = "en_US.UTF-8";

  buildInputs = [  rpkgs  system_packages  wrapped_pkgs ];
  
}
