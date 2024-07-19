{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    lm_sensors
    ltrace
    man-pages
    man-pages-posix
  ];
}
