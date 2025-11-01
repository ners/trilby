{
  security.polkit.enable = true;

  # Cache admin credentials for 5 minutes
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (action.id == "org.freedesktop.policykit.exec") {
        return polkit.Result.AUTH_ADMIN_KEEP;
      }
    });

    polkit.addRule(function(action, subject) {
      if (action.id.indexOf("org.freedesktop.systemd1.") == 0) {
        return polkit.Result.AUTH_ADMIN_KEEP;
      }
    });
  '';
}
