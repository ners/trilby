{
  security.polkit.enable = true;

  # Cache admin credentials for 5 minutes
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
        if ((action.id == "org.freedesktop.systemd1.manage-units") &&
            subject.isInGroup("wheel")) {
                return polkit.Result.AUTH_ADMIN_KEEP;
        }
    });
  '';
}
