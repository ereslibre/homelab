{pkgs, ...}: let
  pythonEnv = pkgs.python3.withPackages (ps:
    with ps; [
      ipykernel
      matplotlib
      numpy
      pandas
      scikit-learn
      scipy
      ipywidgets
    ]);
in {
  services.jupyterhub = {
    enable = true;
    kernels = {
      python3 = {
        displayName = "Python 3";
        argv = [
          "${pythonEnv.interpreter}"
          "-m"
          "ipykernel_launcher"
          "-f"
          "{connection_file}"
        ];
        language = "python";
      };
    };
    extraConfig = ''
      c.JupyterHub.authenticator_class = 'dummy'
      c.Authenticator.allowed_users = {'santi', 'arturo', 'ereslibre'}
      c.Authenticator.admin_users = {'ereslibre'}
      c.SystemdSpawner.notebook_dir = '~'
      c.SystemdSpawner.mem_limit = '4G'
      c.SystemdSpawner.cpu_limit = 2.0
    '';
  };
}
