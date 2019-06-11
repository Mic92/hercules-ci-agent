/*
  A module that configures cachix caches for reading, similar in functionality
  to the `cachix use` command.
 */
{ pkgs, lib, config, ...}:
let
  cfg = config.nix.cachix;
  inherit (lib.lists) filter concatMap concatLists;
  inherit (lib) types isAttrs mkIf escapeShellArg attrValues mapAttrsToList;
  inherit (builtins) readFile fromJSON map split;

  readCacheKeys = fileOrNull:
    if fileOrNull == null
    then { kind = "CacheKeys"; caches = {}; }
    else fromJSON (readFile fileOrNull);

  json = readCacheKeys cfg.secretsFile;
  inherit (json) caches;

  pubkeys = concatMap (cache: cache.publicKeys) (attrValues json.caches);

in
{
  options.nix.cachix = {
    secretsFile = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        A CacheKeys JSON file produced by the cachix export command. It
        will be read during evaluation. This can be a path expression, which
        will not be loaded into the Nix store by the declaring module.
      ''; # TODO (doc) CacheKeys format reference link
    };
    deployedSecretsPath = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        The path to the deployed nix.cachix.secretsFile on the target
        machine(s). This should be a plain string
        literal, to avoid accidentally copying secrets into the Nix store.

        The file will be read by root and any pull credentials will be made
        available exclusively to the Nix daemon. Non-root users of Nix tools
        may need to provide the credentials themselves to function properly.
        For this reason the caches are merely trusted and not enabled by default.
      '';
    };
    /*
      TODO: make this actually configurable
            This module is currently limited to the hardened version which
            deploys netrc to daemon-netrc with 0400, root:root

    rootOnly = lib.mkOption {
      type = types.bool;
      default = true;
      description = ''
        Writes the pull tokens (if any) to a file that is only readable by root
        and the Nix daemon. This makes it harder for users to retrieve these
        tokens.
      '';
    };
    */

  };

  config = lib.mkIf ( # || because of the assertions
                     cfg.secretsFile != null || cfg.deployedSecretsPath != null
                    ) {

    assertions = [
      { assertion = cfg.secretsFile != null -> json.kind == "CacheKeys";
        message = ''
          ${toString cfg.secretsFile}:
          nix.cachix.secretsFile must point to a JSON file with "kind":"CacheKeys"
        '';
      }
      { assertion = (cfg.secretsFile != null && json.kind == "CacheKeys")
                      -> (json ? apiVersion) != true;
        message = ''
          ${toString cfg.secretsFile}:
          nix.cachix.secretsFile points to a CacheKeys JSON file with an unsupported
          apiVersion.
          Please check the file and make sure you're using an up to date version of the
          Hercules CI Agent profile NixOS modules.
        '';
      }
      { assertion = cfg.deployedSecretsPath != null -> cfg.secretsFile != null;
        message = ''
          You need to specify nix.cachix.secretsFile in order to provide the
          non-sensitive parts of the cache configuration.

          WARNING: If you've used a path expression in cfg.deployedSecretsPath,
          you may want to delete it from your Nix store!
        '';
      }

      # TODO: not required when file is not sensitive.
      # TODO (doc) CacheKeys format reference link
      { assertion = cfg.secretsFile != null -> cfg.deployedSecretsPath != null;
        message = ''
          You need to deploy the CacheKeys JSON file to the machine outside the
          Nix store and set nix.cachix.deployedSecretsPath to the location of the
          deployed file.
        '';
      }
    ];

    nix.trustedBinaryCaches = ["https://cache.nixos.org"] ++ mapAttrsToList (name: keys: "https://${name}.cachix.org") caches;
    nix.binaryCachePublicKeys = concatLists (mapAttrsToList (name: keys: keys.publicKeys) caches);

    nix.extraOptions = ''
      netrc-file = /etc/nix/daemon-netrc
    '';

    systemd.paths.cachix-secrets = {
      wantedBy = [ "multi-user.target" ];
      pathConfig.PathExists = cfg.deployedSecretsPath;
      pathConfig.PathChanged = cfg.deployedSecretsPath;
      pathConfig.Unit = "cachix-install-netrc.service";
    };

    systemd.services.cachix-install-netrc = {
      requires = [ "cachix-secrets.path" ];
      serviceConfig.Type = "oneshot";
      script = ''
        ${pkgs.jq}/bin/jq -r <${escapeShellArg cfg.deployedSecretsPath} \
            '.caches | to_entries[] | .key as $key | .value.pullToken | select (. != null) | "machine \($key).cachix.org password \(.)" ' \
          | install --mode=0400 --owner=root --group=root \
              /dev/stdin \
              /etc/nix/daemon-netrc \
        ;
      '';
    };
  };
}
