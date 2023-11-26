{ pkgs, config, ... }:
{
  nixpkgs.overlays = [
    (self: super: {
      etc-nixos-post-receive-hook = self.writeScript "nixos-post-receive-hook.sh" ''
        #! ${self.bash}/bin/bash
        while read oldSHA newSHA ref; do
          if [[ "$ref" != "refs/heads/master" ]]; then
            continue
          fi
          export GIT_WORK_TREE=$(cd $GIT_DIR/../ ; pwd)
          env | grep GIT
          echo ${self.git}/bin/git checkout -f master
          ${self.git}/bin/git checkout -f master
          break
        done
      '';
    })
  ];
}
