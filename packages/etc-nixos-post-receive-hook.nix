{ writeScript, bash, git }:
writeScript "nixos-post-receive-hook.sh" ''
  #! ${bash}/bin/bash
  while read oldSHA newSHA ref; do
    if [[ "$ref" != "refs/heads/master" ]]; then
      continue
    fi
    export GIT_WORK_TREE=$(cd $GIT_DIR/../ ; pwd)
    env | grep GIT
    echo ${git}/bin/git checkout -f master
    ${git}/bin/git checkout -f master
    break
  done
''
