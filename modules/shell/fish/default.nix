{ config, pkgs, lib, ... }:
let 
  cfg = config.modules.shell.fish;
in
  {
    options.modules.shell.fish = {
      enable = lib.mkEnableOption "fish";
    };
    config = lib.mkIf cfg.enable {
      programs.fish = {
        enable = true;
        shellAbbrs = {
          hme = "home-manager edit";
          hms = "home-manager switch";
        };
        functions = {
          start_agent.body = ''
            echo "Initializing new SSH agent ..."
            ssh-agent -c | sed 's/^echo/#echo/' > $SSH_ENV
            echo "succeeded"
            chmod 600 $SSH_ENV 
            . $SSH_ENV > /dev/null
            ssh-add
          '';
          test_identities.body = ''
            ssh-add -l | grep "The agent has no identities" > /dev/null
            if [ $status -eq 0 ]
              ssh-add
              if [ $status -eq 2 ]
                start_agent
              end
            end
          '';
        };
        shellInit = ''
          # See https://gist.github.com/gerbsen/5fd8aa0fde87ac7a2cae
          # content has to be in .config/fish/config.fish
          # if it does not exist, create the file
          setenv SSH_ENV $HOME/.ssh/environment
          if [ -n "$SSH_AGENT_PID" ] 
            ps -ef | grep $SSH_AGENT_PID | grep ssh-agent > /dev/null
            if [ $status -eq 0 ]
              test_identities
            end  
          else
            if [ -f $SSH_ENV ]
              . $SSH_ENV > /dev/null
            end  
            ps -ef | grep $SSH_AGENT_PID | grep -v grep | grep ssh-agent > /dev/null
            if [ $status -eq 0 ]
              test_identities
            else 
              start_agent
            end
          end
          # TODO I'd prefer this to be in my GNU Emacs module.
          # https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
          if [ "$INSIDE_EMACS" = 'vterm' ]
            function clear
              vterm_printf "51;Evterm-clear-scrollback";
              tput clear;
            end
          end
        '';
      };
    };
  }
