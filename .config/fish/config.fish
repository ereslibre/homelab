set fish_greeting

set -x EDITOR "emacs -nw -q"
set -x GIT_EDITOR "emacs -nw -q"

if [ -x /usr/local/bin/keychain ]
    keychain --nogui $HOME/.ssh/id_rsa ^/dev/null
    source $HOME/.keychain/(hostname)-fish
end

if [ -x /usr/local/bin/docker-machine ]
  if docker-machine status default | grep -i running >/dev/null
    set shinit (docker-machine env default ^/dev/null)
    for i in (seq (count $shinit))
      eval $shinit[$i]
    end
  end
end

set PATH $HOME/bin $PATH
set PATH /usr/local/sbin $PATH

if [ -x /usr/local/bin/rbenv ]
  set PATH $HOME/.rbenv/bin $PATH
  set PATH $HOME/.rbenv/shims $PATH
  rbenv rehash >/dev/null ^&1
end

alias whoises 'ssh root@linux2100.neodigit.com '\''php /usr/custombin/dominios.es.stats/whois-toplus.php '\'' $1'
