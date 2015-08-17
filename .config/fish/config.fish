set fish_greeting

if [ -x /usr/local/bin/keychain ]
    keychain --nogui id_rsa ^/dev/null
    . $HOME/.keychain/$HOSTNAME-fish
end

if docker-machine status default | grep running >/dev/null
  set shinit (docker-machine env default ^/dev/null)
  for i in (seq (count $shinit))
    eval $shinit[$i]
  end
end

set PATH $HOME/bin $PATH
set PATH /usr/local/sbin $PATH

set PATH $HOME/.rbenv/bin $PATH
set PATH $HOME/.rbenv/shims $PATH
rbenv rehash >/dev/null ^&1

alias whoises 'ssh root@linux2100.neodigit.com '\''php /usr/custombin/dominios.es.stats/whois-toplus.php '\'' $1'
