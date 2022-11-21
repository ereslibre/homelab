{ username }: ''
  Host ereslibre-1.oracle.cloud ereslibre-2.oracle.cloud strong-arm-1.oracle.cloud
      User                  ubuntu

  Host pinfra-* surpillance-*
      User                  pi

  Host nuc-1 cpi-5
      ForwardAgent          yes
      RemoteForward         /run/user/1000/gnupg/S.gpg-agent /Users/${username}/.gnupg/S.gpg-agent.extra

  Host *.ereslibre.local *.ereslibre.net
      ForwardAgent          yes

  Host 10.0.*
      ForwardAgent          yes

  Host *
      User                  ereslibre
      Compression           yes
      ExitOnForwardFailure  yes
      ForwardX11            no
      ServerAliveCountMax   10
      ServerAliveInterval   20
      TCPKeepAlive          no
''
