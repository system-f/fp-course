FROM gitpod/workspace-full

USER root

# Install Nix
RUN addgroup --system nixbld \
  && adduser gitpod nixbld \
  && for i in $(seq 1 30); do useradd -ms /bin/bash nixbld$i &&  adduser nixbld$i nixbld; done \
  && mkdir -m 0755 /nix && chown gitpod /nix \
  && mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf

# Install Nix
CMD /bin/bash -l
USER gitpod
ENV USER gitpod
WORKDIR /home/gitpod

RUN touch .bash_profile \
 && curl https://nixos.org/releases/nix/nix-2.2.1/install | sh 

RUN echo '. /home/gitpod/.nix-profile/etc/profile.d/nix.sh' >> /home/gitpod/.bashrc

# Install HIE
RUN . /home/gitpod/.nix-profile/etc/profile.d/nix.sh \
  && nix-env -iA nixpkgs.haskell.compiler.ghc864 \
  && nix-env -iA cachix -f https://cachix.org/api/v1/install \
  && cachix use all-hies \
  && nix-env -iA selection --arg selector 'p: { inherit (p) ghc864; }' -f https://github.com/infinisil/all-hies/tarball/master
