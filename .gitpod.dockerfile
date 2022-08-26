FROM gitpod/workspace-full

USER root

# Install Nix
RUN addgroup --system nixbld \
  && adduser gitpod nixbld \
  && for i in $(seq 1 30); do useradd -ms /bin/bash nixbld$i &&  adduser nixbld$i nixbld; done \
  && mkdir -m 0755 /nix && chown gitpod /nix \
  && mkdir -p /etc/nix \
  && echo 'sandbox = false' > /etc/nix/nix.conf \
  && echo 'substituters = https://cache.nixos.org https://hydra.iohk.io' >> /etc/nix/nix.conf \
  && echo 'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' >> /etc/nix/nix.conf

# Install Nix
CMD /bin/bash -l
USER gitpod
ENV USER gitpod
WORKDIR /home/gitpod

RUN touch .bash_profile \
 && curl https://nixos.org/releases/nix/nix-2.3.8/install | sh 

COPY nix nix
COPY support/fp-course.cabal support/cabal.project support/

RUN . /home/gitpod/.nix-profile/etc/profile.d/nix.sh \
  && nix-env -f nix/shell.nix -iA buildInputs \
  && nix-env -f nix/shell.nix -iA nativeBuildInputs
