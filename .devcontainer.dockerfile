FROM ubuntu:20.04

USER root

# Configure apt and install packages
RUN apt-get update \
    && apt-get -y install \
        apt-utils \
        git \
        curl \
        xz-utils

# Install Nix
RUN addgroup --system nixbld \
  && adduser root nixbld \
  && for i in $(seq 1 30); do useradd -ms /bin/bash nixbld$i &&  adduser nixbld$i nixbld; done \
  && mkdir -m 0755 /nix && chown root /nix \
  && mkdir -p /etc/nix \
  && echo 'sandbox = false' > /etc/nix/nix.conf \
  && echo 'substituters = https://cache.nixos.org https://hydra.iohk.io' >> /etc/nix/nix.conf \
  && echo 'trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' >> /etc/nix/nix.conf

# Install Nix
CMD /bin/bash -l
USER root
ENV USER root
WORKDIR /home/root

RUN touch .bash_profile \
 && curl https://nixos.org/releases/nix/nix-2.3.8/install | sh

RUN echo '. /root/.nix-profile/etc/profile.d/nix.sh' >> /root/.bashrc

RUN mkdir suppoort
COPY nix nix
COPY support/fp-course.cabal support/cabal.project support/

RUN . /root/.nix-profile/etc/profile.d/nix.sh \
  && nix-env -f nix/shell.nix -iA buildInputs \
  && nix-env -f nix/shell.nix -iA nativeBuildInputs
