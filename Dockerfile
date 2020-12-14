FROM ubuntu:bionic
ARG SLURM_VERSION=18.08.9
ADD https://download.schedmd.com/slurm/slurm-$SLURM_VERSION.tar.bz2 /tmp
ADD https://get.haskellstack.org/ /tmp/getstack
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y python libmunge-dev && \
    sh /tmp/getstack && \
    cd /tmp && tar xf slurm-$SLURM_VERSION.tar.bz2 && \
      cd slurm-$SLURM_VERSION && \
      ./configure --prefix=/usr && \
      make && \
      make install && \
    rm -rf /var/lib/apt/lists/* /tmp/slurm*

RUN useradd -m run
USER run
WORKDIR /home/run
COPY --chown=run stack.yaml *.cabal Setup.hs ./
RUN stack build --dependencies-only
COPY --chown=run *.hs README.md docker-run ./
COPY --chown=run Slurm ./Slurm
RUN stack install --flag=slurm-prometheus-exporter:-pkgconfig --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib

USER root
RUN useradd -u 450 slurm
USER run
ENV SLURM_CONF=/etc/slurm/slurm.conf
EXPOSE 8090
ENTRYPOINT ["/home/run/docker-run"]
