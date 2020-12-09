FROM ubuntu:20.04
ADD https://get.haskellstack.org/ /tmp/getstack
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y libslurm-dev slurm-wlm-basic-plugins pkg-config && \
    sh /tmp/getstack && \
    rm -rf /var/lib/apt/lists/*
RUN useradd -m run
USER run
WORKDIR /home/run
COPY --chown=run stack.yaml *.cabal Setup.hs ./
RUN stack build --dependencies-only
COPY --chown=run *.hs README.md ./
COPY --chown=run Slurm ./Slurm
RUN stack install --flag=slurm-prometheus-exporter:-pkgconfig
ENV SLURM_CONF=/etc/slurm/slurm.conf
EXPOSE 8090
ENTRYPOINT ["/home/run/.local/bin/slurm-exporter"]
