FROM centos:7
ARG SLURM_VERSION=21.08.8
ADD https://download.schedmd.com/slurm/slurm-$SLURM_VERSION.tar.bz2 /tmp
ADD https://get.haskellstack.org/ /tmp/getstack
RUN yum -y install epel-release && \
    yum -y install bzip2 zlib-devel munge-devel sssd-client && \
    sh /tmp/getstack && \
    useradd -u 450 slurm && \
    cd /tmp && tar xf slurm-$SLURM_VERSION.tar.bz2 && \
      cd slurm-$SLURM_VERSION && \
      sed -i '/^SUBDIRS /s/\<doc\>//' Makefile.* && \
      ./configure --prefix=/usr && \
      make && \
      make install && \
    ldconfig && \
    yum clean all && \
    rm -rf /tmp/slurm*

RUN useradd -m run && chmod 755 /home/run
USER run
WORKDIR /home/run
COPY --chown=run stack.yaml *.cabal Setup.hs ./
RUN stack build --dependencies-only
COPY --chown=run *.hs README.md docker-run ./
COPY --chown=run Slurm ./Slurm
RUN stack install --flag=slurm-prometheus-exporter:-pkgconfig --extra-lib-dirs=/usr/lib/slurm

EXPOSE 8090
ENV SLURM_CONF=/etc/slurm/slurm.conf LD_LIBRARY_PATH=/usr/lib/slurm
ENTRYPOINT ["/home/run/docker-run"]
