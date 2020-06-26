# Slurm Prometheus Exporter

There at least [one](https://github.com/vpenso/prometheus-slurm-exporter) existing Prometheus exporter for slurm that works perfectly well.
However, it doesn't produce much data about jobs or nodes.
This aims to provide a bit more information efficiently.

This exports a variety of metrics from slurm to prometheus about running jobs and node allocations.
It can run anywhere slurm commands work as any user.
Job data is separated by the labels:

- `state`: `running` or `pending` (other jobs are ignored)
- `account`
- `partition`
- `user`: resolved from UIDs locally
- `nodes`: the first feature set on the majority of nodes in the job

Node data is labeled by:

- `state`: `alloc` (anything running on the node), `drain` (only if not set for reboot), `down`, `resv`, `free` (anything else)
- `nodes`: first feature set on the node

## Setup

1. Install slurm headers and library so that it's discoverable by `pkg-config slurm` (you may have to set `PKG_CONFIG_PATH=$SLURM/lib/pkgconfig`) (only tested with 18.08)
1. Install Haskell, ideally [stack](https://docs.haskellstack.org/en/stable/README/)
1. `stack install`
1. Run `slurm-exporter` anywhere you can run `squeue` (no need to be root or anything)
1. Point prometheus to `http://HOST:8090/metrics` (or `/stats`, `/nodes`, `/jobs` for subsets), probably with a reduced scrape interval like 5m.
