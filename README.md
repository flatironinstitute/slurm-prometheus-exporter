# Slurm Prometheus Exporter

There at least [one](https://github.com/vpenso/prometheus-slurm-exporter) existing Prometheus exporter for slurm that works perfectly well.
However, it doesn't produce much data about jobs or nodes.
This aims to provide a bit more information efficiently.

This exports a variety of metrics from slurm to prometheus about running jobs and node allocations.
It can run anywhere slurm commands work as any user.

## Setup

1. Install slurm headers and library (tested with 18.08, 20.02)
1. Install Haskell, ideally [stack](https://docs.haskellstack.org/en/stable/README/)
1. If you have the `slurm.pc` pkg-config file (usually included with rpm installs of slurm):
    - Make sure it's discoverable by `pkg-config slurm` (you may have to set `PKG_CONFIG_PATH=$SLURM/lib/pkgconfig`) 
    - `stack install`
1. Otherwise, disable pkgconfig and manually specify the location of slurm:
    - `stack install --flag slurm-prometheus-exporter:-pkgconfig --extra-lib-dirs=...slurm/lib --extra-include-dirs=...slurm/include`
1. Run `slurm-exporter` anywhere you can run `squeue` (no need to be root or anything)
1. Point prometheus to `http://HOST:8090/metrics` (or `/stats`, `/nodes`, etc.), probably with a reduced scrape interval like 5m

## Usage

```
Usage: slurm-exporter [OPTIONS]

  -p PORT     --port=PORT       listen on port [8090]
  -r          --reasons         include node drain reasons by default (may increase prometheus database size)
  -j          --jobids          include job ids by default (may increase prometheus database size)
  -c CLUSTER  --report=CLUSTER  include sreport data from CLUSTER by default (may be repeated)
```

The following endpoints are available:

### /metrics

Includes everything below, unless otherwise specified.

### /stats

General slurm statistics, similar to `sdiag` output.

### /nodes

Node allocation information.
Node data is labeled by:

- `state`: `alloc` (anything running on the node), `drain` (only if not set for reboot), `down`, `resv`, `free` (anything else)
- `nodes`: first feature set on the node

Optional query parameters:

- `reasons`: enables (non-empty) or disables (empty) including drain reasons in state labels (overrides `-r`)

### /jobs

Running job allocation information
Job data is separated by the labels:

- `state`: `running` or `pending` (other jobs are ignored)
- `account`
- `partition`
- `user`: resolved from UIDs locally
- `nodes`: the first feature set on the majority of nodes in the job (only when included in /metrics)
- `jobid`: job number, if enabled

Optional query parameters:

- `jobids`: enables (non-empty) or disables (empty) including jobid (overrides `-j`)

### /report

Historical usage information. like `sreport user top`.
Only included in /metrics if `-c` is specified, or with `/metrics?report=CLUSTER`.
Report data is labeled by:

- `cluster`
- `account` (only first if there are multiple)
- `user`
- `tres`: `cpu`, `gpu/gres`, etc.

Optional query parameters:

- `cluster` (`report` with /metrics): name of cluster to limit report to, may be repeated, defaults to all clusters
- `start`: start time for query range, defaults to `year`
- `end`: end time for query range, defaults to `now` (XXX currently `day` for performance reasons)

Timestamps may be specified as:

- `now`
- `day`: midnight this morning
- `month`: first of this month
- `year`: Jan 1 this year
- YYYY-MM-DD[THH:MM[:SS]]

Note that slurmdb is much faster at producing reports that cover only whole days.

## Dashboards

There are some grafana dashboards we use with this included in [grafana](grafana/).
