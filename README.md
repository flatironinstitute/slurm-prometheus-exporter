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
  -N          --nodelist        include job node list instead of label by default (will increase prometheus database size)
  -c CLUSTER  --report=CLUSTER  include sreport data from CLUSTER by default (may be repeated)
  -d SECs     --delay=SECs      offset report data by SEC seconds to avoid rollup discontinuities
  -o          --open-metrics    export in OpenMetrics format (rather than prometheus text)
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
- `nodes`: the first feature set on the majority of nodes in the job (only when included in /metrics), or the hostlist (if enabled)
- `jobid`: job number, if enabled

Optional query parameters:

- `jobids`: enables (non-empty) or disables (empty) including jobid (overrides `-j`)
- `nodelist`: enables (non-empty) or disables (empty) replacing nodes with list of node names (overrides `-N`)

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
- `start`: start time for query range, defaults to `year` (metrics are returned as counters, so you should keep this value relatively fixed)
- `end`: end time for query range, defaults to `hour` (queries are split into whole days + partial day as slurmdbd/mysql is much faster at these)
- `historical`: if true, reports every value over the whole time range, either daily or hourly (if `hourly` is true), which can be used (with `-O`) to produce backfilling data

Timestamps may be specified as:

- `now`
- `hour`: top of the current hour
- `day`: midnight this morning
- `month`: first of this month
- `year`: Jan 1 this year
- YYYY-MM-DD[THH:MM[:SS]]

Although these values are reported as counters, in practice, slurm reporting data may sometimes exhibit non-monotonicity and decrease on later reports.
This may be due to rollup corrections or updates after jobs finish, and we have seen some values decrease after multiple days.
(On retroactive reporting, these issues disappear.)
You use the -d option to try to work around these issues (shifts the concept of current time back by the given amount, so you're only looking at data after some time), though we have not found this to be a complete fix.

slurmdb is also often much faster at producing reports that cover only whole days, and so queries are split into initial whole-day segment and partial day, and cached for performance.

## Dashboards

There are some grafana dashboards we use with this included in [grafana](grafana/).
