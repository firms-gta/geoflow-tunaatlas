# Deployment

How to run the workflow outside a developer machine. For local runs, see:

* [RUNNING.md](RUNNING.md) — processing only, with `docker run`;
* [DOCKER_COMPOSE.md](DOCKER_COMPOSE.md) — processing only, with `compose.workflow.yml`;
* [compose/README.md](../compose/README.md) — processing + database + GeoServer/GeoNetwork/Zenodo.

## Prerequisites

* Docker Engine with Docker Compose v2;
* about 30 GB of free disk space for the image, raw inputs and job outputs;
* a writable results path;
* network access only to pull the image, download the Zenodo archive the first
  time, or publish to the database/services;
* an immutable image tag (`sha-<commit>`) or digest for production runs.

## SSP Cloud (Kubernetes)

The workflow is a finite batch process, so a Kubernetes `Job` fits better than
a permanently running service.

> The Job template (`deploy/ssp-cloud-job.yaml`) is **not in the repository
> yet**. The steps below describe what it must contain.

1. Use the image `ghcr.io/firms-gta/gta-workflow` with an immutable tag or
   digest (the package must be public, or the cluster must have pull
   credentials).
2. Mount a persistent volume claim on `/home/rstudio/geoflow-tunaatlas/jobs`
   (outputs), `/cache` (download cache) and, for local inputs,
   `/home/rstudio/geoflow-tunaatlas/data/GTA_2026`.
3. Set `GTA_STEPS` and the input variables (`GTA_DATA_SOURCE`, `GTA_DOI`,
   `GTA_DOI_FILE` or `GTA_DATA_PATH`), as in [RUNNING_ADVANCED.md](RUNNING_ADVANCED.md).
4. Store database or Zenodo credentials in a Kubernetes Secret, never in the
   image or the Job file.
5. Adjust CPU and memory after a representative test.

Typical commands from an SSP Cloud terminal:

```bash
kubectl apply -f deploy/ssp-cloud-job.yaml
kubectl logs -f job/gta-workflow
kubectl delete job gta-workflow   # then apply again to relaunch with new parameters
```

SSP Cloud is based on the Onyxia catalogue; interface labels change over time.
See the [SSP Cloud documentation](https://docs.sspcloud.fr/) and the
[Onyxia documentation](https://docs.onyxia.sh/).

## Resource sizing

Suggested starting values: 2 CPUs and 8 GiB requested, limits of 8 CPUs and
32 GiB. They are not validated figures: set production values from the peak
memory and duration of a representative raw-data to Level 2 run. Report
rendering may need a different memory profile.

## Network

| Operation | Network |
| --- | --- |
| Local directory or cached archive | none (`--network none` possible) |
| First Zenodo download | outbound HTTPS to zenodo.org |
| Database / GeoServer / GeoNetwork publication | access to those endpoints |
| Zenodo publication | outbound HTTPS to (sandbox.)zenodo.org |
| Building the image | R repositories and GitHub |

## Rollback

Every image published by the CI has an immutable `sha-<commit>` tag. To roll
back, rerun with the previous tag. Job outputs are not rolled back: use a
separate output directory per run, or keep snapshots.
