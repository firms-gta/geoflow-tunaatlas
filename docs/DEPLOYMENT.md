# Local and SSP Cloud deployment

## Prerequisites

- Docker Engine with BuildKit and Docker Compose v2 for local runs;
- enough disk space for the R image, raw inputs, and job outputs; --> at least 10GB
- a writable results path;
- network access only when building the image or acquiring a DOI for the first
  time;
- an immutable image tag or digest for production execution.

## Local deployment with Docker Compose

Create the runtime directories and a local environment file:

```bash
mkdir -p runtime/input runtime/extracted runtime/jobs runtime/cache

cat > .env.runtime <<EOF
GTA_UID=$(id -u)
GTA_GID=$(id -g)
GTA_STEPS=rawdata
GTA_DATA_SOURCE=volume_dir
GTA_INPUT_DIR=/absolute/path/to/all_raw_data_GTA
GTA_JOBS_DIR=$PWD/runtime/jobs
GTA_CACHE_DIR=$PWD/runtime/cache
GTA_EXTRACTED_DIR=$PWD/runtime/extracted
EOF
```

Build and run:

```bash
docker compose --env-file .env.runtime -f compose.workflow.yml build
docker compose --env-file .env.runtime -f compose.workflow.yml run --rm gta-workflow
```

Do not commit `.env.runtime`; it records machine-specific paths and may later
contain sensitive values.

### DOI run

```bash
cat > .env.runtime <<EOF
GTA_UID=$(id -u)
GTA_GID=$(id -g)
GTA_STEPS=rawdata
GTA_DATA_SOURCE=doi
GTA_DOI=10.5281/zenodo.20834708
GTA_JOBS_DIR=$PWD/runtime/jobs
GTA_CACHE_DIR=$PWD/runtime/cache
GTA_EXTRACTED_DIR=$PWD/runtime/extracted
EOF

docker compose --env-file .env.runtime -f compose.workflow.yml run --rm gta-workflow
```

### Partial relaunch

Change `GTA_STEPS` and, for summary or report-only runs, add the relevant
existing job paths:

```bash
GTA_STEPS=summaries \
GTA_TUNAATLAS_LEVEL0_CATCH=jobs/LEVEL0_JOB \
GTA_TUNAATLAS_LEVEL1_CATCH=jobs/LEVEL1_JOB \
GTA_TUNAATLAS_LEVEL2_CATCH=jobs/LEVEL2_JOB \
docker compose --env-file .env.runtime -f compose.workflow.yml run --rm gta-workflow
```

Compose forwards all documented existing-job path variables.

## Build the reporting image

```bash
docker build \
  -f docker/Dockerfile.workflow \
  -t gta-workflow:latest \
  .

docker build \
  -f docker/Dockerfile.reporting \
  --build-arg BASE_IMAGE=gta-workflow:latest \
  -t gta-reporting:latest \
  .
```

Use `gta-reporting` only for stages that require the PDF/R Markdown toolchain.

## SSP Cloud deployment model

The workflow is a finite batch process, so a Kubernetes `Job` is a better fit
than a permanently running web service. The supplied
`deploy/ssp-cloud-job.yaml` is a portable template for an SSP Cloud namespace.

Before submitting it:

1. push `gta-workflow` to a registry readable from SSP Cloud;
2. replace the image with an immutable tag or digest;
3. replace `REPLACE_WITH_SSP_CLOUD_PVC` with the persistent volume claim visible
   in the target namespace;
4. adjust CPU and memory after a representative test;
5. set `GTA_STEPS` and the input variables;
6. use a Kubernetes Secret for database credentials if publication is required.

Apply and monitor from an SSP Cloud terminal that has access to the namespace:

```bash
kubectl apply -f deploy/ssp-cloud-job.yaml
kubectl logs -f job/gta-workflow
kubectl get job gta-workflow
```

Inspect the persistent volume after completion and retain the logs with the
validation record. Delete and recreate the Job to relaunch it with changed
parameters:

```bash
kubectl delete job gta-workflow
kubectl apply -f deploy/ssp-cloud-job.yaml
```

The exact catalogue labels and storage names exposed by the SSP Cloud interface
can change. The stable contract is the Kubernetes Job specification, container
environment variables, and mounted paths documented here. SSP Cloud is based on
the Onyxia service catalogue; see the [SSP Cloud documentation](https://docs.sspcloud.fr/)
and [Onyxia documentation](https://docs.onyxia.sh/) for the current interface.

## Resource sizing

The template starts with 2 requested CPUs, 8 GiB requested memory, and limits of
8 CPUs and 32 GiB. These are starting values, not validated capacity figures.
Use the peak memory and duration recorded by a representative raw-data and Level
2 run to set production values. Report rendering may require a different memory
profile from data processing.

## Network policy

- Local directory and cached archive runs can use `--network none`.
- DOI acquisition requires outbound HTTPS to Zenodo until the archive is cached.
- Database publication requires access to the authorised database endpoint.
- Image builds require access to the R repositories and source repositories
  referenced by `renv.lock`.

## Operational rollback

Images should be tagged with both a readable release tag and the source commit
SHA. A failed deployment is rolled back by recreating the Job with the previous
immutable image tag. Job outputs are not rolled back automatically and must use
separate run directories or retained snapshots.
