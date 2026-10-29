# The Fortran benchmark harness as an image: this binding's tools (the data-set runner and the harness, built with fpm
# against the C library from its release archive, with fortran-regex at its pinned release) and Node 24 for the V8
# rows, with tools/bench/run.sh as the entry point. Built from the repository's root:
#
#   docker build -t corvus-regex-ecma262-fortran .
#   docker run --rm -v <workloads.json>:/workloads.json:ro -v <texts dir>:/texts:ro -v <out dir>:/out <image>
#
# which writes /out/industry.json, the run's data file in the results page's shape. The ECMA-262 test data is cloned
# at depth 1 inside the container unless its data/ directory is mounted at /data. JOB=short, SECONDS_EACH, PASSES,
# CORES, SETTLE and RUN_URL pass through (see tools/bench/run.sh). The central benchmark site runs this image each
# night; .github/workflows/harness-image.yml builds, smoke-runs and publishes it.
FROM node:24.22.1-bookworm-slim@sha256:55fb5f5a527ea2c5c4a95f963e48ef90aa38c7b346054fdc8ee9957a2b55d205 AS node

FROM gcc:14@sha256:db19db55a3bd1f836212a76854c4f6c7c4ccb0c9a9426dbf542e3505bc8cdfec
RUN apt-get update && apt-get install -y --no-install-recommends python3 git util-linux ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*
COPY --from=node /usr/local/bin/node /usr/local/bin/node
# fpm: the release's binary on x86_64; on arm64, for which fpm publishes none, built from the release's single source
# file as fpm's own bootstrap builds it. Each is checked against its SHA-256.
RUN case "$(uname -m)" in \
      x86_64) curl -fsSL -o /usr/local/bin/fpm https://github.com/fortran-lang/fpm/releases/download/v0.13.0/fpm-0.13.0-linux-x86_64-gcc-12 \
        && echo "2e50a8649ac4136b9cf6b63c5d1b01adf26c02eedc18f32e16261c0ce655d46d  /usr/local/bin/fpm" | sha256sum -c - ;; \
      aarch64) curl -fsSL -o /tmp/fpm.F90 https://github.com/fortran-lang/fpm/releases/download/v0.13.0/fpm-0.13.0.F90 \
        && echo "001cff6cf1145f215baa0888ba27acc06cea30a89b79d1a8be97db3fffbc8cd2  /tmp/fpm.F90" | sha256sum -c - \
        && gfortran -O1 -o /usr/local/bin/fpm /tmp/fpm.F90 && rm /tmp/fpm.F90 ;; \
      *) echo "no fpm is pinned for $(uname -m)"; exit 1 ;; \
    esac \
    && chmod +x /usr/local/bin/fpm
# The C library: the release's archive for the architecture being built, checked against its hash.
ARG CRE_VERSION=0.2.5
ARG CRE_SHA256_X86_64=d5568cdb5e5436c0a0b83c1616aabb1192198def688a8bd36bb5fb6cc8137ef1
ARG CRE_SHA256_AARCH64=9ab1c0659377e8da919fcb425b6975492a518ed11c0e1ae3129d5cab7172d05a
RUN case "$(uname -m)" in x86_64) arch=x86_64 sum=$CRE_SHA256_X86_64 ;; aarch64) arch=aarch64 sum=$CRE_SHA256_AARCH64 ;; \
        *) echo "no archive of the C library is pinned for $(uname -m)"; exit 1 ;; esac \
    && curl -sSfL -o /tmp/cre.tar.gz "https://github.com/corvus-dotnet/corvus-regex-ecma262-rs/releases/download/v${CRE_VERSION}/corvus-regex-ecma262-${CRE_VERSION}-$arch-unknown-linux-gnu.tar.gz" \
    && echo "$sum  /tmp/cre.tar.gz" | sha256sum -c - \
    && mkdir -p /tmp/cre && tar -xzf /tmp/cre.tar.gz -C /tmp/cre --strip-components 1 \
    && cp -r /tmp/cre/include/. /usr/local/include/ && cp -r /tmp/cre/lib/. /usr/local/lib/ && ldconfig \
    && rm -rf /tmp/cre /tmp/cre.tar.gz
WORKDIR /harness
COPY . .
RUN (cd tools/dataset && fpm build --profile release) && (cd tools/bench && fpm build --profile release)
# The commit the image is built from, for the run's record (the image has no .git).
ARG COMMIT=""
ENV CORVUS_COMMIT=$COMMIT
ENTRYPOINT ["/harness/tools/bench/container-entry.sh"]
