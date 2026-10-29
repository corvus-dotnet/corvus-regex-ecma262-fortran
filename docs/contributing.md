# Contributing

This guide is for someone who wants to build the module, run its tests and gates, and open a pull request. It covers
the tools, the build step by step, the gates every change must pass, and how the module is released. The
[design document](design.md) describes what is underneath.

## 1. Tools

| Tool | Needed for | Version |
|---|---|---|
| A Fortran compiler | Everything | gfortran 11 or later, or Intel ifx |
| fpm or CMake | The build and the tests | fpm 0.13; CMake 3.16 or later (the getting-started programs run under CMake) |
| The C library | The module links it | The release CI names (`CRE_VERSION`), unpacked from its archive for the platform |
| Git | The repository and the test data | Any recent version |
| PowerShell 7 | The API reference's generator | 7.4 or later |
| Node.js | The test data for other versions of Unicode and for ECMAScript 2020, and the V8 rows of the harness | 24 (the V8 it carries is the oracle) |

- **Linux:** gfortran and CMake from the distribution; fpm from
  [its releases](https://github.com/fortran-lang/fpm/releases); PowerShell from Microsoft's feed; Node.js 24.
- **macOS:** `brew install gcc cmake fpm powershell node@24` (gfortran comes with `gcc`).
- **Windows:** Intel's oneAPI Fortran (ifx) with the Visual Studio Build Tools, or gfortran from MSYS2; CMake,
  PowerShell and Node.js from winget.

Unpack the C library's archive for the platform from
[its release](https://github.com/corvus-dotnet/corvus-regex-ecma262-rs/releases), and put its `lib` directory on the
linker's path (`LIBRARY_PATH`) and the loader's (`LD_LIBRARY_PATH`), or name it to CMake in `CMAKE_PREFIX_PATH`.

## 2. Getting the sources

```
git clone https://github.com/corvus-dotnet/corvus-regex-ecma262-fortran.git
git clone https://github.com/corvus-dotnet/ecma262-regex-test-data.git   # beside it: the test data
cd corvus-regex-ecma262-fortran
```

The test data is the oracle for the engine; the commands below assume it at `../ecma262-regex-test-data`.

## 3. Building and testing, step by step

Every command runs from the repository root.

1. **fpm.** `fpm test --flag "-Wall -Wextra -std=f2018 -fimplicit-none -Werror -g -fcheck=all"` builds the module and
   runs the API test; `fpm run --example basic` runs the README's example.
2. **CMake.**

   ```
   cmake -S . -B build -DCMAKE_PREFIX_PATH=<the C library> -DCMAKE_BUILD_TYPE=Release
   cmake --build build && ctest --test-dir build --output-on-failure
   ```

   runs the API test, the README's example and each program of the getting-started page.
3. **The version.** `fpm.toml`, `binding_version`, `CMakeLists.txt` and the API test name one version, which CI checks.
4. **The API reference.** After a change to the module's public declarations or their comments, run
   `pwsh tools/docs/gen-api.ps1`; `pwsh tools/docs/gen-api.ps1 -Check` must pass.
5. **The test data.** From `tools/dataset`: `fpm run --profile release -- ../../../ecma262-regex-test-data/data
   wrong-cases.tsv` must report 0 wrong, 0 refused and 0 at a limit. For the other versions of Unicode and for
   ECMAScript 2020, write the data with its own scripts (`unicode.js apply`, `edition.js apply` in the data repository)
   and run it with `--unicode 13` or `--edition es2020`, as CI does.

## 4. The C library

The module is built on one release of the C library: CI names its version (`CRE_VERSION`), and the Dockerfile the
version and each architecture's archive's SHA-256 (`CRE_SHA256_X86_64`, `CRE_SHA256_AARCH64`). Moving to a new release
moves all of them. A fix to the engine's answers is made in
[the C library's repository](https://github.com/corvus-dotnet/corvus-regex-ecma262-rs) and taken here by moving the
release.

## 5. Benchmarks

`tools/bench/run.sh <workloads.json> <texts directory> <out.json>` runs this module, POSIX `regex.h` and V8 over the
Industry workloads; the script lists what it needs and its settings. The published figures are on
[the benchmark page](https://corvus-oss.org/corvus-regex-ecma262-benchmarks/), which runs this repository's harness
image each night beside every other engine; a run on another machine is comparable with itself, never with the page.

## 6. What a pull request needs

- **The gates of section 3** passing on the change as pushed, with gfortran and ifx.
- **A fix to the engine's answers in the C library's repository**, with its case in the test data first.
- **A failing test first** for a bug in the module: the test that reproduces it, then the fix.
- **The design document kept current**, describing the module as it is.
- **`!>` comments** on every public declaration and procedure, since the API reference is generated from them.
- **The getting-started page** kept true: its programs are built and run by ctest.

### Conventions

- Fortran 2018, `implicit none`, clean under gfortran's `-Wall -Wextra -std=f2018 -Werror`, and building with ifx.
- A procedure that can fail takes `stat` and `errmsg`, and stops the program only when `stat` is absent.

## 7. Releases

A release is made by hand, as a GitHub release tagged `v<version>`, the version in `fpm.toml` (and in
`binding_version`, `CMakeLists.txt` and the API test). An fpm project depends on the tag; the release publishes the
harness image with its tag. The documentation site takes the published documents from the latest release, so a change
to them ships with the next one. A change to the benchmark harness alone is a harness release (`gh release create
harness-<version>.<n> --latest=false`), which publishes the harness image.

`main` is protected by a ruleset: changes arrive by pull request, and the `CI gate` and `Harness image` checks must be
green on the branch as it will merge.
