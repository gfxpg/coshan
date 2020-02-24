# Exploring generated IR and assembly

(*An AMD GPU is not required to run the compiler*)

```sh
docker run -it --rm rocm/rocm-terminal
```

Inside the container:

```sh
KMDUMPLLVM=1 KMDUMPISA=1 /opt/rocm/bin/hipcc --genco --targets=gfx900 test.cpp
```

## Viewing LLVM graphs

CFG:

```sh
opt -analyze -view-cfg dump-gfx900.opt.bc
```
