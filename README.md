# coshan

## About

_Compute Shader Analyzer_ is a tool for static verification
of data dependency resolution and manually inserted wait states in AMD GCN/CDNA kernels. I developed it as a part of my [bachelor's thesis](https://github.com/timlathy/itmo-bachelor-thesis).

## Prerequisites

1. [Stack](https://docs.haskellstack.org/en/stable/README/)
2. LLVM runtime library (`libLLVM-11.so`; `libLLVM-12.so` should work as well with some minimal changes to `package.yaml`)

When using [HLS](https://github.com/haskell/haskell-language-server) for development:

```
stack install hspec-discover
```

## Running Automated Tests

```
stack test
```

## Usage Examples

In the following examples, substitute `sum_cols.co` with the path to the target AMDHSA code object (AMDPAL code objects are not supported).

1. Inspect the shader's control flow graph:

<pre>
$ stack run -- sum_cols.co --disasm
bb0: // predecessors: none (program start), exit: bb1
s_load_dwordx4 s[8:11], s[0:1], 0                                                                    // 0000000000000000: c00a0200 00000000
s_load_dwordx4 s[12:15], s[0:1], 16                                                                  // 0000000000000008: c00a0300 00000010
s_load_dword s6, s[0:1], 32                                                                          // 0000000000000010: c0020180 00000020
s_load_dword s7, s[0:1], 36                                                                          // 0000000000000018: c00201c0 00000024
s_mov_b32 s16, 0                                                                                     // 0000000000000020: be900080
v_mov_b32_e32 v3, 0                                                                                  // 0000000000000024: 7e060280
v_mov_b32_e32 v4, 0                                                                                  // 0000000000000028: 7e080280
s_waitcnt vmcnt(0), expcnt(0), lgkmcnt(0)                                                            // 000000000000002c: bf8c0000

bb1: // predecessors: bb0, bb1, exit: conditional jump to bb1 or bb2
v_add_f32_e32 v3, v3, v4                                                                             // 0000000000000030: 02060903
s_mul_i32 s17, s16, s7                                                                               // 0000000000000034: 92110710
s_lshl_b32 s17, s17, 2                                                                               // 0000000000000038: 8e118211
buffer_load_dword v4, v0, s[8:11], s17, idxen                                                        // 000000000000003c: e0502000 11020400
s_add_u32 s16, s16, 1                                                                                // 0000000000000044: 80108110
s_cmp_eq_u32 s16, s6                                                                                 // 0000000000000048: bf060610
s_cbranch_scc0 65528                                                                                 // 000000000000004c: bf84fff8

bb2: // predecessors: bb1, exit: none (program end)
buffer_store_dword v3, v0, s[12:15], 0, idxen                                                        // 0000000000000050: e0702000 80030300
s_endpgm                                                                                             // 0000000000000058: bf810000
</pre>

2. Verify `s_waitcnt` and `s_nop` instructions in the shader:

<pre>
$ stack run -- sum_cols.co
<b>[ OK ] Manually Inserted Wait States (s_nop)</b>

<b>[ 1/1 ] Data Dependency Resolution (s_waitcnt)</b>
<b>Problem:</b> Missing <b>s_waitcnt vmcnt(0)</b>
<b>Explanation:</b> Source register v4 is read from memory. The operation is complete when the counter reaches 0 because there are 0 operations enqueued after it.
<b>Location:</b>
              2c s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
              <b>30 v_add_f32_e32 v3, v3, v4</b>
              34 s_mul_i32 s17, s16, s7
<b>Memory operation:</b>
              38 s_lshl_b32 s17, s17, 2
              <b>3c buffer_load_dword v4, v0, s[8:11], s17 idxen</b>
              44 s_add_u32 s16, s16, 1
</pre>

## Known Limiations

1. Verification of _manually inserted wait states_ is implemented for just one case, out of 16 cases described in the ISA manual. The code located in `src/Coshan/Analysis/WaitStateHazard.hs` is intended to be fairly extendable, however, so if additional checks are needed, they can be added by copying the definition of `rwLaneMatcher` (~10 lines of code) and adjusting the criteria for dependent instructions.

2. Verification of _data dependency resolution_ does not track `expcnt` events. There are likely to be other false negatives as well. This is not so much an inherent limitation of the analysis algorithm, but rather a consequence of how little information I managed to find on the exact behavior of the hardware. When more test cases become available, I'll gladly update the code.
