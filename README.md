# KUDZU will slowly grow to cover all of your code

Kudzu is a library that throws test cases at your property tests until the code coverage no longer increases.

# WHY?

Property testing has no feedback loop, you randomly choose a number of test cases and hope for the best.

How do you know if your property tests were any good? The best feedback I know is to use [hpc](https://wiki.haskell.org/Haskell_program_coverage) and look at the pretty colored HTML output to see what code was exercised.

But wait, why do *I* have to look at the output? Isn't that why we have computers?

# HOW?

In Haskell, you can get [code coverage results](https://hackage.haskell.org/package/hpc/docs/Trace-Hpc-Reflect.html#v:examineTix) while your program is running!

# WHAT FEEDBACK LOOP?

The simplest feedback loop is to keep running random tests until new code coverage stops increasing.

# HOW DO I MAKE IT GO?

1. add kudzu to your test-suite depends,
2. import the [`Kudzu`] module
3. call the testUntil function of your choice
   1. [`testUntilSameQCMany`] or [`testUntilSameQC`]
   2. [`testUntilSameHHMany`] or [`testUntilSameHH`]
   3. [`testUntilSameLCMany`] or [`testUntilsameLC`]
4. run with `cabal test --enable-coverage`

# TELL ME MORE

The best write up of this idea is [Random Test Generation, Coverage Based](https://danluu.com/testing/).

# TODO

- [x] support HedgeHog
- [x] support QuickCheck
- [x] support LeanCheck
- [ ] figure out how to use Kudzu on Kudzu without looping forever

# EXAMPLE

You can see kudzu in use in the [tests](https://github.com/shapr/takedouble/blob/main/test/Main.hs) for [takedouble](https://github.com/shapr/takedouble/)

[`Kudzu`]:               https://hackage.haskell.org/package/kudzu/docs/Kudzu.html
[`testUntilSameQC`]:     https://hackage.haskell.org/package/kudzu/docs/Kudzu.html#v:testUntilSameQC
[`testUntilSameQCMany`]: https://hackage.haskell.org/package/kudzu/docs/Kudzu.html#v:testUntilSameQCMany
[`testUntilSameHH`]:     https://hackage.haskell.org/package/kudzu/docs/Kudzu.html#v:testUntilSameHH
[`testUntilSameHHMany`]: https://hackage.haskell.org/package/kudzu/docs/Kudzu.html#v:testUntilSameHHMany
[`testUntilSameLC`]:     https://hackage.haskell.org/package/kudzu/docs/Kudzu.html#v:testUntilSameLC
[`testUntilSameLCMany`]: https://hackage.haskell.org/package/kudzu/docs/Kudzu.html#v:testUntilSameLCMany
