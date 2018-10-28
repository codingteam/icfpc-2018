# Codingteam's Submission for ICFP Programming Contest 2018

## Build Prerequisites

You'll need [Haskell Stack][stack]. We had teammates running Linux and Windows,
so it should work fine on those platforms.

There are two helper scripts, one written in BASH and another in Perl, but they
aren't essential to problem-solving--they were only used to prepare submissions.

[stack]: https://docs.haskellstack.org/en/stable/README/

## How to Run

```shell
$ ./gentraces.sh prepare-submission problemsF.zip solutions.zip
```

where "problemsF.zip" is an archive of problems from the full round (as
downloaded from the contest webpage). This command will prepare a non-encrypted
archive called "solutions.zip".

Note: that script uses the latest iteration of the solver. For our submissions,
we used a more complicated approach: we ran current solver on all problems, then
merged the solutions with our previous submissions using "select.pl" script. The
latter picks solutions with the lowest energy (but it doesn't validate them, so
might also pick an invalid solution).

In case you're interested in the earlier versions of our solvers, we're
including a complete Git bundle with the whole history of the repository. You
can clone from it just like from an ordinary repository:

```shell
$ git clone history.bundle name-of-the-clone
```

## Approaches We Used

We implemented the naive strategy of building things layer by layer, line by
line, flipping into high harmonics when necessary. After that, most of the
effort was spent on optimizing this; for example, we started using GFill to fill
rectangular chunks of layers, otherwise our nanobot scanned it line-by-line and
filled three voxels at a time.

Our destruction strategy is the same, but the order is reversed: we start from
the top and move to the bottom.

Reconstruction problems are solved naively: first deconstruct, then construct.

## The Team

In randomized order:

- Friedrich von Never
- ulidtko
- Ilya Portnov ([report (in Russian)](https://iportnov.blogspot.com/2018/07/icfpc-2018.html))
- Akon32
- Alexander Batischev ([report (in Russian)](https://blog.debiania.in.ua/posts/2018-10-28-icfpc-2018.html))

Thank you for the great three days of fun!
