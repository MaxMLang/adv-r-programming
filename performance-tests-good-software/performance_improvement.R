# Exercise 01: Subset with Minimal Difference ---------------------------------------------------------------------
# In this exercise you are once again given a function that technically does the right thing, but it's doing so
# at an unjustifiably slow speed for inputs of larger sizes.
# Imagine a scenario where you have a large bag full of packets of gummy bears.
# Each packet has a varying number of gummy bears inside, and the range can be pretty large. You want to distribute
# packets of gummy bears among your `m` friends, and you want it to be as fair as possible.
# That means that the difference in numbers of gummy bears should be as small as possible, such that nobody feels
# treated unfairly. It's not about getting the most gummy bears, we just want to avoid yelling.
# For a given `set` of gummy bear packages, you want to find the subset of size `m` such that this subset has the
# minimum difference between the smallest and largest element in the subset.
#
# Function arguments:
# - `set`: Integer vector (with e.g. number `4` allowed rather than enforcing `4L`) of numbers of gummy bears
#          per packet. Must be positive numbers (1 or greater), and at least contain 3 elements.
# - `m`: Integer (again with e.g. `3` allowed) of the number of friends you want to distribute packets among.
#        If `set` is of length `n`, then `m` must obviously be at most `n - 1` large and at least 2.
# Neither argument has a default and you should, like in previous assignments, add appropriate checkmate assertions.
#
# Function output:
# A numeric vector of the subset of `set` of length `m` with the minimal difference between largest and smallest
# value, sorted in ascending order. If there are multiple valid subsets for which the minimal difference is equally
# minimal, you pick only one (see how the reference function does it).
# Tests for this function will use input `set`s where there is only one valid solution.
#
# Example output:
# ex01MinDiff(set = c(3, 5, 7, 3, 2, 8), m = 4)
# Return: c(2, 3, 3, 5)
#
# ex01MinDiff(set = c(5, 6, 4, 9, 3), m = 4)
# Return: c(3, 4, 5, 6)
#
# Ambiguous case:
# ex01MinDiff(set = c(3, 5, 7, 3, 2, 8), m = 5)
# Return: c(2, 3, 3, 5, 7)
# OR
# Return: c(3, 3, 5, 7, 8)
# are acceptable, but only one should be returned! Reference function would pick: c(2, 3, 3, 5, 7)
#
# Examples for invalid input:
# ex01MinDiff(set = c("A", "B", "A", "B"), m = 2) -> Assertion error, set is not interpretable as integer
# ex01MinDiff(set = c(1.5, 6.8, 3.2, 3.4), m = 2) -> Assertion error, set is also not integers
# ex01MinDiff(set = c(4, 7, 2, NA, 3), m = 2) -> Assertion error, set contains missing values
# ex01MinDiff(set = c(4, 5, 7, 2, 3), m = NA) -> Assertion error, m is missing
# ex01MinDiff(set = c(4, 5, 7, 2, 3), m = -1) -> Assertion error, m must be positive (and greater 2 anyway)
# ex01MinDiff(set = c(3, 5, 7, 3, 1), m = 5) -> Assertion error, m must me smaller than the length of the set
# ex01MinDiff(set = c(3, 1), m = 1) -> Assertion error, either because set must be at least of length 3 or because
#                                      m must be at least of length 2 (either assertion should catch it)
#
# Examining the reference function given in `ex01MinDiffReference`, you will notice that its code
# already looks pretty minimal, it neither uses for-loops nor requires pre-allocation.
# It even uses that `vapply` function we always tell you to use. And `which.min` is pretty fast.
# To improve it, you will need to think about the problem you're trying to solve, and how to solve it more efficiently.
# Hint: Listing all possible subsets with `combn` seems excessive. Brute-force is rarely the fastest option.
#
# Benchmark note:
# Input `set` will be at maximum of length 20, and `m` will be below 10. Since the reference function's runtime will
# explode for larger input `set`s, you probably also won't want to experiment with larger inputs.
# You will need to pass an improvement threshold of 500 - so that we can be somewhat certain you actually found
# a more efficient solution. From our testing, an improvement of ~1000 can be expected.
# Also note that the benchmark test will be skipped automatically if any of the previous tests has failed!

ex01MinDiffReference <- function(set, m) {
  # Reference function, no reason to modify this!
  # Get all possible subsets of size m
  allComb <- combn(set, m, simplify = FALSE)
  # Calculate differences for each subset
  setDiffs <- vapply(allComb, function(x) max(x) - min(x), numeric(1))
  # Get the first subset for which difference is minimal
  bestSet <- allComb[[which.min(setDiffs)]]
  # Return sorted subset
  sort(bestSet)
}

ex01MinDiff <- function(set, m) {
  assertIntegerish(set, lower = 1, any.missing = FALSE, min.len = 3)
  assertIntegerish(m, lower = 3, upper = length(set) - 1, any.missing = FALSE, len = 1)
  sorted <- sort(set)
  mindiff <- Inf

  for (i in seq_len(length(sorted) - (m -  1))) {
    res <- sorted[[i + (m - 1)]] - sorted[[i]]
    if (res < mindiff) {
      mindiff <- res
      start.ind <- i
      end.ind <- i + (m - 1)
    }
  }
  sorted[start.ind:end.ind]
}
