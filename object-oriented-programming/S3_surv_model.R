# In these exercises we will take a look at the S3 class system by creating a new generic function with a placeholder
# default method and then methods to do some work for us.
# For this purpose you are given two sets of model objects, which you can read in with the following code:
# (Do not modify the file paths - they are relative to the project root and "just work" when you're set up correctly).
# For the first portion:
exampleSfit <- readRDS("assets/exampleSfit.rds")
exampleSfitStrata <- readRDS("assets/exampleSfitStrata.rds")
# For the second portion:
exampleMfit <- readRDS("assets/exampleMfit.rds")
exampleMfitStrata <- readRDS("assets/exampleMfitStrata.rds")
# The first set of models has class "survfit", and the second set additionally has class "survfitms".
# Please note that you don't need to know anything about survival analysis or the meaning/interpretation of these
# models - all relevant information is given below for the task at hand. For the purposes of these exercises,
# they are simply some S3 object examples you might encounter in the real world - and have to deal with using
# R's S3 class system.
# For debugging and testing you may modify these objects by e.g. overwriting individual components, but please make
# sure to never modify the original `.rds` files so you can always return to a state with "correct" objects.
#
# Important note on function names for this sheet:
# Since part of the S3 class system is the correct naming of functions, you will only be given the name of the
# generic function and the specific class you are supposed to write a method for. Contrary to other exercises,
# you will therefore not be given an empty function to complete, but have to write the function from scratch.
# It is very important though that you stick to the name of the generic function as given in the
# first exercise so that automated tests work as intended.
#
# Additional note on tests:
# 1. The tests in this sheet will only be executed if the correctly named function exists.
# 2. The tests for these exercises will mostly be conducted using different objects which you will not
# have access to. To ensure test failures are still informative enough to let you adjust your functions accordingly,
# we have added additional messages to many tests. If it is still unclear what the issue is, please ask in the forum
# so others can benefit from any clarifications as well.

# Exercise 01: S3 Generic -----------------------------------------------------------------------------------------
# Our overall goal is going to be to extract useful information from these statistical model objects by tidying
# up the information stored within them. To do so, we want to have a generic function `tidyModel` that has specific
# methods for both sets of model objects (the ones of class "survfit" and the ones that also have class "survfitms").
# The first step is to create a generic function named `tidyModel` that takes an object `x` as argument.
# Generic functions also typically include dots (`...`) to pass further arguments to the dispatched methods,
# so you should include them as well so later methods can add additional arguments if required.
#
# Since this generic function alone is not going to be doing anything useful, you will need to add more methods
# to it before you can make sure that it behaves appropriately.

tidyModel <- function(x, ...) {
  UseMethod("tidyModel")
}


# Exercise 02: S3 Default Method ----------------------------------------------------------------------------------
# With your new generic, you now need to create a default method which is called whenever the input object
# is not of the specific class we can handle. Since there is no meaningful "default" behavior, we just throw an error:
#
#  `Can not tidy object of class 'XYZ'`
#
# This should be a regular error, not a checkmate assertion or a `print()`ed message, nor a `message()` of course.
# The 'XYZ' portion should be the first element reported by `class(x)` in single quotes.
#
# Since this method is only there as a safeguard to handle objects of classes you have not defined methods for, the
# only way to detect if it's working correctly is to try it on objects of classes you are not going to write methods
# for.
#
# Example behavior for an object of the wrong class:
# > tidyModel("hello")
# Error in <function call will be displayed here> :
#   Can not tidy object of class 'character'
#
# > tidyModel(list(2))
# Error in <function call will be displayed here> :
#   Can not tidy object of class 'list'

tidyModel.default <- function(x) {
  stop(sprintf("Can not tidy object of class '%s'", class(x)[[1]]))
}


# Exercise 03: Tidy "survfit" Model Object --------------------------------------------------------------------------
# The first method is meant to work on the objects `exampleSfit` and `exampleSfitStrata` defined above.
# Now that we have the simple boilerplate out of the way, we can start with the actual work.
# The models first two objects you're given above can be treated like `list` objects in R with regards to accessing
# elements, which lets you extract and reorganize their contents.
# We are interested in the elements "time" and "surv" for survival predictions at given time points in both cases.
# For `exampleSfitStrata` we also need to account for the "strata" element, which is not present in `exampleSfit`.
# Our goal is to create a tidy data.table that contains the `time`, `surv` and if present, the `strata` information,
# with the exact format described below.
# The "strata" referred to here are merely groupings by categorical/factor variables like `trt` ("treatment") or
# `sex` used within a model.
# (A very simplified yet sufficient statement for our purposes)
#
# Relevant elements of objects of class `survfit`:
# - `time`: Numeric vector of positive time points as observed in the data the model was fit on.
#           The length is equal to that of `surv` since the survival probability is estimated per time point.
# - `surv`: Numeric vector of estimated survival probabilities, possibly by `strata` if there are any.
# - `strata`: NULL if there are no strata (`exampleSfit`), or a named vector of integer counts, denoting the order
#             and number of elements of the `surv` vector belonging to the respective stratum.
#
# Using `exampleSfitStrata` as an example: The strata are "trt=1" and "trt=2", with n = 61 and n = 53 respectively.
# That means that the first 61 elements of the `surv` vector belong to stratum "trt=1" and the next 53 belong to
# stratum "trt=2".
# In general, you should not assume that there are exactly two strata, or that you know their names beforehand, meaning
# that your function should be general enough to deal with different strata names or e.g. three strata instead of two.
#
# Function inputs:
# Same as defined in the `tidyModel` generic.
#
# While in general this function is a class-specific method, so one might assume that it will only ever be called on
# `survfit` models that have the correct structure, you should still add a checkmate assertion to make sure the input
# object `x` really is of class "survfit", just to be on the safe side in case someone were to directly call your
# method on a different kind of object. If the object is of the correct class however, you can safely assume that it
# is "correct" in the sense that e.g. `time` never contains missing values or that the counts in `strata` sum up
# to the correct total N etc.
#
# Expected output:
# A data.table with columns in that order:
# - `time`: Numeric vector of time points, same values and order as in the `time` element.
# - `strata` (if applicable), `factor` with levels like in the `strata` element.
# - `surv`: Numeric, containing values from `surv`.
#
# Your function will be tested on different "survfit" models that have the same general structure, but of course
# will differ regarding specific values like `time` or `strata`.
#
# > tidyModel(exampleSfit)
#      time        surv
#     <num>       <num>
#  1:     1 0.985401460
#  2:     2 0.978102190
#  3:     3 0.970802920
#  4:     4 0.963503650
#  5:     7 0.941605839
#  ---
#  97:  467 0.036018043
#  98:  553 0.027013532
#  99:  587 0.018009021
# 100:  991 0.009004511
# 101:  999 0.000000000
#
# > tidyModel(exampleSfitStrata)
#      time strata       surv
#     <num> <fctr>      <num>
# 1:      3  trt=1 0.98550725
# 2:      4  trt=1 0.97101449
# 3:      7  trt=1 0.95652174
# 4:      8  trt=1 0.92753623
# 5:     10  trt=1 0.89855072
# ---
# 110:  389  trt=2 0.07318235
# 111:  467  trt=2 0.05488676
# 112:  587  trt=2 0.03659118
# 113:  991  trt=2 0.01829559
# 114:  999  trt=2 0.00000000
#
# Note: If you find that your function does not automatically print the data.table as you expect, refer to this
# answer on stackoverflow: https://stackoverflow.com/a/32989328/409362

# your code: tidyModel method for "survfit" object.

tidyModel.survfit <- function(x, ...) {
  assertClass(x, "survfit")
  if (is.null(x$strata)) {
    dt <- data.table(time = x$time, surv = x$surv)
    setorder(dt, cols = "time")
    dt
  } else {
    strata.long <- character()
    for (i in seq_along(x$strata)) {
      strata.long <- c(strata.long, rep(names(x$strata)[i], x$strata[i]))
    }
    dt <- data.table(time = x$time, strata = as.factor(strata.long), surv = x$surv)
    dt
  }
}

# Exercise 04: Tidy "survfitms" Model Object -------------------------------------------------------------------------
# This method is meant to work on the objects `exampleMfit` and `exampleMfitStrata` defined above.
# Here we are interested in the element `"pstate"`, which in both cases is a numeric matrix with 3 columns
# and either 268 (for `exampleMfit`) or 454 rows (for `exampleMfitStrata`).
# Our goal is to create a tidy data.table (format described below) that contains a `time` variable, the `state`
# information, the predictions from `pstate`, and if applicable the `strata` information.
#
# Relevant elements of objects of classes `survfitms` and `survfit`:
# - `time`: Numeric vector of unique time points as observed in the data the model was fit on. The length is equal
#           to the number of rows in `pstate`.
# - `pstate`: Predictions of a cumulative incidence function (CIF) for 3 different states, where
#             the columns correspond to the states in the next element (including their ordering) and the rows
#             correspond to time points as in `time`, with the same ordering.
# - `states`: Character vector of `states`. The `(s0)` state is the baseline and we ignore it, meaning we also
#             want to remove it from the output. In this case there are two other states,
#             but in general there could also be more.
# - `strata`: NULL if there are no strata (`exampleMfit`), or a named vector of integer counts, denoting the order
#             and number of rows of the `pstate` matrix belonging to the specified stratum.
#
# Using `exampleMfitStrata` as an example: The strata are "sex=F" and "sex=M", each with n = 227.
# That means that the first 227 rows of the `pstate` matrix belong to stratum "sex=F" and the next 227 rows belong to
# stratum "sex=M". In general, you should not assume that both counts are equal, or that there are exactly two strata.
#
# In general you should assume that:
# - There will either be no strata at all or strata for at least two distinct groups with non-zero counts.
# - The names of the strata or their counts will depend on the input object, so your function should automatically
#   handle varying names or counts!
# - There will be at least two states, but their names may be completely different than here.
# - The "baseline" state will always be called `(s0)`. We always want to exclude it from the output.
#
# Like in the previous exercise you should make sure the input object `x` really is of class "survfitms", just to be
# on the safe side in case someone were to directly call your method on a different kind of object. Once again, no
# specific assertions are required since we "trust" a "survfitms" model to be correct and internally consistent.
#
# Expected output:
# A data.table with columns in that order:
# - `time`: Numeric vector of time points, same values and order as in the `time` element.
# - `state`: `factor`, with levels like in the `states` element.
# - `strata` (if applicable), `factor` with levels like in the `strata` element.
# - `cif`: Numeric, containing values from `pstate`.
#
# Like in the following output:
# > tidyModel(exampleMfit)
#      time state         cif
#     <num> <fctr>       <num>
# 1:    1   pcm   0.000000000
# 2:    2   pcm   0.001446164
# 3:    3   pcm   0.001446164
# 4:    4   pcm   0.002169246
# 5:    5   pcm   0.002892329
# ---
# 532:  341 death 0.784208247
# 533:  350 death 0.784208247
# 534:  373 death 0.784208247
# 535:  394 death 0.784208247
# 536:  424 death 0.838708319
#
# > tidyModel(exampleMfitStrata)
#      strata  time state        cif
#      <fctr> <num> <fctr>      <num>
# 1:    sex=F    1   pcm 0.000000000
# 2:    sex=F    2   pcm 0.003174768
# 3:    sex=F    3   pcm 0.003174768
# 4:    sex=F    4   pcm 0.004762152
# 5:    sex=F    5   pcm 0.004762152
# ---
# 904:  sex=M  301 death 0.799436407
# 905:  sex=M  314 death 0.799436407
# 906:  sex=M  338 death 0.799436407
# 907:  sex=M  341 death 0.799436407
# 908:  sex=M  424 death 0.895539770
#
# Hint: If you're used to the tidyverse, you may find the data.table-equivalent of pivot_longer() (or gather()) very
# useful here.

tidyModel.survfitms <- function(x, ...) {
  assertClass(x, "survfitms")
  if (is.null(x$strata)) {
    dt.pstate <- data.table(x$pstate)
    names(dt.pstate) <- x$states
    dt.pstate <- dt.pstate[, -1, with = FALSE]
    dt.pstate.long <- melt.data.table(dt.pstate, measure.vars = names(dt.pstate))
    names(dt.pstate.long) <- c("state", "cif")
    dt <- data.table(time = x$time, as.factor(dt.pstate.long$state),
                     dt.pstate.long$cif)
    names(dt) <- c("time", "state", "cif")
    dt
  } else {
    dt.pstate <- data.table(x$pstate)
    names(dt.pstate) <- x$states
    dt.pstate <- dt.pstate[, -1, with = FALSE]
    dt.pstate.long <- melt.data.table(dt.pstate, measure.vars = names(dt.pstate))
    names(dt.pstate.long) <- c("state", "cif")
    strata.long <- character()
    for (i in seq_along(x$strata)) {
      strata.long <- c(strata.long, rep(names(x$strata)[i], x$strata[i]))
    }
    dt <- data.table(time = x$time, strata = as.factor(strata.long),
                     as.factor(dt.pstate.long$state), dt.pstate.long$cif)
    names(dt) <- c("time", "strata", "state", "cif")
    dt
  }
}


# Exercise 05: Model Plotting Method for "survfit" -------------------------------------------------------------------
# One of the main reasons you might want to clean up model objects like the ones you are provided with here is that
# tidy tabular data is much more convenient for plotting things with ggplot2.
# In this exercise, you want to write a `plot` method for objects of class `survfit` (so for example files
# `exampleSfit` and `exampleSfitStrata`) that plots the estimated survival probability (`surv`) on the y-axis with
# the `time` points on the x-axis as a step graph (hint: `geom_path`) and if there are `strata`, two lines with
# different colors should be printed. If there are no strata, the default color should be used.
#
# Since the `plot` generic function already exists in R, you neither have to nor should you define any other `plot`-
# method than the ones required for these exercises.
#
# Expected output:
# The function should return an object of classes "gg" and "ggplot", i.e. a regular ggplot2 plot object.
# It should also directly print the plot (= display it in the RStudio "Plots" window), as is usually expected from
# plotting functions.
#
# The plots produced by your function should look like the example plots we have included in the `assets` directory:
# assets/examplePlot-Sfit.png (for a plot based on `exampleSfit`)
# assets/examplePlot-SfitStrata.png (for a plot based on `exampleSfitStrata`)
#
# After your are done and are prepared to submit:
# Save your plots for each example file with `ggsave` to the repository root directory like this:
# ggsave("A02Plot-Sfit.png", plot(exampleSfit), width = 6, height = 5)
# ggsave("A02Plot-SfitStrata.png", plot(exampleSfitStrata), width = 6, height = 5)
plot.survfit <- function(x, ...) {
  assertClass(x, "survfit")
  if (is.null(x$strata)) {
    ggplot(tidyModel(x), aes(x = time)) +
      geom_path(aes(y = surv)) +
      ggtitle("Survival probability") +
      labs(y = "S(t)", x = "Time") +
      theme(legend.position = "top")
  } else {
    ggplot(tidyModel(x), aes(x = time)) +
      geom_path(aes(y = surv, colour = strata)) +
      ggtitle("Survival probability") +
      labs(y = "S(t)", x = "Time") +
      theme(legend.position = "top") +
      guides(colour = guide_legend(title = "Strata"))
  }
}
# Exercise 06: Model Plotting Method for "survfitms" -----------------------------------------------------------------
# Similar to the `plot`-method for "survfit" objects, you now want to add a method for objects of class `survfitms`.
# This time we want the plot to have the following components:
# - x-axis: The `time` points.
# - y-axis: The estimated CIF (`cif`).
# - color: Depending on the levels / names of the `states` in the given objects (e.g. `exampleMfit`)
# - Line type: Depending on the `strata`, if applicable (i.e. for `exampleMfitStrata`). If there are no `strata`,
#   the default line type in ggplot2 should be used.
#
# After your are done and are prepared to submit:
# Save your plots for each example file with `ggsave` to the repository root directory like this:
# ggsave("A02Plot-Mfit.png", plot(exampleMfit), width = 6, height = 5)
# ggsave("A02Plot-MfitStrata.png", plot(exampleMfitStrata), width = 6, height = 5)
plot.survfitms <- function(x, ...) {
  assertClass(x, "survfitms")
  if (is.null(x$strata)) {
    ggplot(tidyModel(x), aes(x = time, y = cif, colour = state)) +
      geom_path() +
      theme(legend.position = "top") +
      guides(colour = guide_legend(title = "State")) +
      ggtitle("Cumulative incidence by state") +
      labs(x = "Time", y = "CIF(t)")
  } else {
    ggplot(tidyModel(x), aes(x = time, y = cif, colour = state, linetype = strata)) +
      geom_path() +
      theme(legend.position = "top") +
      guides(colour = guide_legend(title = "State"), linetype = guide_legend(title = "Strata")) +
      ggtitle("Cumulative incidence by state") +
      labs(x = "Time", y = "CIF(t)")
  }
}
