# You bought a new laptop that requires you to change your password every two weeks. To keep things simple,
# you wanted to keep the same password and just add a different number every time, but apparently the system
# does not accept passwords that are too similar to each other.
# Passwords are only allowed to contain certain characters, otherwise your laptop will not
# accept them. Numbers from 0 to 9 as well as lower and upper case letters are allowed.
# Valid special characters are ?!,._-+#$&%=. All other special characters
# including ä, ü, ö and ß are not allowed. Also, passwords must at least be a certain length.

# Exercise 01: Password Check  ----------------------------------------------------------------------------------
# Write a function that checks passwords for their validity. The goal is a function that can be used like
# an assertion from the checkmate package, so you can use it in the next function in this exercise.
#
# Function inputs:
# - `pw`: A character vector containing at least one password
# - `numChars`: An integer specifying the minimum number of characters for each password
# - `numPw`: An integer specifying the minimum number of passwords in the `pw` vector.
#
# You want to keep your function flexible, but of course an empty password does not make sense,
# so the minimum number of characters in each password must be >= 1 and the password vector must at least contain
# one element. If this is not the case, your function throws a checkmate assertion error. Any missing values
# should also throw a checkmate assertion error. There are no defaults for the three inputs to your function.
#
# Examples for valid input (expected return value is identical to `pw`):
# ex01PwValidity(pw = "mJj-GHDXyl+GTFrHkQs,", numChars = 5, numPw = 1)
# ex01PwValidity(pw = c("hallowelt", "welthallo"), numChars = 5, numPw = 2)
# ex01PwValidity(pw = c("a", "b", "c", "d", "e", "f"), numChars = 1, numPw = 6)
#
# Examples for invalid input that should result in a checkmate assertion error:
# ex01PwValidity(pw = c("a", "b", "c", "d", "e", "f"), numChars = 1, numPw = 100)
# -> numPw says there's 100 passwords but `pw` is only 6 elements long
# ex01PwValidity(pw = "Rühreikompott", numChars = 5, numPw = 1) -> no äöüß characters
# ex01PwValidity(pw = "Motörhead", numChars = 5, numPw = 1) -> no äöüß characters
# ex01PwValidity(pw = "mJj-GHDXyl+GTFrHkQs,", numChars = 5, numPw = -3) -> numPw can't be negative
# ex01PwValidity(pw = "mJj-GHDXyl+GTFrHkQs,", numChars = 0, numPw = 1) -> numChars must be 1 or greater
# ex01PwValidity(pw = "uff", numChars = 5, numPw = 1) -> `pw` contains less characters than `numChars`
# ex01PwValidity(pw = NA, numChars = NA, numPw = NA) -> None of the inputs can be NA individually or together
ex01PwValidity <- function(pw, numChars, numPw) {
  assertNumeric(numChars, lower = 1, any.missing = FALSE, len = 1)
  assertNumeric(numPw, lower = 1, any.missing = FALSE, len = 1)
  assertCharacter(pw, any.missing = FALSE, min.chars = numChars, len = numPw)
  allowed.special.characters <- c("?", "!", ",", ".", "_", "-", "+", "#", "$", "&", "%", "=")
  special.characters <- lapply(regmatches(pw, gregexpr("[^0-9A-Za-z///' ]", pw, ignore.case = TRUE)), unique)
  n.specialchars <- lapply(special.characters, length)
  n.allowed.specialchars <- lapply(lapply(special.characters, `%in%`, x = allowed.special.characters), sum)
  assert(identical(n.specialchars, n.allowed.specialchars), msg = "A password contains an illegal special character")
  pw
}

# Exercise 02: Compare Passwords --------------------------------------------------------------------------------
# Write a function that checks for a given password (currentPw) and a given character vector of new
# passwords (newPw) which passwords are valid new passwords. From experience with your new laptop
# you know that if more than a few consecutive characters in your new password are the same as in the old
# password, the system rejects it. You don't know exactly how many characters that is, so you want to write
# a function that is flexible in that way.
#
# Function inputs:
# - `currentPw`: Your current password (a character vector with 1 element)
# - `newPw`: Vector of new passwords (a character vector of length >= 1)
# - `match`: An integer >= 1 that specifies how many consecutive characters the function should check for.
#
# For `currentPw` and `newPw` there are no default values, for `match` the default is 3.
#
# Write a function that compares passwords, using the three inputs described above.
# Example: Your current password is "IamaPassword.123" and you want to check if this new password "789_IamNew"
# is okay, the number of consecutive characters that are the same in these two passwords is three ("Iam"), so if
# you set match to 3 in your function, it returns FALSE meaning the new password is not valid. But if you set
# match to 4 your function returns TRUE, because there are no 4 (or more) consecutive characters in the two
# passwords that are the same. So your new password is a valid new password with this setting.
#
# Your function should treat lower and upper case letters the same, meaning "Iam", "iam", "IAM" and "iAm"
# all match. If your current password (`currentPw`) contains any special characters that are not allowed as
# specified above, make sure your function throws a checkmate error.
# If any of the new passwords (`newPW`) contain any special characters that are not allowed, your function
# simply returns `FALSE` for that specific password meaning it is not a valid new password.
# The length of the current password must at least be `match`, otherwise your function returns a checkmate error.
# If the length of any new passwords is smaller than `match`, your function returns `FALSE` for that specific password.
# No missing entries (NA, etc.) in any of the inputs is allowed, your function returns a checkmate error.
#
# EXAMPLES for valid input and expected output:
# ex02PwCompare(currentPw = "IamaPassword.123", newPw = "helloWORLD", match = 3) -> FALSE
# ex02PwCompare(currentPw = "IamaPassword.123", newPw = "helloWORLD") -> FALSE
# ex02PwCompare(currentPw = "IamaPassword.123", newPw = "helloWORLD", match = 4) -> TRUE
# ex02PwCompare(currentPw = "!thisISfun!792", newPw = "!792newpasswordAGAIN?", match = 4) -> FALSE
# ex02PwCompare(currentPw = "!thisISfun!792", newPw = "Icontain/VERY\specialCharacters", match = 4) -> FALSE
# ex02PwCompare(currentPw = "!thisISfun=792", newPw =
#               c("7iamcool!", "butI-dislike-New", "792passwords", "immensly!!!")) -> c(TRUE, TRUE, FALSE, TRUE)
# ex02PwCompare(currentPw = "valid_password", newPw =
#              c("=???#_", "****iiii;;", "not okay")) -> c(TRUE, FALSE, FALSE)
#ex02PwCompare(currentPw = "valid_password", newPw =
#               c("ääääää", "ö", "üüüüüüü", "ßß", "aouie")) -> c(FALSE, FALSE, FALSE, FALSE, TRUE)
# ex02PwCompare(currentPw = "valid_password", newPw = "notgood", match = 1) -> c(FALSE)
#
# EXAMPLES for invalid input:
# (unspecified parameters are assumed to be the default values)
# ex02PwCompare(currentPw = "p1", newPw = "helloWORLD", match = 3)
#               -> Your function gives a checkmate assertion error
# ex02PwCompare(currentPw = "IamaPasswörd", newPw = "helloWORLD")
#               -> Your function gives a checkmate assertion error
# ex02PwCompare(currentPw = "no blanks please", newPw = "helloWORLD")
#               -> Your function gives a checkmate assertion error
# ex02PwCompare(currentPw = c("password1", "secondpw79?"), newPw = "helloWORLD")
#               -> Your function gives a checkmate assertion error
# ex02PwCompare(currentPw = "password1", newPw = c("helloWORLD", NA))
#               -> Your function gives a checkmate assertion error
# ex02PwCompare(currentPw = NA, newPw = c("helloWORLD"))
#               -> Your function gives a checkmate assertion error
# ex02PwCompare(currentPw = "password1", newPw = c("helloWORLD"), match = 0)
#               -> Your function gives a checkmate assertion error
# ex02PwCompare(currentPw = "password1", newPw = c("helloWORLD"), match = NA)
#               -> Your function gives a checkmate assertion error
getConsecutiveChars <- function(x, len = 3) {
  n.letters <- character()
  if (length(x) < len) {
    return(x)
  }
  for (i in 1:(length(x) - (len - 1))) {
    n.letters[i] <- paste0(x[i:(i + (len - 1))], collapse = "")
  }
  n.letters
}
checkPwValidityLogical <- function(pw, numChars, numPw) {
  assertNumeric(numChars, lower = 1, any.missing = FALSE, len = 1)
  assertNumeric(numPw, lower = 1, any.missing = FALSE, len = 1)
  assertCharacter(pw, any.missing = FALSE, min.chars = numChars, len = numPw)
  allowed.special.characters <- c("?", "!", ",", ".", "_", "-", "+", "#", "$", "&", "%", "=")
  special.characters <- lapply(regmatches(pw, gregexpr("[^0-9A-Za-z///' ]", pw, ignore.case = TRUE)), unique)
  n.specialchars <- lapply(special.characters, length)
  n.allowed.specialchars <- lapply(lapply(special.characters, `%in%`, x = allowed.special.characters), sum)
  res <- unlist(lapply(lapply(n.specialchars, `==`, x = n.allowed.specialchars), any))
  res[which(nchar(pw) != nchar(gsub(" ", "", pw, fixed = TRUE)))] <- FALSE
  res
}
ex02PwCompare <- function(currentPw, newPw, match = 3) {
  assertNumeric(match, lower = 1, any.missing = FALSE, len = 1)
  assertString(currentPw, min.chars = match, na.ok = FALSE, null.ok = FALSE)
  assert(nchar(currentPw) == nchar(gsub(" ", "", currentPw, fixed = TRUE)), msg = "No blanks in PW allowed!")
  assertCharacter(newPw, min.len = 1, any.missing = FALSE)
  currentPw <- ex01PwValidity(currentPw, numChars = nchar(currentPw), numPw = 1)
  currentpw.split <- unlist(strsplit(currentPw, ""))
  newpw.split <- strsplit(newPw, "")
  consecutive.currentpw <- getConsecutiveChars(currentpw.split, len = match)
  consecutive.newpw <- lapply(newpw.split, getConsecutiveChars, len = match)
  consecutive.currentpw <- tolower(consecutive.currentpw)
  consecutive.newpw <- lapply(consecutive.newpw, tolower)
  res <- !unlist(lapply(lapply(consecutive.newpw, `%in%`, x = consecutive.currentpw), any))
  res[which(!checkPwValidityLogical(newPw, numChars = min(nchar(newPw)), numPw = length(newPw)))] <- FALSE
  res
}
