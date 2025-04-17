# get all files in the repo
all_files <- list.files(recursive = TRUE)

rmds <- all_files[grepl(".Rmd$", all_files)]
r_scripts <- all_files[grepl(".R$", all_files)]

# output lints
for (rfile in r_scripts) {
  print(lintr::lint(rfile))
}

# expect no lints
for (rfile in r_scripts) {
  lintr::expect_lint(checks = NULL, file = rfile)
}


# output lints
for (rmdfile in rmds) {
  print(lintr::lint(rmdfile))
}

# expect no lints
for (rmdfile in rmds) {
  lintr::expect_lint(checks = NULL, file = rmdfile)
}
