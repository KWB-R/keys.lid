### How to build an R package from scratch

usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")



author <- list(name = "Roberto Tatis-Muvdi",
               orcid = "0000-0003-0490-7999",
               url = "https://github.com/robetatis")

pkg <- list(name = "keys.lid",
            title = "R Package for Simulating the Impact of Different LIDs
            under Varying Climate Boundary Conditions on Annual Volume Rainfall
            Retention",
            desc  = paste("R Package for Simulating the Impact of Different LIDs",
                          "(Low Impact Development) under Varying Climate Boundary",
                          "Conditions in China on annual VRR (Volume Rainfall
                          Retention)."))


kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("validation")
usethis::use_vignette("sensitivity")

### R functions
pkg_dependencies <- c("zoo", "dplyr", "swmmr")

sapply(pkg_dependencies, usethis::use_package)

desc::desc_add_remotes("kwb-r/kwb.utils",normalize = TRUE)

usethis::use_pipe()

kwb.pkgbuild::use_autopkgdown()

kwb.pkgbuild::create_empty_branch_ghpages("urbanAnnualRunoff")
