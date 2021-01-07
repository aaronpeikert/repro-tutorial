# I try to log what I am doing
if(!requireNamespace("remotes"))install.packages("remotes")
remotes::install_github("aaronpeikert/repro")
repro::use_repro_template()
# ISSUE: https://github.com/aaronpeikert/repro/issues/53
# manually add install.Rmd, delete examples
# configure Docker and Make using repro:
repro::check_make()
repro::check_docker()
repro::automate()
# remind git add correct files

# add git:
repro::check_git()
usethis::use_git()

# add a licence:
usethis::use_ccby_license()
git2r::add("." ,"LICENSE.md")
git2r::commit(".", "usethis::use_ccby_license()")

# add a readme:
usethis::use_readme_rmd()
# manually edit readme
# add readme to makefile:
repro::automate()
# ISSUE: https://github.com/aaronpeikert/repro/issues/46
system("make docker")
system("make -B DOCKER=TRUE")
# ISSUE: https://github.com/aaronpeikert/repro/issues/54
# changed R version to 4.0.3 in Dockerfile

# add a code of conduct
usethis::use_code_of_conduct()
git2r::add("." ,"README.Rmd")
git2r::add("." ,"CODE_OF_CONDUCT.md")
system("make DOCKER=TRUE")
git2r::add("." ,"README.md")
git2r::commit(".", "usethis::use_code_of_conduct()")

# use github
repro::check_github()
# ISSUE: https://github.com/aaronpeikert/repro/issues/55
usethis::use_github()
