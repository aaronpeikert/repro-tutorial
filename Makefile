PROJECT := reprotutorial
WORKDIR := $(CURDIR)

# list below your targets and their recipies
all: install.md README.md manuscript.pdf preregistration.pdf

manuscript.tex: manuscript.pdf

submission.zip: manuscript.pdf manuscript.tex manuscript.Rmd journalnames.tex mdpi.bst mdpi.cls logo-updates.pdf journalnames.tex chicago2.bst
	$(RUN1) zip $@ $^ $(RUN2)

data/simulation_results.csv: R/simulation.R R/simulation_funs.R
	$(RUN1) Rscript -e 'source("$<")' $(RUN2)

simulated_data.csv: R/simulation.R
	$(RUN1) Rscript -e 'source("R/simulate.R")' $(RUN2)

data/sd3.csv: R/simulation_funs.R
	$(RUN1) Rscript -e 'source("$<"); set.seed(1235); readr::write_csv(generate_data(500), "data/sd3.csv")' $(RUN2)

images/nutshell.svg:
	$(RUN1) mkdir -p images && \
	wget -O $@ https://github.com/aaronpeikert/reproducible-research/raw/master/Images/nutshell.svg $(RUN2)
images/nutshell.pdf:
	$(RUN1) mkdir -p images && \
	wget -O $@ https://github.com/aaronpeikert/reproducible-research/raw/master/Images/nutshell.pdf $(RUN2)

apa7.csl:
	$(RUN1) wget -O $@ https://raw.githubusercontent.com/citation-style-language/styles/master/apa.csl $(RUN2)

temp.bib: R/bibliography.R references.bib
	$(RUN1) Rscript -e 'source("$<")' $(RUN2)

### Wrap Commands ###
# if a command is to be send to another process e.g. a container/scheduler use:
# $(RUN1) mycommand --myflag $(RUN2)
RUN1 = $(QRUN1) $(SRUN) $(DRUN)
RUN2 = $(QRUN2)

### Rmd's ###
include .repro/Makefile_Rmds

### Docker ###
# this is a workaround for windows users
# please set WINDOWS=TRUE and adapt WINPATH if you are a windows user
# note the unusual way to specify the path
WINPATH = //c/Users/someuser/Documents/myproject/
include .repro/Makefile_Docker

