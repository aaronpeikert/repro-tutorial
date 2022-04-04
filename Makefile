PROJECT := reprotutorial
WORKDIR := $(CURDIR)

# list below your targets and their recipies
all: install.md README.md manuscript.pdf preregistration.pdf

manuscript.tex: manuscript.pdf

submission.zip: manuscript.pdf manuscript.tex manuscript.Rmd journalnames.tex mdpi.bst mdpi.cls logo-updates.pdf journalnames.tex chicago2.bst manuscript_files/ *.bib
	$(RUN1) zip -r $@ $^ $(RUN2)

figures.zip: images/ manuscript_files/
	$(RUN1) zip -r $@ $^ $(RUN2)

data/MACH_data.zip:
	$(RUN1) curl https://openpsychometrics.org/_rawdata/MACH_data.zip -o $@ $(RUN2) 

data/data.csv: data/MACH_data.zip
	$(RUN1) unzip -p $< MACH_data/data.csv > $@ $(RUN2) 

data/simulation_results.csv: R/simulation.R R/simulation_funs.R
	$(RUN1) Rscript -e 'source("$<")' $(RUN2)

simulated_data.csv: R/simulation.R
	$(RUN1) Rscript -e 'source("R/simulate.R")' $(RUN2)

data/sd3.csv: R/simulation_funs.R
	$(RUN1) Rscript -e 'source("$<"); set.seed(1235); readr::write_csv(generate_data(500), "data/sd3.csv")' $(RUN2)

images/nutshell.pdf: images/nutshell.svg
	inkscape --export-area-page --export-filename=$@ $<

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

