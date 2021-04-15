library("future.batchtools")
# ssh keys login
#ssh-copy-id -i /home/aaron/.ssh/id_rsa peikert@tardis.mpib-berlin.mpg.de
tardis <- parallelly::makeClusterPSOCK("tardis.mpib-berlin.mpg.de",
                port='random', user="peikert",
                rscript=c("/opt/software/R/4.0.3/bin/Rscript"), 
                homogeneous = TRUE)
ncpus <- 10
plan(list(tweak(cluster, workers=tardis),
          tweak(batchtools_slurm,
                workers = 16,
                template = ".batchtools.slurm.singularity.tmpl",
                resources=list(ncpus=ncpus,
                               memory='700m',
                               walltime=6600,
                               partition=c('gpu'))),
          tweak(multicore, workers=ncpus)))
