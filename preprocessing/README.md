
# Preprocessing

This folder contains the preprocessing scripts. Each experiment has 
one`.R` file, named in the standard way. 
 If there is an experiment called "urntask", for instance, the 
corresponding preprocessing script should be called `pre_urntask.R`.

The preprocessing script should read data files from the raw data folder within the
relevant experiment (e.g., [./experiments/exp_urntask/data](./experiments/exp_urntask/data))
(not in github yet for copyright reasons) but should not write anything to that folder. The only thing it should do is
generate the clean data file, e.g. [./data/data_urntask.csv](./data/data_urntask.csv)
