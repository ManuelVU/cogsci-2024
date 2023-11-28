################################################################################
# Read raw data from matlab file and write a csv file in long format
################################################################################

# load read-write function
source(file = "src/data-f/three-environments-rw.R")

# write data from all domains into a csv with all 55 participants.
three_env_rw(data_path = "data/matlab-files/three-environments.mat")

# write data from disease domain into a csv with all 55 participants.
three_env_rw(data_path = "data/matlab-files/three-environments.mat", 
             domains_keep = "Disease", file_suffix = "")

# write data for analysis with only those participants who saw at least one 
# change
source(file = "analysis/data-cleaning/filter-participants-disease-4conditions.R")
three_env_rw(data_path = "data/matlab-files/three-environments.mat", 
             domains_keep = "Disease", participants_keep = participants_2_more, 
             file_suffix = "-2-more-categories")

# write data from disease domain into a csv keeping only participants that 
# experienced all 4 category structures and meet the learning criterion
source(file = "analysis/data-cleaning/filter-participants-disease-4conditions.R")
three_env_rw(data_path = "data/matlab-files/three-environments.mat", 
             domains_keep = "Disease", participants_keep = participants_4c,
             file_suffix = "")


