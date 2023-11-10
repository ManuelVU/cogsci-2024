################################################################################
# Filter out participants that experienced all 4 conditions for the disease 
# domain in the three environments experiment
################################################################################

# read csv data only for the disease domain
three_environments <- readr::read_csv(
  file = "data/csv-files/three-environments-Disease.csv")

# store unique id numbers to loop over participants
participants <- unique(three_environments$id)

# start variable to store the number of conditions that participant experienced
conditions <- c()

# start loop to subset by participant id
for (n in participants) {
  
# subset data by participant id
  tmp <- subset(x = three_environments, subset = id == n)

# store number of unique conditions experienced by participant
  conditions[n] <- length(unique(tmp$condition))
}

# create a vector to pass to data function with the id's of participants who
# experienced all four conditions of the disease domain
participants_4c <- which(conditions == 4)
