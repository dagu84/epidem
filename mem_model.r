library(dplyr)
library(mem)

# importing arguments from python function
args = commandArgs(trailingOnly = TRUE)
data_path = args[1]
disease = args[2]

# data
data = read.csv(data_path)

# model
epi = memmodel(data)
output = as.data.frame(epi$intensity.thresholds)

# export
output_path = file.path(dirname(data_path), paste0("mem_threshold_", disease, ".csv"))
write.csv(output, output_path, row.names = FALSE)