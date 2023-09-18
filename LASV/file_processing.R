# Reformats and summarises output from Lassa transmission model
# Processes each file separately.

rm(list=ls())

library(dplyr)
library(tidyr)
library(data.table)
library(readr)

# Start output file loop
for (i in (0:98)) {
  message("i = ", i)
  model_output <- read.csv(paste0("output", i, ".csv"))

  extracted <- model_output %>%
    filter(measure != "runaways",
           measure != "times")
  
  rm(model_output)

  reformatted <- extracted %>%
    gather(key = "Run", value = "Value", -c("identifier", "name", "measure")) %>%
    group_by(identifier, name, measure, Run) %>%
    mutate("time" = 1:3650) %>%
    mutate(Run = recode(Run, run1 = paste0("run", (i+1)))) %>%
    spread(key = "measure", value = "Value")
  
  write.csv(reformatted, file = paste0("OxLiv_lassa_Routput_R0_0631_v6C_reformatted_", i, ".csv"), row.names=FALSE)
}

