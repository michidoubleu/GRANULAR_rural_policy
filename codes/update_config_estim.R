# Define your config file path
config_path <- "config_estim.R"

save(param.grid,all.vars.considered,all.dummies.considered, file = "input/temp_grid.RData")

# Define the current list of jobs
current_jobs <- 1:nrow(param.grid)

# Read the config file
config_lines <- readLines(config_path)

# Update the JOBS line
config_lines <- gsub(
  pattern = "^JOBS = .*",
  replacement = paste0("JOBS = c(",  paste(current_jobs[1],current_jobs[length(current_jobs)],  sep = ":"), ")"),
  x = config_lines
)

# Write the updated config back to the file
writeLines(config_lines, config_path)

# in case i wanna delete output on new run
# # Set the path to the folder
# folder_path <- "output"
# # List all files in the folder
# files_to_delete <- list.files(folder_path, full.names = TRUE)
# # Delete the files
# unlink(files_to_delete)


