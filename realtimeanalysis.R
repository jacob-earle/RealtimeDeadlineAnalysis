#!/usr/bin/env -S Rscript --vanilla
library(argparser)

# function used to statically see whether deadlines will be met under hard constraints
hard_deadline_analyzer <- function(task_data) {
  # for each task with values of period and compute, we will calculate ( compute / period ) to find the fraction of compute time used by the task
  task_compute_times <- mapply(function(x, y) x / y, task_data$compute, task_data$period)
  # if the sum of these quotients across all tasks is less than 1, then the tasks will be able to be scheduled under RMS or EDF algorithms
  sum_of_compute_times <- sum(task_compute_times)
  
  cat(paste0("Sum of ( compute time / period ) over all tasks: ", sum_of_compute_times, "\n"))
  if (sum_of_compute_times < 1) {
    cat("Tasks CAN be scheduled successfully under hard realtime deadlines.\n")
  } else {
    cat("Tasks CANNOT be scheduled successfully under hard realtime deadlines.\n")
  }
}

# main body of script
main <- function() {
  # setting up arg parser to read args
  p <- arg_parser("Realtime Deadline Analyzer")
  
  p <- add_argument(p, "file", help="Name of csv file containing task periods and deadlines")
  p <- add_argument(p, "--debug", help="Print additional information for debugging", flag=TRUE)
  p <- add_argument(p, "--soft", help="Analyze tasks under soft deadline constraints, the file used must provide mean and standard deviation of execution times", flag=TRUE)
  
  # parsing arguments
  argv <- parse_args(p)
  
  
  cat("Realtime Deadline Analyzer v. 0.1\n")
  
  if (isTRUE(argv$debug)) {
    cat("Running in debug mode.\n")
  }
  
  cat(paste0("Attempting to read from file: ", argv$file, "\n"))
  
  # parse csv file and validate that it contains the necessary values
  tasks <- read.csv(argv$file, colClasses = c("integer", "double", "double"))
  
  if (isTRUE(argv$debug)) {
    cat("Successfully read csv file.\n")
  }
  
  if (!("taskid" %in% colnames(tasks))) {
    stop("Data frame must contain task id's in 'taskid' column.\n")
  } else if (!("period" %in% colnames(tasks))) {
    stop("Data frame must contain information about the periods of the tasks in 'period' column.\n")
  } else if (!("compute" %in% colnames(tasks))) {
    stop("Data frame must contain information about the compute times of the tasks in the 'compute' column.\n")
  } else if (isTRUE(argv$debug)) {
    cat("Data read from csv file contains all necessary columns.\n")
  }
  
  # analyze the data set statically under soft deadline constraints if the "--soft" flag was specified
  if (isTRUE(argv$soft)) {
    cat("Soft deadline analysis is not supported yet.\n")
  }
  # 
  else {
    hard_deadline_analyzer(task_data = tasks)
  }
}

main()
