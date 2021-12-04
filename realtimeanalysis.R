#!/usr/bin/env -S Rscript --vanilla
library(argparser)

# global variable stores debug status
DEBUG <- FALSE

# function used to calculate the total utilization bound of a set of tasks
# this is defined as the sum of (compute / period) over the set of tasks
calculate_utilization_bound <- function(compute_times, periods) {
  # for each task with values of period and compute, we will calculate ( compute / period ) to find the fraction of compute time used by the task
  task_compute_times <- mapply(function(x, y) x / y, compute_times, periods)
  # if the sum of these quotients across all tasks is less than 1, then the tasks will be able to be scheduled under RMS or EDF algorithms
  sum_of_compute_times <- sum(task_compute_times)
  return(sum_of_compute_times)
}

# function used to determine whether the task periods are harmonic
# the set of periods is harmonic if each period is an integer multiple of the next smaller period
periods_are_harmonic <- function(periods) {
  n <- length(periods)
  if(n < 2) {
    return(TRUE)
  }
  
  sorted_periods <- sort(periods)
  
  # calculate the modulus of each successive pair of periods
  successive_modulus <- mapply(function(x, y) x %% y, sorted_periods[2:n], sorted_periods[1:(n-1)])
  
  return(all(successive_modulus == 0))
}

# analyze the feasability of RMS under traditional Liu-Layland test in single core contexts
liu_layland_rms_test <- function(n, task_data, utilization) {
  cat("RMS Schedulability (Liu-Layland)\n")
  is_harmonic <- periods_are_harmonic(task_data$period)
  cat(paste0("Task periods are harmonic: ", ifelse(is_harmonic, "Y", "N"), "\n"))
  
  # a sufficient bound on feasible compute time under RMS given by Liu and Layland
  rms_upper_bound <- n * (2 ^ (1/n) - 1)
  cat(paste0("Viable upper bound on utilization under RMS: ", rms_upper_bound, " (Aharmonic), 1 (Harmonic)\n"))
  
  # the tasks can be scheduled under RMS if either
  # 1. utilization <= rms_upper_bound
  # 2. utilization <= 1 and periods of tasks are harmonic
  schedulable_under_rms <-  (utilization <= rms_upper_bound) || (utilization <= 1 && is_harmonic)
  cat(paste0("Tasks are schedulable under RMS: ", ifelse(schedulable_under_rms, "Y", "N"), "\n"))
}

# analyze the feasability of RMS under new Bini et. al test
bini_rms_test <- function(task_data) {
  cat("RMS Schedulability (Bini et. al)\n")
  
  # computing the product of (compute time / period + 1) across the tasks
  calculate_bini_statistic <- function(compute_times, periods) {
    individual_utilizations <- mapply(function(x,y) (x / y) + 1, compute_times, periods)
    return(prod(individual_utilizations))
  }
  
  bini_statistic <- calculate_bini_statistic(task_data$compute, task_data$period)
  cat(paste0("Product of (compute time / period + 1) over all tasks (Bini statistic): ", bini_statistic, "\n"))
  cat("Viable upper bound on Bini statistic: 2\n")
  schedulable_under_rms <- (bini_statistic <= 2)
  cat(paste0("Tasks are schedulable under RMS: ", ifelse(schedulable_under_rms, "Y", "N"), "\n"))
}

# analyze the feasability of EDF in single core contexts
edf_test <- function(utilization) {
  cat("EDF Schedulability\n")
  # EDF is an optimal scheduling algorithm and guarantee tasks can be scheduled as long as the utilization is less than 1
  edf_upper_bound <- 1
  cat(paste0("Viable upper bound on utilization under EDF: ", edf_upper_bound, "\n"))
  schedulable_under_edf <- utilization <= edf_upper_bound
  cat(paste0("Tasks are schedulable under EDF: ", ifelse(schedulable_under_edf, "Y", "N"), "\n"))
}


# function used to statically check whether deadlines will be met under hard constraints in single core context
hard_deadline_analyzer_single_core <- function(task_data, use_bini) {
  cat("Running hard deadline analysis of single core system...\n")
  n <- length(task_data)
  cat(paste0("Number of tasks: ", n, "\n"))
  utilization <- calculate_utilization_bound(task_data$compute, task_data$period)
  
  cat(paste0("Sum of ( compute time / period ) over all tasks: ", utilization, "\n\n"))
  
  liu_layland_rms_test(n, task_data, utilization)
  
  if (use_bini) {
    cat("\n")
    bini_rms_test(task_data)
  }
  
  cat("\n")
  edf_test(utilization)
}

# function used to statically check whether deadlines will be met under hard constraints on multicore systems
# m represents the number of cores
hard_deadline_analyzer_multi_core <- function(task_data, m) {
  cat("Running hard deadline analysis of multicore system...\n")
  n <- length(task_data)
  cat(paste0("Number of tasks: ", n, "\n"))
}

# main body of script
main <- function() {
  # setting up arg parser to read args
  p <- arg_parser("Realtime Deadline Analyzer")
  
  p <- add_argument(p, "file", help="Name of csv file containing task periods and deadlines")
  p <- add_argument(p, "--debug", help="Print additional information for debugging", flag=TRUE)
  p <- add_argument(p, "--cores", help="Number of cores, must be positive, triggers multicore analysis for values greater than 1", type="integer", default=1)
  p <- add_argument(p, "--soft", help="Analyze tasks under soft deadline constraints, the file used must provide mean and standard deviation of execution times", flag=TRUE)
  p <- add_argument(p, "--bini", help="Use the new feasability test from Bini et. al instead of the Liu-Layland test, only applicable in hard, single core context", flag=TRUE)
  
  # parsing arguments
  argv <- parse_args(p)
  
  
  cat("Realtime Deadline Analyzer v. 0.1\n")
  
  if (isTRUE(argv$debug)) {
    DEBUG <- TRUE
  }
  
  if(isTRUE(DEBUG)){
    cat("Running in debug mode.\n")
    cat(paste0("Attempting to read from file: ", argv$file, "\n"))
  }
  
  # parse csv file and validate that it contains the necessary values
  tasks <- read.csv(argv$file, colClasses = c("integer", "double", "double"))
  
  if (isTRUE(DEBUG)) {
    cat("Successfully read csv file.\n")
  }
  
  if (!("taskid" %in% colnames(tasks))) {
    stop("Data frame must contain task id's in 'taskid' column.\n")
  } else if (!("period" %in% colnames(tasks))) {
    stop("Data frame must contain information about the periods of the tasks in 'period' column.\n")
  } else if (!("compute" %in% colnames(tasks))) {
    stop("Data frame must contain information about the compute times of the tasks in the 'compute' column.\n")
  } else if (isTRUE(DEBUG)) {
    cat("Data read from csv file contains all necessary columns.\n")
    print(head(tasks))
  }
  cat(paste0("Number of cores: ", argv$cores, "\n\n"))
  # analyze the data set statically under soft deadline constraints if the "--soft" flag was specified
  if (isTRUE(argv$soft)) {
    cat("Soft deadline analysis is not supported yet.\n")
  }
  # 
  else {
    if (argv$cores > 1) {
      hard_deadline_analyzer_multi_core(task_data = tasks, m = argv$cores)
    } else {
      hard_deadline_analyzer_single_core(task_data = tasks, use_bini = argv$bini)
    }
  }
}

main()
