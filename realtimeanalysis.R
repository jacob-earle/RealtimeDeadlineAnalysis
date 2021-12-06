#!/usr/bin/env -S Rscript --vanilla
library(argparser)

# global variable stores debug status
DEBUG <- FALSE

# function used to check that the deadlines in a task set are sensible
# i.e. the function will check that for each task, compute <= deadline <= period
# and panic if either of these conditions are not met
check_deadlines <- function(task_data) {
  for(i in seq(1, nrow(task_data))) {
    if (task_data$deadline[i] < task_data$compute[i]) {
      stop(paste0("Task ", task_data$taskid[i], " has a deadline that is less than its compute time."))
    } else if (task_data$deadline[i] > task_data$period[i]) {
      stop(paste0("Task ", task_data$taskid[i], " has a deadline that is greater than its period."))
    }
  }
}

# function used to check that the total computation time of each task is less than or equal to its period
# will panic if this condition is not met
check_periods <- function(task_data) {
  for (i in seq(1, nrow(task_data))) {
    if (task_data$compute[i] > task_data$period[i]) {
      stop(paste0("Task ", task_data$taskid[i], " has a greater compute time than its period."))
    }
  }
}

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

# analyze the feasability of fp scheduling algorithms in single core contexts using the delta hyperplanes exact test (d-HET)
dHET_test <- function(task_data) {
  cat("Fixed Priority Schedulability (d-HET)\n")
  
  periods <- task_data$period
  computes <- task_data$compute
  deadlines <- task_data$deadline
  n <- nrow(task_data)
  
  # defining helper function which calculates the amount of work done by the i highest priority tasks in the interval [0,b]
  # these variables will be used within the workload function and must be initialized
  last_phi <- numeric(n)
  last_workload <- numeric(n)
  workload <- function(i, b) {
    if (i <= 0) { return(0) }
    if (b <= last_phi[i]) { return(last_workload[i]) } # if workload(i,b) already calculated
    f <- floor(b / periods[i])
    c <- ceiling(b / periods[i])
    branch0 <- b - f*(periods[i] - computes[i]) + workload(i-1, b)
    branch1 <- c*computes[i] + workload(i-1, b)
    last_phi[i] <- b
    last_workload[i] <- min(branch0, branch1)
    return(last_workload[i])
  }
  
  # defining helper function that will perform the analysis and return a boolean representing whether the task set is feasible
  fp_test <- function() {
    for (i in c(1:n)) {
      if ((computes[i] + workload(i - 1, deadlines[i])) > deadlines[i]) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  
  # conduct and print the results of the test
  schedulable_under_fp <- fp_test()
  cat(paste0("Tasks are schedulable under Fixed Priority: ", ifelse(schedulable_under_fp, "Y", "N"), "\n"))
}

# helper function used within the EDF test in cases when explicit deadlines are specified
baruah_test_helper <- function(task_data, utilization) {
  periods <- task_data$period
  computes <- task_data$compute
  deadlines <- task_data$deadline
  utilizations <- computes / deadlines
  
  L_star <- sum(mapply(function(a,b,c) (a - b) * c  , periods, deadlines, utilizations)) / (1 - utilization)
  upper_bound_for_testing <- max(deadlines, L_star)
  
  # generate all deadlines that are less than the upper bound for testing
  times_to_test <- c()
  for (i in seq(1, nrow(task_data))) {
    times_to_test <- c(times_to_test, seq(deadlines[i], upper_bound_for_testing, by=periods[i]))
  }
  times_to_test <- unique(times_to_test)
  
  # checking the conditions are met for each deadline with the help of a helper function
  check_condition <- function(l) {
    baruah_statistic <- sum(
      mapply(
        function(a,b,c) {
          floor((l + a - b) / a) * c
        },
        periods,
        deadlines,
        computes
      )
    )
    
    return(baruah_statistic <= l)
  }
  
  for(t in times_to_test) {
    if (!check_condition(t)) { return(FALSE) }
  }
  
  # all times satisfied the test, so we can return TRUE
  return(TRUE)
}

# analyze the feasability of EDF in single core contexts
edf_test <- function(task_data, utilization, use_deadlines) {
  cat("EDF Schedulability\n")
  # EDF is an optimal scheduling algorithm and guarantee tasks can be scheduled as long as the utilization is less than 1
  edf_upper_bound <- 1
  cat(paste0("Viable upper bound on utilization under EDF: ", edf_upper_bound, "\n"))
  if (use_deadlines) {
    baruah_result <- baruah_test_helper(task_data, utilization)
    cat(paste0("Tasks pass the Baruah test for schedulability: ", ifelse(baruah_result, "Y", "N"), "\n"))
    schedulable_under_edf <- (utilization < 1) && baruah_result
  } else {
    schedulable_under_edf <- utilization <= edf_upper_bound
  }
  cat(paste0("Tasks are schedulable under EDF: ", ifelse(schedulable_under_edf, "Y", "N"), "\n"))
}


# function used to statically check whether deadlines will be met under hard constraints in single core context
hard_deadline_analyzer_single_core <- function(task_data, use_bini, use_exact_deadlines) {
  cat("Running hard deadline analysis of single core system...\n")
  n <- nrow(task_data)
  cat(paste0("Number of tasks: ", n, "\n"))
  utilization <- calculate_utilization_bound(task_data$compute, task_data$period)
  
  cat(paste0("Sum of ( compute time / period ) over all tasks: ", utilization, "\n\n"))
  
  if (use_exact_deadlines) {
    dHET_test(task_data)
  } else {
    liu_layland_rms_test(n, task_data, utilization)
    
    if (use_bini) {
      cat("\n")
      bini_rms_test(task_data)
    }
  }
  
  cat("\n")
  edf_test(task_data, utilization, use_exact_deadlines)
}

# function used to statically check whether deadlines will be met under hard constraints on multicore systems
# m represents the number of cores
hard_deadline_analyzer_multi_core <- function(task_data, m) {
  cat("Running hard deadline analysis of multicore system...\n")
  n <- nrow(task_data)
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
  p <- add_argument(p, "--deadlines", help="Let deadlines be explicitly set rather than being assumed to be equal to the periods, when using this option, your file must contain a deadline column", flag=TRUE)
  
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
  } else if (argv$deadlines && !("deadline" %in% colnames(tasks))) {
    stop("Data frame must contain 'deadline' column when --deadlines option is set.")
  } else if (isTRUE(DEBUG)) {
    cat("Data read from csv file contains all necessary columns.\n")
    print(head(tasks))
  }
  
  check_periods(tasks)
  if (argv$deadlines) {
    check_deadlines(tasks)
  }
  
  cat(paste0("Number of cores: ", argv$cores, "\n\n"))
  # analyze the data set statically under soft deadline constraints if the "--soft" flag was specified
  if (isTRUE(argv$soft)) {
    cat("Soft deadline analysis is not supported yet.\n")
  }
  # running analysis under hard deadline constraints 
  else {
    if (argv$cores > 1) {
      if (argv$deadlines) {
        stop("Use of explicit deadlines is not supported for multicore systems.")
      }
      hard_deadline_analyzer_multi_core(task_data = tasks, m = argv$cores)
    } else {
      hard_deadline_analyzer_single_core(task_data = tasks, use_bini = argv$bini, use_exact_deadlines = argv$deadlines)
    }
  }
}

main()
