# RealtimeDeadlineAnalysis

## Overview
This command line tool is a part of my senior research on the topic of real-time operating systems and scheduling algorithms. This project provides the `realtimeanalysis.R` script, which can read a CSV description of a theoretical real-time task set with specified periods, compute times, and deadlines, and will perform various tests to see whether the task set can be feasibly scheduled under various scheduling algorithms.

## Running the Script
In order to run the script, you must make sure the R language is installed on your system. Note, these instructions will only work on UNIX-like systems. You can find a guide on how to install the R language on your system [here](https://mirror.las.iastate.edu/CRAN/) if it is not already installed.

Once R is installed, you will also need to make sure the `argparser` package is installed locally. To do this, enter the R interactive prompt from the command line with 
```
R
```
From here, install the `argparser` package by typing
```
> install.packages("argparser")
```
and selecting the appropriate mirror.

Finally, you will need to navigate to this project's directory and make the script executable
```
chmod +x ./realtimeanalysis.R
```

Now, you will be able to use the script. The basic usage from the project directory is 
```
./realtimeanalysis.R [-c NUMBER_OF_CORES] [OPTIONS] PATH_TO_TASKSET_FILE
```

Some example task set files are provided in the `examples` directory, with `harddeadlines1.csv` representing a task set implied deadlines equal to the task periods, and `harddeadlines2.csv` representing a task set with explicit deadlines. For more help on usage and all of the different options available, use
```
./realtimeanalysis.R --help
```

## Tests Implemented
This script provides analysis for real-time task systems in hard deadline contexts, i.e. in contexts where no task may miss any of its deadlines in order for the task set to be considered feasible. The tests implemented here can provide analysis in both single- and multicore contexts, and in situations where deadlines are either implicitly equal to the task periods or explicitly provided. The tests provided are summarized below:

### Singlecore Context
#### Implicit Deadlines
1. 1973 Liu-Layland Test for feasibility under the RMS algorithm (more info  on the algorithm [here](https://en.wikipedia.org/wiki/Rate-monotonic_scheduling)); this is a sufficient but not necessary test
2. 1973 Liu-Layland Test for feasibility under the EDF algorithm (more info on the algorithm [here](https://en.wikipedia.org/wiki/Earliest_deadline_first_scheduling)); this test is both sufficient and necessary
3. 2003 Bini et. al. Test for feasibility under the RMS algorithm; this is also a sufficient but not necessary test, but improves on original Liu-Layland Test

#### Explicit Deadlines
1. 2004 Bini et. al. Hyperplanes delta-Exact Test for feasibility under any fixed priority algorithm; this test is both sufficient and necessary
2. 1990 Baruah et. al. Test for feasibility under the EDF algorithm; this is both sufficient and necessary

### Multicore Context
1. 2 tests for feasibility under the RMFF algorithm (more info on the algorithm [here](http://www.it.uu.se/edu/course/homepage/realtid/ht14/schedule/multiprocessor-1.pdf)); both tests are sufficient but not necessary
2. 2001 Andersson et. al. Test for feasibility under any fixed priority multicore scheduling algorithm

## Future Developments
In the future, this project could be expanded by adding tests for analyzing task sets in additional contexts, for example, analyzing the tasks using soft deadline constraints or adding analysis for aperiodic tasks.
