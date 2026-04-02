# ramTrackR

`ramTrackR` is an R package designed to monitor the RAM usage in the background without blocking the active R session. It allows users to start a background monitor, "bookmark" specific execution timepoints, and visualize memory usage over time.

## Installation

You can install the development version of `ramTrackR` from GitHub:

```r
# install.packages("devtools")
devtools::install_github("mvesuviusc/ramTrackR")
```

## Setup & Dependencies

This package relies on several underlying libraries:
* `callr` for background process execution.
* `ps` to poll system process statistics.
* `ggplot2` for rendering RAM usage over time.

## Usage

### 1. Start Monitoring

To begin monitoring the current R session, initialize the monitor:

```r
library(ramTrackR)

# Starts a background process polling every 0.5 seconds
monitor <- start_monitoring(interval = 0.5)
```

The `start_monitoring()` function returns an S4 class object of type `RamMonitor` which maintains the state, the background process handle, and bookmark data.

### 2. Run Code and Bookmark Timepoints

While your R code runs, you can record specific moments in time to analyze how much RAM a specific operation took using `bookmark_time()`.

```r
# Simulate some memory intensive work
x <- runif(1e7)

# Bookmark when we finished allocating 'x'
bookmark_time(
  monitor, 
  label = "Allocated x"
)

y <- runif(2e7)
bookmark_time(
  monitor, 
  label = "Allocated y"
)

# Clean up memory
rm(y)
gc()

bookmark_time(
  monitor, 
  label = "Removed y"
)
```

### 3. Check Memory Stats

Extract the maximum RAM used during the session or the current RAM usage at any moment:

```r
current_ram <- get_current_ram_usage(monitor)
max_ram <- get_max_ram_usage(monitor)

cat("Max RAM:", max_ram, "GB\n")
```

### 4. Stop Monitoring

Always make sure to cleanly stop the background monitoring process when you are finished:

```r
stop_monitoring(monitor)
```

### 5. Visualize Results

Plot the memory usage over time. Your bookmarked events will automatically appear as vertical red dashed lines.

```r
# Note: You can customize line size, line color, and font sizes.
ram_plot <- plot_ram_usage(
  monitor,
  line_size = 2,
  line_color = "steelblue",
  font_size_title = 18,
  font_size_axis = 14
)

print(ram_plot)
```
