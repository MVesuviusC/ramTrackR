#' @importFrom methods is new setClass setGeneric setMethod
#' @importFrom utils read.csv write.table write.csv
NULL

#' RamMonitor S4 Class
#'
#' An S4 class to manage the state of a RAM monitoring process.
#' @slot pid Numeric. The process ID being monitored.
#' @slot process Environment. Holds the background process handle.
#' @slot data_path Character. Path to the CSV data file.
#' @slot bookmarks Environment. Stores named timestamps as bookmarks.
#' @export
RamMonitor <- setClass(
  "RamMonitor",
  slots = c(
    pid = "numeric",
    process = "environment",
    data_path = "character",
    bookmarks = "environment"
  )
)

#' Internal Data Reader
#'
#' @param monitor A RamMonitor object
#' @return A data.frame of RAM data or an empty data.frame
#' @noRd
read_monitor_data <- function(monitor) {
  if (!is(monitor, "RamMonitor")) {
    stop("monitor must be a RamMonitor object")
  }

  if (!file.exists(monitor@data_path)) {
    return(
      data.frame(
        timestamp = numeric(0),
        memory_bytes = numeric(0)
      )
    )
  }

  # Safe read with a tryCatch to prevent collisions on Windows
  # (callr process writing while we are reading)
  ram_data <- tryCatch(
    {
      read.csv(monitor@data_path)
    },
    error = function(e) {
      Sys.sleep(0.1)
      read.csv(monitor@data_path)
    }
  )

  return(ram_data)
}


#' Internal Background Monitor Function
#'
#' @param pid Target Process ID
#' @param interval Polling interval
#' @param data_path Output file path
#' @noRd
monitor_bg_routine <- function(
  pid,
  interval,
  data_path
) {
  write.csv(
    data.frame(
      timestamp = numeric(0),
      memory_bytes = numeric(0)
    ),
    file = data_path,
    row.names = FALSE
  )
  running <- TRUE
  while (running) {
    tryCatch(
      {
        handle <- ps::ps_handle(pid)
        if (!ps::ps_is_running(handle)) {
          running <- FALSE
        } else {
          mem <- ps::ps_memory_info(handle)[['rss']]
          mem_row <- data.frame(
            timestamp = as.numeric(Sys.time()),
            memory_bytes = mem
          )
          write.table(
            mem_row,
            file = data_path,
            append = TRUE,
            sep = ',',
            col.names = FALSE,
            row.names = FALSE
          )
        }
      },
      error = function(e) {
        log_path <- sub("\\.csv$", ".log", data_path)
        cat(
          as.character(Sys.time()),
          ": ",
          e$message,
          "\n",
          file = log_path,
          append = TRUE
        )
        running <<- FALSE
      }
    )
    if (running) Sys.sleep(interval)
  }
}

#' Start Monitoring RAM
#'
#' @param pid Process ID to monitor. Defaults to current R session.
#' @param process_name Optional process name. If provided, pid is ignored.
#' @param interval Polling interval in seconds. Default is 1.
#' @param data_dir Directory to store the memory log. Default is tempdir().
#'
#' @return A RamMonitor S4 object.
#' @export
#' @importFrom callr r_bg
start_monitoring <- function(
  pid = NULL,
  process_name = NULL,
  interval = 1,
  data_dir = tempdir()
) {
  if (is.null(pid) && is.null(process_name)) {
    pid <- Sys.getpid()
  } else if (!is.null(process_name)) {
    procs <- ps::ps()
    matches <- procs[procs$name == process_name, ]
    if (nrow(matches) == 0) {
      stop("Process not found")
    }
    pid <- matches$pid[1]
  }

  if (!ps::ps_is_running(ps::ps_handle(pid))) {
    stop("Target PID is not running.")
  }

  ram_file_name <- paste0(
    "ram_monitor_",
    as.integer(Sys.time()),
    "_",
    pid,
    ".csv"
  )
  data_path <- file.path(
    data_dir,
    ram_file_name
  )
  data_path <- normalizePath(
    data_path,
    winslash = "/",
    mustWork = FALSE
  )

  px <- callr::r_bg(
    func = monitor_bg_routine,
    args = list(
      pid = pid,
      interval = interval,
      data_path = data_path
    )
  )

  env_px <- new.env(parent = emptyenv())
  env_px$px <- px
  env_px$data_path <- data_path

  # Add a finalizer to kill the background process if the object is garbage collected
  reg.finalizer(
    env_px,
    function(e) {
      if (e$px$is_alive()) {
        e$px$kill()
      }
    },
    onexit = TRUE
  )

  bookmarks_env <- new.env(parent = emptyenv())

  monitor <- RamMonitor(
    pid = as.numeric(pid),
    process = env_px,
    data_path = data_path,
    bookmarks = bookmarks_env
  )

  return(monitor)
}

#' Stop Monitoring (Generic)
#'
#' @param monitor A RamMonitor object
#' @param cleanup Logical. If TRUE, deletes the temporary data file. Default is FALSE.
#' @export
setGeneric("stop_monitoring", function(monitor, cleanup = FALSE) {
  standardGeneric("stop_monitoring")
})

#' @rdname stop_monitoring
#' @export
setMethod("stop_monitoring", "RamMonitor", function(monitor, cleanup = FALSE) {
  if (monitor@process$px$is_alive()) {
    monitor@process$px$kill()
  }

  if (cleanup && file.exists(monitor@data_path)) {
    unlink(monitor@data_path)
  }
})

#' Bookmark Time (Generic)
#'
#' @param monitor A RamMonitor object
#' @param label A string label
#' @export
setGeneric("bookmark_time", function(monitor, label) {
  standardGeneric("bookmark_time")
})

#' @rdname bookmark_time
#' @export
setMethod(
  "bookmark_time",
  signature(monitor = "RamMonitor", label = "character"),
  function(monitor, label) {
    ts <- as.numeric(Sys.time())
    monitor@bookmarks[[label]] <- ts
    invisible(ts)
  }
)

#' Get Current RAM Usage (Generic)
#' @param monitor A RamMonitor object
#' @return Numeric. Current RAM usage in GB.
#' @export
setGeneric("get_current_ram_usage", function(monitor) {
  standardGeneric("get_current_ram_usage")
})

#' @rdname get_current_ram_usage
#' @export
setMethod("get_current_ram_usage", "RamMonitor", function(monitor) {
  ram_data <- read_monitor_data(monitor)
  if (nrow(ram_data) == 0) {
    return(NA_real_)
  }
  return(ram_data$memory_bytes[nrow(ram_data)] / 1024^3)
})

#' Get Maximum RAM Usage (Generic)
#' @param monitor A RamMonitor object
#' @return Numeric. Maximum RAM usage in GB.
#' @export
setGeneric("get_max_ram_usage", function(monitor) {
  standardGeneric("get_max_ram_usage")
})

#' @rdname get_max_ram_usage
#' @export
setMethod("get_max_ram_usage", "RamMonitor", function(monitor) {
  ram_data <- read_monitor_data(monitor)
  if (nrow(ram_data) == 0) {
    return(NA_real_)
  }
  return(
    max(
      ram_data$memory_bytes,
      na.rm = TRUE
    ) /
      1024^3
  )
})

#' Get Detailed RAM Data (Generic)
#'
#' Retrieves the memory usage data as a tibble, including a column
#' indicating the "stage" of execution based on bookmarked timepoints.
#'
#' @param monitor A RamMonitor object
#' @return A tibble with columns `timestamp`, `memory_bytes`, `time`, and `stage`
#' @export
#' @importFrom dplyr arrange mutate left_join select
#' @importFrom tibble as_tibble
setGeneric("get_ram_data", function(monitor) standardGeneric("get_ram_data"))

#' @rdname get_ram_data
#' @export
setMethod("get_ram_data", "RamMonitor", function(monitor) {
  ram_data <- read_monitor_data(monitor)

  if (nrow(ram_data) == 0) {
    return(
      tibble::tibble(
        timestamp = numeric(0),
        memory_bytes = numeric(0),
        time = as.POSIXct(character(0)),
        stage = character(0)
      )
    )
  }

  ram_tbl <- tibble::as_tibble(ram_data)
  ram_tbl$time <- as.POSIXct(
    ram_tbl$timestamp,
    origin = "1970-01-01"
  )

  bookmarks_list <- as.list(monitor@bookmarks)
  if (length(bookmarks_list) == 0) {
    ram_tbl$stage <- "Initialization"
    return(ram_tbl)
  }

  bookmarks_data <- data.frame(
    stage = names(bookmarks_list),
    timestamp = unlist(bookmarks_list),
    stringsAsFactors = FALSE
  )

  bookmarks_data <- bookmarks_data[order(bookmarks_data$timestamp), ]

  # Assign stages based on timestamp intervals
  ram_tbl$stage <- "Initialization"
  for (i in seq_len(nrow(bookmarks_data))) {
    bm_time <- bookmarks_data$timestamp[i]
    bm_label <- bookmarks_data$stage[i]
    ram_tbl$stage[ram_tbl$timestamp >= bm_time] <- bm_label
  }

  return(ram_tbl)
})

# Global variable declarations for R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "timestamp",
    "memory_mb",
    "time",
    "memory_bytes",
    "stage",
    "label"
  ))
}
