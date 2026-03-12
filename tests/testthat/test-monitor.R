test_that("RamMonitor core functionality works", {
  # Start monitor on current process
  monitor <- start_monitoring(interval = 0.5)
  expect_s4_class(monitor, "RamMonitor")
  
  # Wait for a poll
  Sys.sleep(1)
  
  # Bookmark
  bookmark_time(monitor, "test_alloc")
  expect_true("test_alloc" %in% names(monitor@bookmarks))
  
  # Check memory info
  Sys.sleep(2)
  cur_ram <- get_current_ram_usage(monitor)
  expect_true(is.numeric(cur_ram))
  
  max_ram <- get_max_ram_usage(monitor)
  expect_true(is.numeric(max_ram))
  expect_true(max_ram >= cur_ram)
  
  # Plot
  p <- plot_ram_usage(monitor)
  expect_s3_class(p, "ggplot")
  
  # Stop monitoring
  stop_monitoring(monitor)
  Sys.sleep(0.5)
  expect_false(monitor@process$px$is_alive())
})

test_that("RamMonitor edge cases handle errors gracefully", {
  # Invalid PID
  expect_error(start_monitoring(pid = 999999))
  
  # Invalid process name
  expect_error(start_monitoring(process_name = "non_existent_process_xyz"))
  
  # Empty/Missing data handling
  monitor <- start_monitoring(interval = 0.5)
  stop_monitoring(monitor)
  
  # Delete file and check getters
  if (file.exists(monitor@data_path)) unlink(monitor@data_path)
  
  expect_true(is.na(get_current_ram_usage(monitor)))
  expect_true(is.na(get_max_ram_usage(monitor)))
  
  df_empty <- get_ram_data(monitor)
  expect_s3_class(df_empty, "tbl_df")
  expect_equal(nrow(df_empty), 0)
  
  # Cleanup option
  monitor2 <- start_monitoring(interval = 0.5)
  Sys.sleep(3)
  data_path <- monitor2@data_path
  expect_true(file.exists(data_path))
  stop_monitoring(monitor2, cleanup = TRUE)
  expect_false(file.exists(data_path))
})

test_that("RamMonitor S4 generic dispatch works", {
  # Test with non-Monitor object
  expect_error(get_current_ram_usage(list()))
  expect_error(stop_monitoring("not a monitor"))
  expect_error(plot_ram_usage(list()))
})
