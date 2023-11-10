# Load packages
library(magrittr)
library(dplyr)
library(tibble)

# Create sample data set
groupings <- c(rep("Group1",5), rep("Group2",5), rep("Group3",5))
subgroupings <- c("subgroup1", "subgroup2", "subgroup3", "subgroup3", "subgroup2","subgroup1", "subgroup1", "subgroup1", "subgroup1", "subgroup3", "subgroup2", "subgroup3", "subgroup2", "subgroup1", "subgroup1")
value_set1 <- c(59.56, 38.74, 54.49, 52.52, 59.54, NA, 42.10, 35.06, 57.97, 57.52, 57.64, 38.62, NA, 41.15, NA)
value_set2 <- c(2,5,7,NA,35,8,NA,15,67,8,9,36,14,23,6)
ex_data <- tibble(main_groups = groupings, sub_groups = subgroupings, values1 = value_set1, values2 = value_set2)


## Manual calculations (to be compared with function calculations)

# Test 1: With both groups and both variables without NAs
manual1 <- ex_data %>%
  group_by(main_groups, sub_groups) %>%
  summarize(across(values1:values2, ~ max(.x, na.rm = TRUE)))
# Test 2: With one group and one variable with NAs
manual2 <- ex_data %>%
  group_by(main_groups) %>%
  summarize(across(values2, ~ max(.x, na.rm = FALSE)))
# Test 3: Test 1 with data set as a data frame
manual3 <- as.data.frame(ex_data) %>%
  group_by(main_groups, sub_groups) %>%
  summarize(across(values1:values2, ~ max(.x, na.rm = TRUE)))

## Function calculations

# Test 1
function1 <- max_values(data = ex_data, main_groups, sub_groups, vars = values1:values2, na.rm = TRUE)
# Test 2
function2 <- max_values(data = ex_data, main_groups, vars = values2, na.rm = FALSE)
# Test 3
function3 <- max_values(data = as.data.frame(ex_data), main_groups, sub_groups, vars = values1:values2, na.rm = TRUE)

# Tests for equality
test_that("Output is correct", {
  expect_equal(manual1, function1)
  expect_equal(manual2, function2)
  expect_equal(manual3, function3)})
