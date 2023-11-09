#' @title Group data and summarize maximums of selected variables
#'
#' @description max_values groups a data frame (or data frame extension) and maximizes the variables. The groups and variables to be maximized are specified in the function arguments.
#'
#' @param data A data frame or tibble.
#' @param vars A character vector with the variables (vars) to be maximized. These variable names must correspond to columns with numerical entries.
#' @param ... Additional function arguments specifying the groups. These arguments must be character values corresponding to column names.
#' @param na.rm A Boolean variable matching the argument name "na.rm" in the "max" function. Default input is TRUE, which removes NA values.
#' @return A tibble summarizing the maximum values of variable by groups.
#' @import magrittr
#' @import dplyr
#' @import tibble
#' @examples
#' library(tibble)
#' ex_data <- tibble(main_groups = c(rep("Group1",5), rep("Group2",5), rep("Group3",5)), sub_groups = c("subgroup1", "subgroup2", "subgroup3", "subgroup3", "subgroup2","subgroup1", "subgroup1", "subgroup1", "subgroup1", "subgroup3", "subgroup2", "subgroup3", "subgroup2", "subgroup1", "subgroup1"), values1 = c(59.55907, 38.73754, 54.49307, 52.52262, 59.53699, NA, 42.09835, 35.06464, 57.96973, 57.52726, 57.64511, 38.66776, NA, 41.15472, NA), values2 = c(2,5,7,NA,35,8,NA,15,67,8,9,36,14,23,6))
#' max_values(data = ex_data, main_groups, sub_groups, vars = values1:values2, na.rm = TRUE)
#' max_values(data = ex_data, main_groups, vars = values1:values2, na.rm = FALSE)
#' max_values(data = ex_data, sub_groups, vars = values2, na.rm = TRUE)
#' @export
max_values <- function(data, ..., vars, na.rm = TRUE){
  # Produce maximum of user-specified variables for user-specified groupings
  data %>%
    group_by(...) %>%
    summarize(across({{vars}}, ~ max(.x, na.rm = na.rm)))
}
