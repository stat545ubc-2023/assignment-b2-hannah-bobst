# Tests from on assignment b1

## Manual calculations (to be compared with function calculations)

# Example 1
manual_1 <- palmerpenguins::penguins %>%
  group_by(species, island) %>%
  summarize(across(bill_length_mm:body_mass_g, ~ max(.x, na.rm = TRUE)))
# Example 2
manual_2 <- gapminder::gapminder %>%
  group_by(continent, country) %>%
  summarize(across(lifeExp:gdpPercap, ~ max(.x, na.rm = TRUE)))
# Example 3
manual_3 <- gapminder::gapminder %>%
  group_by(continent) %>%
  summarize(across(gdpPercap, ~ max(.x, na.rm = TRUE)))
# Example 1 (with NAs)
manual_1_na <- palmerpenguins::penguins %>%
  group_by(species, island) %>%
  summarize(across(bill_length_mm:body_mass_g, ~ max(.x, na.rm = FALSE)))
# Example 1 (as data frame)
manual_1_df <- as.data.frame(palmerpenguins::penguins) %>%
  group_by(species, island) %>%
  summarize(across(bill_length_mm:body_mass_g, ~ max(.x, na.rm = TRUE)))

## Function calculations

# Example 1
function_1 <- max_values(data = palmerpenguins::penguins, species, island, vars = bill_length_mm:body_mass_g, na.rm = TRUE)
# Example 2
function_2 <- max_values(data = gapminder::gapminder, continent, country, vars = lifeExp:gdpPercap, na.rm = TRUE)
# Example 3
function_3 <- max_values(data = gapminder::gapminder, continent, vars = gdpPercap, na.rm = TRUE)
# Example 1 (with NAs)
function_1_na <- max_values(data = palmerpenguins::penguins, species, island, vars = bill_length_mm:body_mass_g, na.rm = FALSE)
# Example 1 (with data argument as a data frame)
function_1_df <- max_values(data = as.data.frame(palmerpenguins::penguins), species, island, vars = bill_length_mm:body_mass_g, na.rm = TRUE)

# Tests for equality
test_that("Output is correct", {
  expect_equal(manual_1, function_1)
  expect_equal(manual_2, function_2)
  expect_equal(manual_3, function_3)
  expect_equal(manual_1_na, function_1_na)
  expect_equal(manual_1_df, function_1_df)})
