# Print tests

t <- tplyr_table(mtcars, gear, cols = vs, where = am == 1)

c1 <- group_count(t, cyl, by = hp)

t <- add_layers(t, c1)


# test_that("A tplyr_table prints to console as expected", {
#   print_t <- "*** tplyr_table ***
# Target (data.frame):
# 	Name:  mtcars
# 	Rows:  32
# 	Columns:  11
# pop_data (data.frame)
# 	Name:  target
# 	Rows:  32
# 	Columns:  11
# treat_var variable (quosure)
# 	gear
# header_n: 6 header groups
# treat_grps groupings (list)
# Table Columns (cols):
# 	vs
# where: == am 1
# Number of layer(s): 1
# layer_output: 0"
#
#   expect_equal(print(t), print_t)
# })
