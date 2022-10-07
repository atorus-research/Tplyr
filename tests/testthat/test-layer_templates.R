op <- options()

# Template doesn't exist
load(test_path('adsl.Rdata'))

test_that("Template errors correctly upon creation", {

  # Enforce ellipsis
  # Invalid starting place
  expect_snapshot_error(
    new_layer_template(
      "test_template",
      set_format_strings()
      )
  )

  # Didn't use ellispsis
  expect_snapshot_error(
    new_layer_template(
      "test_template",
      group_count(adsl, TRT01P)
    )
  )

  # Non-Tplyr functions
  expect_snapshot_error(
    new_layer_template(
      "test_template",
      group_count(...) %>%
        print()
    )
  )

  # template exists warning
  expect_warning({
    new_layer_template("test_template", group_count(...))
    new_layer_template("test_template", group_count(...))
  })

  # This makes sense here - remove_layer_template effectively removes layer templates
  expect_silent(remove_layer_template("test_template"))
  expect_true(!("test_template" %in% names(getOption('tplyr.layer_templates'))))
  expect_warning(remove_layer_template("test_template"), "No template named")
})

test_that("Templates create effectively", {
  # Basic template
  expect_silent(
    new_layer_template(
      "test_template",
      group_count(...)
      )
  )

  expect_true('test_template' %in% names(getOption('tplyr.layer_templates')))

  remove_layer_template("test_template")

  # Templates identify additional params
  new_layer_template(
    "test_template",
    group_count(...) %>%
      set_order_count_method({sort_meth}) %>%
      set_ordering_cols({sort_col})
    )

  expect_equal(attr(get_layer_template('test_template'), 'params'), c("sort_meth", "sort_col"))

  remove_layer_template("test_template")
})

test_that("Template errors correctly upon execution", {
  new_layer_template(
    "test1",
    group_count(...) %>%
      set_format_strings(f_str("xx (xx.x%)", n, pct))
  )

  new_layer_template(
    "test2",
    group_count(...) %>%
      set_format_strings(f_str("xx (xx.x%)", n, pct)) %>%
      set_order_count_method({sort_meth}) %>%
      set_ordering_cols({sort_col})
  )

  expect_snapshot_error(
    tplyr_table(adsl, TRT01P) %>%
      add_layer(
        use_template('bad', RACE)
      )
  )

  # Args aren't in list
  expect_snapshot_error(
    tplyr_table(adsl, TRT01P) %>%
      add_layer(
        use_template('test2', RACE, add_params = "bad")
      )
  )

  expect_snapshot_error(
    tplyr_table(adsl, TRT01P) %>%
      add_layer(
        use_template('test2', RACE, add_params = vars(USUBJID))
      )
  )

  # Args must be named
  expect_snapshot_error(
    tplyr_table(adsl, TRT01P) %>%
      add_layer(
        use_template('test2', RACE, add_params = list("bycount"))
      )
  )

  # Invalid template
  options('tplyr.layer_templates' = append(getOption('tplyr.layer_templates'), list(bad = "bad")))

  # Args must be named
  expect_snapshot_error(
    tplyr_table(adsl, TRT01P) %>%
      add_layer(
        use_template('bad', RACE)
      )
  )

  remove_layer_template("bad")

  # Param mismatches
  # Extra param in call
  expect_snapshot_error(
    tplyr_table(adsl, TRT01P) %>%
      add_layer(
        use_template('test2', RACE, add_params = list(
          sort_meth = "bycount",
          sort_col = Placebo,
          test = vars(a, b, c)
        ))
      )
  )
  # Missing param in call
  expect_snapshot_error(
    tplyr_table(adsl, TRT01P) %>%
      add_layer(
        use_template('test2', RACE, add_params = list(
          sort_meth = "bycount"
        ))
      )
  )

  remove_layer_template("test1")
  remove_layer_template("test2")
})

test_that("Templates can be used correctly", {
  new_layer_template(
    "test1",
    group_count(...) %>%
      set_format_strings(f_str("xx (xx.x%)", n, pct))
  )

  new_layer_template(
    "test2",
    group_count(...) %>%
      set_format_strings(f_str("xx (xx.x%)", n, pct)) %>%
      set_order_count_method({sort_meth}) %>%
      set_ordering_cols({sort_col})
  )

  # NULL params on template without params
  t1 <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      group_count(RACE) %>%
        set_format_strings(f_str("xx (xx.x%)", n, pct))
    ) %>%
    build()

  t2 <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      use_template("test1", RACE)
    ) %>%
    build()

  expect_equal(t1, t2)

  # Params passed through - this tests both quoted and non-quoted args
  t3 <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      group_count(RACE, by=ETHNIC) %>%
        set_format_strings(f_str("xx (xx.x%)", n, pct)) %>%
        set_order_count_method("bycount") %>%
        set_ordering_cols(`Xanomeline Low Dose`)
    ) %>%
    build()

  t4 <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      use_template("test2", RACE, by=ETHNIC, add_params =
                     list(
                       sort_meth = "bycount",
                       sort_col = `Xanomeline Low Dose`
                     ))
    ) %>%
    build()

  expect_equal(t3, t4)
})

test_that("Templates are extensible", {

  t1 <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      group_count(RACE) %>%
        set_format_strings(f_str("xx (xx.x%)", n, pct)) %>%
        add_total_row()
    ) %>%
    build()

  t2 <- tplyr_table(adsl, TRT01P) %>%
    add_layer(
      use_template("test1", RACE) %>%
        add_total_row()
    ) %>%
    build()

  expect_equal(t1, t2)

})

test_that("Templates print appropriately", {
  expect_snapshot_output(get_layer_templates())
})

options(op)
