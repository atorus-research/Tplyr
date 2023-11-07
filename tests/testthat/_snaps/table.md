# tplyr_table throws error when passed a bad table argument

    unused argument (a = 1:10)

# Table level where clauses with invalid syntax give informative error

    tplyr_table `where` condition `bad == code` is invalid. Filter error:
    Error in `filter()`:
    i In argument: `bad == code`.
    Caused by error:
    ! object 'bad' not found
    

# Population data where clauses with invalid syntax give informative error

    Population data `pop_where` condition `bad == code` is invalid. Filter error:
    Error in `filter()`:
    i In argument: `bad == code`.
    Caused by error:
    ! object 'bad' not found
    If the population data and target data subsets should be different, use `set_pop_where`.

