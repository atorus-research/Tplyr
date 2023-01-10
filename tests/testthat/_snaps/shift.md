# Shift layer clauses with invalid syntax give informative error

    i In index: 1.
    Caused by error in `value[[3L]]()`:
    ! group_shift `where` condition `bad == code` is invalid. Filter error:
    Error in `filter()`:
    ! Problem while computing `..1 = bad == code`.
    Caused by error in `mask$eval_all_filter()`:
    ! object 'bad' not found

