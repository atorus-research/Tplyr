# Template errors correctly upon creation

    Invalid template - templates must start with an ellipsis (i.e. ...) passed to either group_count, group_desc, or group_shift. For example, group_count(...)

---

    Invalid template - templates must start with an ellipsis (i.e. ...) passed to either group_count, group_desc, or group_shift. For example, group_count(...)

---

    Functions called within `add_layer` must be part of `Tplyr`

# Template errors correctly upon execution

    Template bad does not exist

---

    Arguments must be passed to `add_params` in a list.

---

    Arguments must be passed to `add_params` in a list.

---

    Arguments pass in `add_params` must be named

---

    Invalid template - templates must be created using `new_layer_template()`

---

    In use_template() the following parameters provided to add_params are invalid: test

---

    In use_template() the following parameters provided to add_params are missing: sort_col

# Templates print appropriately

    $test1
    Template name: test1
    Template parameters: None
    Template code:
    {
    group_count(...) %>% set_format_strings(f_str("xx (xx.x%)", n, pct))
    } 
    
    $test2
    Template name: test2
    Template parameters: sort_meth, sort_col
    Template code:
    {
    group_count(...) %>% set_format_strings(f_str("xx (xx.x%)", n, pct)) %>% set_order_count_method({
        sort_meth
    }) %>% set_ordering_cols({
        sort_col
    })
    } 
    

