# tplyr_table is printed as expected

    *** tplyr_table ***
    Target (data.frame):
    	Name:  mtcars
    	Rows:  32
    	Columns:  11 
    treat_var variable (quosure)
    	gear
    header_n:  header groups
    treat_grps groupings (list)
    	Total
    Table Columns (cols):
    	vs
    where: TRUE
    Number of layer(s): 1
    layer_output: 0

---

    *** target data.frame ***
    Target Name:  mtcars 
    'data.frame':	6 obs. of  11 variables:
     $ mpg : num  21 21 22.8 21.4 18.7 18.1
     $ cyl : num  6 6 4 6 8 6
     $ disp: num  160 160 108 258 360 225
     $ hp  : num  110 110 93 110 175 105
     $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76
     $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
     $ qsec: num  16.5 17 18.6 19.4 17 ...
     $ vs  : num  0 0 1 1 0 1
     $ am  : num  1 1 1 0 0 0
     $ gear: num  4 4 4 3 3 3
     $ carb: num  4 4 1 1 2 1
    *** treat_var***
    gear
    *** pop_data data.frame ***
    'data.frame':	32 obs. of  11 variables:
     $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
     $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
     $ disp: num  160 160 108 258 360 ...
     $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
     $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
     $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
     $ qsec: num  16.5 17 18.6 19.4 17 ...
     $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
     $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
     $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
     $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
    *** pop_treat_var ***
    gear
    *** treat_grps ***
     Total:
    	 4 3 5

---

    *** tplyr_table ***
    Target (data.frame):
    	Name:  mtcars
    	Rows:  32
    	Columns:  11 
    treat_var variable (quosure)
    	gear
    header_n: 8 header groups
    treat_grps groupings (list)
    	Total
    Table Columns (cols):
    	vs
    where: TRUE
    Number of layer(s): 1
    layer_output: 0

---

    *** target data.frame ***
    Target Name:  mtcars 
    'data.frame':	6 obs. of  11 variables:
     $ mpg : num  21 21 22.8 21.4 18.7 18.1
     $ cyl : num  6 6 4 6 8 6
     $ disp: num  160 160 108 258 360 225
     $ hp  : num  110 110 93 110 175 105
     $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76
     $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
     $ qsec: num  16.5 17 18.6 19.4 17 ...
     $ vs  : num  0 0 1 1 0 1
     $ am  : num  1 1 1 0 0 0
     $ gear: num  4 4 4 3 3 3
     $ carb: num  4 4 1 1 2 1
    *** treat_var***
    gear
    *** pop_data data.frame ***
    'data.frame':	32 obs. of  11 variables:
     $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
     $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
     $ disp: num  160 160 108 258 360 ...
     $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
     $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
     $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
     $ qsec: num  16.5 17 18.6 19.4 17 ...
     $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
     $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
     $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
     $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
    *** pop_treat_var ***
    gear
    *** treat_grps ***
     Total:
    	 4 3 5

# tplyr layers are printed as expected

    *** count_layer ***
    
    target_var: 
     cyl
    by: am
    where: TRUE
    Layer(s): 0

---

    *** tplyr_layer ***
    	Target Name:  mtcars
    *** target_var ***
     cyl
    *** by ***
    am
    *** where ***
    TRUE

# f_str objects are printed as expected

    $n_counts
    *** Format String ***
    xx (xx.xx%) [xxx] [xx.xx%]
    *** vars, extracted formats, and settings ***
    distinct_n formated as: xx
    	integer length: 2
    	decimal length: 0
    distinct_pct formated as: xx.xx
    	integer length: 2
    	decimal length: 2
    n formated as: xxx
    	integer length: 3
    	decimal length: 0
    pct formated as: xx.xx
    	integer length: 2
    	decimal length: 2
    Total Format Size: 26

---

    List of 1
     $ n_counts:*** Format String ***
    xx (xx.xx%) [xxx] [xx.xx%]
    *** vars, extracted formats, and settings ***
    distinct_n formated as: xx
    	integer length: 2
    	decimal length: 0
    distinct_pct formated as: xx.xx
    	integer length: 2
    	decimal length: 2
    n formated as: xxx
    	integer length: 3
    	decimal length: 0
    pct formated as: xx.xx
    	integer length: 2
    	decimal length: 2
    Total Format Size: 26

