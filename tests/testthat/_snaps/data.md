# get_data_labels

    Code
      get_data_labels(tplyr_adsl)
    Output
      # A tibble: 49 x 2
         name    label                              
         <chr>   <chr>                              
       1 STUDYID Study Identifier                   
       2 USUBJID Unique Subject Identifier          
       3 SUBJID  Subject Identifier for the Study   
       4 SITEID  Study Site Identifier              
       5 SITEGR1 Pooled Site Group 1                
       6 ARM     Description of Planned Arm         
       7 TRT01P  Planned Treatment for Period 01    
       8 TRT01PN Planned Treatment for Period 01 (N)
       9 TRT01A  Actual Treatment for Period 01     
      10 TRT01AN Actual Treatment for Period 01 (N) 
      # i 39 more rows

---

    Code
      get_data_labels(tplyr_adae)
    Output
      # A tibble: 55 x 2
         name    label                    
         <chr>   <chr>                    
       1 STUDYID Study Identifier         
       2 SITEID  Study Site Identifier    
       3 USUBJID Unique Subject Identifier
       4 TRTA    Actual Treatment         
       5 TRTAN   Actual Treatment (N)     
       6 AGE     Age                      
       7 AGEGR1  Pooled Age Group 1       
       8 AGEGR1N Pooled Age Group 1 (N)   
       9 RACE    Race                     
      10 RACEN   Race (N)                 
      # i 45 more rows

---

    Code
      get_data_labels(tplyr_adas)
    Output
      # A tibble: 40 x 2
         name    label                              
         <chr>   <chr>                              
       1 STUDYID Study Identifier                   
       2 SITEID  Study Site Identifier              
       3 SITEGR1 Pooled Site Group 1                
       4 USUBJID Unique Subject Identifier          
       5 TRTSDT  Date of First Exposure to Treatment
       6 TRTEDT  Date of Last Exposure to Treatment 
       7 TRTP    Planned Treatment                  
       8 TRTPN   Planned Treatment (N)              
       9 AGE     Age                                
      10 AGEGR1  Pooled Age Group 1                 
      # i 30 more rows

---

    Code
      get_data_labels(tplyr_adlb)
    Output
      # A tibble: 46 x 2
         name    label                              
         <chr>   <chr>                              
       1 STUDYID Study Identifier                   
       2 SUBJID  Subject Identifier for the Study   
       3 USUBJID Unique Subject Identifier          
       4 TRTP    Planned Treatment                  
       5 TRTPN   Planned Treatment (N)              
       6 TRTA    Actual Treatment                   
       7 TRTAN   Actual Treatment (N)               
       8 TRTSDT  Date of First Exposure to Treatment
       9 TRTEDT  Date of Last Exposure to Treatment 
      10 AGE     Age                                
      # i 36 more rows

