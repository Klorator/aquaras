# well.update() snapshot test

    Code
      well.update(df = Example_Runlist, well.current = "3:A,1", date = "20220730",
        signature = "RH", compound = "Warfarin", timepoint = "42", type = "bead",
        replicate = "6")
    Warning <lifecycle_warning_deprecated>
      The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
      The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
      The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
      The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
      The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
      The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
      The `i` argument of `[` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
      The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
      The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
      The `i` argument of `[<-` can't be a matrix as of tibble 3.0.0.
      Convert to a vector.
    Output
      # A tibble: 672 x 14
         Index Plate Row     Col LC_Po~1 Date  Signa~2 Sampl~3 Compo~4 Timep~5 Well_~6
         <dbl> <dbl> <chr> <dbl> <chr>   <chr> <chr>   <chr>   <chr>   <chr>   <chr>  
       1     1     3 A         1 3:A,1   2022~ RH      202207~ Warfar~ 42      bead   
       2     2     3 A         2 3:A,2   2022~ RH      202207~ Parace~ 15      cell   
       3     3     3 A         3 3:A,3   2022~ RH      202207~ Parace~ 30      cell   
       4     4     3 A         4 3:A,4   2022~ RH      202207~ Parace~ 60      cell   
       5     5     3 A         5 3:A,5   2022~ RH      202207~ Parace~ 90      cell   
       6     6     3 A         6 3:A,6   2022~ RH      202207~ Parace~ 120     cell   
       7     7     3 A         7 3:A,7   2022~ RH      202207~ Parace~ 0       STD    
       8     8     3 A         8 3:A,8   2022~ RH      202207~ Parace~ 0       STD    
       9     9     3 A         9 3:A,9   2022~ RH      202207~ Parace~ 0       STD    
      10    10     3 A        10 3:A,10  2022~ RH      202207~ Parace~ 0       STD    
      # ... with 662 more rows, 3 more variables: LC_Well_Type <chr>,
      #   Replicate <chr>, Sample_text <chr>, and abbreviated variable names
      #   1: LC_Position, 2: Signature, 3: Sample_name, 4: Compound, 5: Timepoint,
      #   6: Well_Type
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# dateSignAll.update() snapshot

    Code
      dateSignAll.update(df = Example_Runlist, date = "20220730", signature = "RH")
    Output
      # A tibble: 672 x 14
         Index Plate Row     Col LC_Po~1 Date  Signa~2 Sampl~3 Compo~4 Timep~5 Well_~6
         <dbl> <dbl> <chr> <dbl> <chr>   <chr> <chr>   <chr>   <chr>   <chr>   <chr>  
       1     1     3 A         1 3:A,1   2022~ RH      202207~ Parace~ 0       cell   
       2     2     3 A         2 3:A,2   2022~ RH      202207~ Parace~ 15      cell   
       3     3     3 A         3 3:A,3   2022~ RH      202207~ Parace~ 30      cell   
       4     4     3 A         4 3:A,4   2022~ RH      202207~ Parace~ 60      cell   
       5     5     3 A         5 3:A,5   2022~ RH      202207~ Parace~ 90      cell   
       6     6     3 A         6 3:A,6   2022~ RH      202207~ Parace~ 120     cell   
       7     7     3 A         7 3:A,7   2022~ RH      202207~ Parace~ 0       STD    
       8     8     3 A         8 3:A,8   2022~ RH      202207~ Parace~ 0       STD    
       9     9     3 A         9 3:A,9   2022~ RH      202207~ Parace~ 0       STD    
      10    10     3 A        10 3:A,10  2022~ RH      202207~ Parace~ 0       STD    
      # ... with 662 more rows, 3 more variables: LC_Well_Type <chr>,
      #   Replicate <chr>, Sample_text <chr>, and abbreviated variable names
      #   1: LC_Position, 2: Signature, 3: Sample_name, 4: Compound, 5: Timepoint,
      #   6: Well_Type
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# sanitizeInput() snapshot

    Code
      sanitizeInput("Spaces etc. / -- but_no_underscore!")
    Output
      [1] "Spaces etc. / -- but.no.underscore!"

