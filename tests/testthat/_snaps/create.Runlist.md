# ras.add.blank() snapshot

    Code
      full.list = ras.Example_Runlist
      blank.max = 5
      df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>%
        dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
      Runlist = tibble::tibble(Index = double(), Plate = double(), Row = character(),
      Col = double(), LC_Position = character(), Date = character(), Signature = character(),
      Sample_name = character(), Compound = character(), Timepoint = character(),
      Well_Type = character(), LC_Well_Type = character(), Replicate = character(),
      Sample_text = character(), Draw_Max = double(), Draw_Count = double())
      ras.add.blank(Runlist, df.blank, 3)
    Output
      # A tibble: 3 x 16
        Index Plate Row     Col LC_Pos~1 Date  Signa~2 Sampl~3 Compo~4 Timep~5 Well_~6
        <dbl> <dbl> <chr> <dbl> <chr>    <chr> <chr>   <chr>   <chr>   <chr>   <chr>  
      1    13     3 B         1 3:B,1    2022~ RH      202207~ <NA>    <NA>    blank  
      2    13     3 B         1 3:B,1    2022~ RH      202207~ <NA>    <NA>    blank  
      3    13     3 B         1 3:B,1    2022~ RH      202207~ <NA>    <NA>    blank  
      # ... with 5 more variables: LC_Well_Type <chr>, Replicate <chr>,
      #   Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>, and abbreviated
      #   variable names 1: LC_Position, 2: Signature, 3: Sample_name, 4: Compound,
      #   5: Timepoint, 6: Well_Type
      # i Use `colnames()` to see all variable names

# ras.add.type() snapshot

    Code
      full.list = ras.Example_Runlist
      blank.max = 5
      df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte")
      Runlist = tibble::tibble(Index = double(), Plate = double(), Row = character(),
      Col = double(), LC_Position = character(), Date = character(), Signature = character(),
      Sample_name = character(), Compound = character(), Timepoint = character(),
      Well_Type = character(), LC_Well_Type = character(), Replicate = character(),
      Sample_text = character(), Draw_Max = double(), Draw_Count = double())
      ras.add.type(Runlist, df.analyte, "Paracetamol", "cell")
    Output
      # A tibble: 18 x 16
         Index Plate Row     Col LC_Po~1 Date  Signa~2 Sampl~3 Compo~4 Timep~5 Well_~6
         <dbl> <dbl> <chr> <dbl> <chr>   <chr> <chr>   <chr>   <chr>   <chr>   <chr>  
       1     1     3 A         1 3:A,1   2022~ RH      202207~ Parace~ 0       cell   
       2     2     3 A         2 3:A,2   2022~ RH      202207~ Parace~ 15      cell   
       3     3     3 A         3 3:A,3   2022~ RH      202207~ Parace~ 30      cell   
       4     4     3 A         4 3:A,4   2022~ RH      202207~ Parace~ 60      cell   
       5     5     3 A         5 3:A,5   2022~ RH      202207~ Parace~ 90      cell   
       6     6     3 A         6 3:A,6   2022~ RH      202207~ Parace~ 120     cell   
       7    32     3 C         8 3:C,8   2022~ RH      202207~ Parace~ 0       cell   
       8    33     3 C         9 3:C,9   2022~ RH      202207~ Parace~ 15      cell   
       9    34     3 C        10 3:C,10  2022~ RH      202207~ Parace~ 30      cell   
      10    35     3 C        11 3:C,11  2022~ RH      202207~ Parace~ 60      cell   
      11    36     3 C        12 3:C,12  2022~ RH      202207~ Parace~ 90      cell   
      12    37     3 D         1 3:D,1   2022~ RH      202207~ Parace~ 120     cell   
      13    38     3 D         2 3:D,2   2022~ RH      202207~ Parace~ 0       cell   
      14    39     3 D         3 3:D,3   2022~ RH      202207~ Parace~ 15      cell   
      15    40     3 D         4 3:D,4   2022~ RH      202207~ Parace~ 30      cell   
      16    41     3 D         5 3:D,5   2022~ RH      202207~ Parace~ 60      cell   
      17    42     3 D         6 3:D,6   2022~ RH      202207~ Parace~ 90      cell   
      18    43     3 D         7 3:D,7   2022~ RH      202207~ Parace~ 120     cell   
      # ... with 5 more variables: LC_Well_Type <chr>, Replicate <chr>,
      #   Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>, and abbreviated
      #   variable names 1: LC_Position, 2: Signature, 3: Sample_name, 4: Compound,
      #   5: Timepoint, 6: Well_Type
      # i Use `colnames()` to see all variable names

# ras.add.compound() snapshot (1 blank, default)

    Code
      full.list = ras.Example_Runlist
      blank.max = 5
      df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>%
        dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
      df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte")
      Runlist = tibble::tibble(Index = double(), Plate = double(), Row = character(),
      Col = double(), LC_Position = character(), Date = character(), Signature = character(),
      Sample_name = character(), Compound = character(), Timepoint = character(),
      Well_Type = character(), LC_Well_Type = character(), Replicate = character(),
      Sample_text = character(), Draw_Max = double(), Draw_Count = double())
      ras.add.compound(Runlist, df.analyte, df.blank, "Paracetamol", c("bead",
        "medium", "cell", "STD", "blank"))
    Output
      # A tibble: 47 x 16
         Index Plate Row     Col LC_Po~1 Date  Signa~2 Sampl~3 Compo~4 Timep~5 Well_~6
         <dbl> <dbl> <chr> <dbl> <chr>   <chr> <chr>   <chr>   <chr>   <chr>   <chr>  
       1    25     3 C         1 3:C,1   2022~ RH      202207~ Parace~ 0       bead   
       2    26     3 C         2 3:C,2   2022~ RH      202207~ Parace~ 0       bead   
       3    27     3 C         3 3:C,3   2022~ RH      202207~ Parace~ 0       bead   
       4    28     3 C         4 3:C,4   2022~ RH      202207~ Parace~ 0       bead   
       5    29     3 C         5 3:C,5   2022~ RH      202207~ Parace~ 0       bead   
       6    30     3 C         6 3:C,6   2022~ RH      202207~ Parace~ 0       bead   
       7    31     3 C         7 3:C,7   2022~ RH      202207~ Parace~ 0       bead   
       8   136     4 D         4 4:D,4   2022~ RH      202207~ Parace~ 0       bead   
       9   137     4 D         5 4:D,5   2022~ RH      202207~ Parace~ 0       bead   
      10   138     4 D         6 4:D,6   2022~ RH      202207~ Parace~ 0       bead   
      # ... with 37 more rows, 5 more variables: LC_Well_Type <chr>, Replicate <chr>,
      #   Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>, and abbreviated
      #   variable names 1: LC_Position, 2: Signature, 3: Sample_name, 4: Compound,
      #   5: Timepoint, 6: Well_Type
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# ras.add.compound() snapshot (3 blanks)

    Code
      full.list = ras.Example_Runlist
      blank.max = 5
      df.blank = dplyr::filter(full.list, full.list$LC_Well_Type == "blank") %>%
        dplyr::mutate(Draw_Max = blank.max, Draw_Count = 0)
      df.analyte = dplyr::filter(full.list, full.list$LC_Well_Type == "Analyte")
      Runlist = tibble::tibble(Index = double(), Plate = double(), Row = character(),
      Col = double(), LC_Position = character(), Date = character(), Signature = character(),
      Sample_name = character(), Compound = character(), Timepoint = character(),
      Well_Type = character(), LC_Well_Type = character(), Replicate = character(),
      Sample_text = character(), Draw_Max = double(), Draw_Count = double())
      ras.add.compound(Runlist, df.analyte, df.blank, "Ibuprofen", c("bead", "medium",
        "cell", "STD", "blank"), 3)
    Output
      # A tibble: 55 x 16
         Index Plate Row     Col LC_Po~1 Date  Signa~2 Sampl~3 Compo~4 Timep~5 Well_~6
         <dbl> <dbl> <chr> <dbl> <chr>   <chr> <chr>   <chr>   <chr>   <chr>   <chr>  
       1    62     3 F         2 3:F,2   2022~ RH      202207~ Ibupro~ 0       bead   
       2    63     3 F         3 3:F,3   2022~ RH      202207~ Ibupro~ 0       bead   
       3    64     3 F         4 3:F,4   2022~ RH      202207~ Ibupro~ 0       bead   
       4    65     3 F         5 3:F,5   2022~ RH      202207~ Ibupro~ 0       bead   
       5    66     3 F         6 3:F,6   2022~ RH      202207~ Ibupro~ 0       bead   
       6    67     3 F         7 3:F,7   2022~ RH      202207~ Ibupro~ 0       bead   
       7    68     3 F         8 3:F,8   2022~ RH      202207~ Ibupro~ 0       bead   
       8    69     3 F         9 3:F,9   2022~ RH      202207~ Ibupro~ 0       bead   
       9    70     3 F        10 3:F,10  2022~ RH      202207~ Ibupro~ 0       bead   
      10    71     3 F        11 3:F,11  2022~ RH      202207~ Ibupro~ 0       bead   
      # ... with 45 more rows, 5 more variables: LC_Well_Type <chr>, Replicate <chr>,
      #   Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>, and abbreviated
      #   variable names 1: LC_Position, 2: Signature, 3: Sample_name, 4: Compound,
      #   5: Timepoint, 6: Well_Type
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# ras.create.Runlist() snapshot (default)

    Code
      full.list = ras.Example_Runlist
      ras.create.Runlist(full.list)
    Output
      # A tibble: 150 x 16
         Index Plate Row     Col LC_Po~1 Date  Signa~2 Sampl~3 Compo~4 Timep~5 Well_~6
         <dbl> <dbl> <chr> <dbl> <chr>   <chr> <chr>   <chr>   <chr>   <chr>   <chr>  
       1    13     3 B         1 3:B,1   2022~ RH      202207~ <NA>    <NA>    blank  
       2    13     3 B         1 3:B,1   2022~ RH      202207~ <NA>    <NA>    blank  
       3    13     3 B         1 3:B,1   2022~ RH      202207~ <NA>    <NA>    blank  
       4    62     3 F         2 3:F,2   2022~ RH      202207~ Ibupro~ 0       bead   
       5    63     3 F         3 3:F,3   2022~ RH      202207~ Ibupro~ 0       bead   
       6    64     3 F         4 3:F,4   2022~ RH      202207~ Ibupro~ 0       bead   
       7    65     3 F         5 3:F,5   2022~ RH      202207~ Ibupro~ 0       bead   
       8    66     3 F         6 3:F,6   2022~ RH      202207~ Ibupro~ 0       bead   
       9    67     3 F         7 3:F,7   2022~ RH      202207~ Ibupro~ 0       bead   
      10    68     3 F         8 3:F,8   2022~ RH      202207~ Ibupro~ 0       bead   
      # ... with 140 more rows, 5 more variables: LC_Well_Type <chr>,
      #   Replicate <chr>, Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>, and
      #   abbreviated variable names 1: LC_Position, 2: Signature, 3: Sample_name,
      #   4: Compound, 5: Timepoint, 6: Well_Type
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

# ras.create.Runlist() snapshot

    Code
      full.list = ras.Example_Runlist
      ras.create.Runlist(full.list, blank.start = 4, blank.end = 2, blank.comp = 3,
        blank.type = 2, blank.max = 3)
    Output
      # A tibble: 159 x 16
         Index Plate Row     Col LC_Po~1 Date  Signa~2 Sampl~3 Compo~4 Timep~5 Well_~6
         <dbl> <dbl> <chr> <dbl> <chr>   <chr> <chr>   <chr>   <chr>   <chr>   <chr>  
       1    13     3 B         1 3:B,1   2022~ RH      202207~ <NA>    <NA>    blank  
       2    13     3 B         1 3:B,1   2022~ RH      202207~ <NA>    <NA>    blank  
       3    13     3 B         1 3:B,1   2022~ RH      202207~ <NA>    <NA>    blank  
       4    14     3 B         2 3:B,2   2022~ RH      202207~ <NA>    <NA>    blank  
       5    62     3 F         2 3:F,2   2022~ RH      202207~ Ibupro~ 0       bead   
       6    63     3 F         3 3:F,3   2022~ RH      202207~ Ibupro~ 0       bead   
       7    64     3 F         4 3:F,4   2022~ RH      202207~ Ibupro~ 0       bead   
       8    65     3 F         5 3:F,5   2022~ RH      202207~ Ibupro~ 0       bead   
       9    66     3 F         6 3:F,6   2022~ RH      202207~ Ibupro~ 0       bead   
      10    67     3 F         7 3:F,7   2022~ RH      202207~ Ibupro~ 0       bead   
      # ... with 149 more rows, 5 more variables: LC_Well_Type <chr>,
      #   Replicate <chr>, Sample_text <chr>, Draw_Max <dbl>, Draw_Count <dbl>, and
      #   abbreviated variable names 1: LC_Position, 2: Signature, 3: Sample_name,
      #   4: Compound, 5: Timepoint, 6: Well_Type
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

