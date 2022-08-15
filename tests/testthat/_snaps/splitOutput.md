# loadFile.ML() loads dataLines

    Code
      loadFile.ML(sourceFile = system.file("extdata", "Example_MLOutput.txt",
        package = "aquaras", mustWork = TRUE))
    Output
      [[1]]
      [1] "Quantify Compound Summary Report \t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
      
      [[2]]
      [1] "Printed Tue Mar 15 16:33:55 2022\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
      
      [[3]]
      [1] "Compound 1:  Ibuprofen\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
      
      [[4]]
      [1] "\tName\tSample Text\tType\tStd. Conc\tRT\tArea\tIS Area\tResponse\tPrimary Flags\tnM\t%Dev\tS/N\tConc. Dev. Flagged\tConc. Dev. Allowed\tPk Width\tVial\tAcq.Date\tAcq.Time\tID"
      
      [[5]]
      [1] "1\t20220725_RH_Index_001\tblank\tBlank\t50\t3.97\t25432\t\t25432.006\tbb\t23.43604\t-53.1\t1329.93\tNO\t0\t2.884\t4:G,10\t23-feb-22\t11:56:26\t"
      
      [[6]]
      [1] "2\t20220725_RH_Index_002\tblank\tBlank\t50\t3.96\t25071\t\t25071.139\tbb\t23.1035\t-53.8\t1649.934\tNO\t0\t2.67\t4:G,10\t23-feb-22\t12:03:04\t"
      
      [[7]]
      [1] "3\t20220725_RH_Index_003\tblank\tBlank\t50\t3.95\t25559\t\t25559.383\tbb\t23.55342\t-52.9\t1943.923\tNO\t0\t2.86\t4:G,10\t23-feb-22\t12:09:07\t"
      
      [[8]]
      [1] "4\t20220725_RH_Index_004\tblank\tBlank\t50\t3.96\t25055\t\t25054.875\tbb\t23.08851\t-53.8\t2114.842\tNO\t0\t2.675\t4:G,10\t23-feb-22\t12:15:09\t"
      
      [[9]]
      [1] "5\t20220725_RH_Index_005\tblank\tBlank\t50\t3.96\t25825\t\t25825.176\tbb\t23.79835\t-52.4\t2097.918\tNO\t0\t2.721\t4:G,10\t23-feb-22\t12:21:12\t"
      
      [[10]]
      [1] "6\t20220725_RH_Index_006\tIbuprofen_0_cell_2\tAnalyte\t50\t3.96\t11171\t\t11170.61\tbb\t10.29391\t-79.4\t945.1\tNO\t0\t3.99\t4:A,1\t23-feb-22\t12:27:15\t"
      
      [[11]]
      [1] "7\t20220725_RH_Index_007\tIbuprofen_15_cell_2\tAnalyte\t50\t3.96\t10070\t\t10069.771\tbb\t9.27947\t-81.4\t793.704\tNO\t0\t5.115\t4:A,2\t23-feb-22\t12:33:17\t"
      
      [[12]]
      [1] "8\t20220725_RH_Index_008\tIbuprofen_30_cell_2\tAnalyte\t50\t3.96\t14646\t\t14645.96\tbb\t13.49651\t-73\t1731.491\tNO\t0\t3.411\t4:A,3\t23-feb-22\t12:39:20\t"
      
      [[13]]
      [1] "9\t20220725_RH_Index_009\tIbuprofen_60_cell_2\tAnalyte\t50\t3.96\t11718\t\t11718.259\tbb\t10.79858\t-78.4\t1202.853\tNO\t0\t3.531\t4:A,4\t23-feb-22\t12:45:23\t"
      
      [[14]]
      [1] "10\t20220725_RH_Index_010\tIbuprofen_90_cell_2\tAnalyte\t50\t3.96\t11348\t\t11347.764\tbb\t10.45716\t-79.1\t707.617\tNO\t0\t4.486\t4:A,5\t23-feb-22\t12:51:26\t"
      
      [[15]]
      [1] "11\t20220725_RH_Index_011\tblank\tBlank\t50\t3.96\t25342\t\t25341.746\tbb\t23.35286\t-53.3\t2057.022\tNO\t0\t2.651\t4:G,11\t23-feb-22\t12:57:29\t"
      
      [[16]]
      [1] "12\t20220725_RH_Index_012\tblank\tBlank\t50\t3.96\t25093\t\t25093.004\tbb\t23.12364\t-53.8\t2857.301\tNO\t0\t2.797\t4:G,11\t23-feb-22\t13:03:32\t"
      
      [[17]]
      [1] "13\t20220725_RH_Index_013\tIbuprofen_0_STD_\tAnalyte\t50\t3.96\t10373\t\t10373.271\tbb\t9.55915\t-80.9\t972.466\tNO\t0\t4.313\t4:A,6\t23-feb-22\t13:09:34\t"
      
      [[18]]
      [1] "14\t20220725_RH_Index_014\tIbuprofen_0_STD_\tAnalyte\t50\t3.96\t11953\t\t11952.766\tbb\t11.01468\t-78\t1179.287\tNO\t0\t3.852\t4:A,7\t23-feb-22\t13:15:37\t"
      
      [[19]]
      [1] "15\t20220725_RH_Index_015\tIbuprofen_0_STD_\tAnalyte\t50\t3.96\t10112\t\t10112.404\tbb\t9.31876\t-81.4\t1008.988\tNO\t0\t3.442\t4:A,8\t23-feb-22\t13:21:40\t"
      
      [[20]]
      [1] "16\t20220725_RH_Index_016\tIbuprofen_0_STD_\tAnalyte\t50\t3.96\t9893\t\t9892.557\tbb\t9.11617\t-81.8\t864.328\tNO\t0\t4.352\t4:A,9\t23-feb-22\t13:27:43\t"
      
      [[21]]
      [1] "17\t20220725_RH_Index_017\tIbuprofen_0_STD_\tAnalyte\t50\t3.96\t8445\t\t8444.872\tbb\t7.7821\t-84.4\t904.988\tNO\t0\t4.474\t4:A,10\t23-feb-22\t13:33:45\t"
      
      [[22]]
      [1] "18\t20220725_RH_Index_018\tblank\tBlank\t50\t3.96\t7735\t\t7735.303\tbb\t7.12822\t-85.7\t453.673\tNO\t0\t2.922\t4:G,12\t23-feb-22\t13:39:47\t"
      
      [[23]]
      [1] "19\t20220725_RH_Index_019\tblank\tBlank\t50\t3.96\t8134\t\t8133.504\tbb\t7.49517\t-85\t592.746\tNO\t0\t2.93\t4:G,12\t23-feb-22\t13:45:49\t"
      
      [[24]]
      [1] "Compound 2:  Loperamide\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
      
      [[25]]
      [1] "\tName\tSample Text\tType\tStd. Conc\tRT\tArea\tIS Area\tResponse\tPrimary Flags\tnM\t%Dev\tS/N\tConc. Dev. Flagged\tConc. Dev. Allowed\tPk Width\tVial\tAcq.Date\tAcq.Time\tID"
      
      [[26]]
      [1] "1\t20220725_RH_Index_001\tblank\tBlank\t\t\t\t25432.006\t\t\t\t\t\tNO\t0\t\t4:G,10\t23-feb-22\t11:56:26\t"
      
      [[27]]
      [1] "2\t20220725_RH_Index_002\tblank\tBlank\t\t\t\t25071.139\t\t\t\t\t\tNO\t0\t\t4:G,10\t23-feb-22\t12:03:04\t"
      
      [[28]]
      [1] "3\t20220725_RH_Index_003\tblank\tBlank\t\t\t\t25559.383\t\t\t\t\t\tNO\t0\t\t4:G,10\t23-feb-22\t12:09:07\t"
      
      [[29]]
      [1] "4\t20220725_RH_Index_004\tblank\tBlank\t\t\t\t25054.875\t\t\t\t\t\tNO\t0\t\t4:G,10\t23-feb-22\t12:15:09\t"
      
      [[30]]
      [1] "5\t20220725_RH_Index_005\tblank\tBlank\t\t\t\t25825.176\t\t\t\t\t\tNO\t0\t\t4:G,10\t23-feb-22\t12:21:12\t"
      
      [[31]]
      [1] "6\t20220725_RH_Index_006\tLoperamide_0_bead_\tAnalyte\t\t2.34\t288034\t11170.61\t1289.249\tbb\t\t\t21759.044\tNO\t0\t2.468\t4:A,1\t23-feb-22\t12:27:15\t"
      
      [[32]]
      [1] "7\t20220725_RH_Index_007\tLoperamide_0_bead_\tAnalyte\t\t2.33\t269700\t10069.771\t1339.155\tbb\t\t\t16189.105\tNO\t0\t2.51\t4:A,2\t23-feb-22\t12:33:17\t"
      
      [[33]]
      [1] "8\t20220725_RH_Index_008\tLoperamide_0_bead_\tAnalyte\t\t2.34\t337501\t14645.96\t1152.198\tbb\t\t\t24879.31\tNO\t0\t2.475\t4:A,3\t23-feb-22\t12:39:20\t"
      
      [[34]]
      [1] "9\t20220725_RH_Index_009\tLoperamide_0_bead_\tAnalyte\t\t2.33\t314270\t11718.259\t1340.94\tbb\t\t\t17399.287\tNO\t0\t2.529\t4:A,4\t23-feb-22\t12:45:23\t"
      
      [[35]]
      [1] "10\t20220725_RH_Index_010\tLoperamide_0_bead_\tAnalyte\t\t2.34\t296403\t11347.764\t1305.997\tbb\t\t\t19891.39\tNO\t0\t2.621\t4:A,5\t23-feb-22\t12:51:26\t"
      
      [[36]]
      [1] "11\t20220725_RH_Index_011\tblank\tBlank\t\t2.34\t50\t25341.746\t0.098\tbb\t\t\t3.428\tNO\t0\t2.904\t4:G,11\t23-feb-22\t12:57:29\t"
      
      [[37]]
      [1] "12\t20220725_RH_Index_012\tblank\tBlank\t\t\t\t25093.004\t\t\t\t\t\tNO\t0\t\t4:G,11\t23-feb-22\t13:03:32\t"
      
      [[38]]
      [1] "13\t20220725_RH_Index_013\tLoperamide_0_medium_\tAnalyte\t\t2.33\t262816\t10373.271\t1266.796\tbb\t\t\t15521.891\tNO\t0\t2.693\t4:A,6\t23-feb-22\t13:09:34\t"
      
      [[39]]
      [1] "14\t20220725_RH_Index_014\tLoperamide_15_medium_\tAnalyte\t\t2.33\t338490\t11952.766\t1415.948\tbb\t\t\t18723.516\tNO\t0\t2.515\t4:A,7\t23-feb-22\t13:15:37\t"
      
      [[40]]
      [1] "15\t20220725_RH_Index_015\tLoperamide_30_medium_\tAnalyte\t\t2.34\t300627\t10112.404\t1486.427\tbb\t\t\t16945.093\tNO\t0\t2.476\t4:A,8\t23-feb-22\t13:21:40\t"
      
      [[41]]
      [1] "16\t20220725_RH_Index_016\tLoperamide_60_medium_\tAnalyte\t\t2.34\t298127\t9892.557\t1506.825\tbb\t\t\t15491.5\tNO\t0\t2.495\t4:A,9\t23-feb-22\t13:27:43\t"
      
      [[42]]
      [1] "17\t20220725_RH_Index_017\tLoperamide_90_medium_\tAnalyte\t\t2.34\t188537\t8444.872\t1116.283\tbb\t\t\t14998.812\tNO\t0\t2.485\t4:A,10\t23-feb-22\t13:33:45\t"
      
      [[43]]
      [1] "18\t20220725_RH_Index_018\tblank\tBlank\t\t\t\t7735.303\t\t\t\t\t\tNO\t0\t\t4:G,12\t23-feb-22\t13:39:47\t"
      
      [[44]]
      [1] "19\t20220725_RH_Index_019\tblank\tBlank\t\t\t\t8133.504\t\t\t\t\t\tNO\t0\t\t4:G,12\t23-feb-22\t13:45:49\t"
      
      [[45]]
      [1] "20\t20220725_RH_Index_020\tLoperamide_0_cell_2\tAnalyte\t\t2.34\t372432\t10725.858\t1736.141\tbb\t\t\t9338.778\tNO\t0\t2.435\t4:B,1\t23-feb-22\t13:51:52\t"
      
      [[46]]
      [1] "21\t20220725_RH_Index_021\tLoperamide_15_cell_2\tAnalyte\t\t2.34\t341380\t11920.614\t1431.889\tbb\t\t\t19322.296\tNO\t0\t2.479\t4:B,2\t23-feb-22\t13:57:54\t"
      
      [[47]]
      [1] "22\t20220725_RH_Index_022\tLoperamide_30_cell_2\tAnalyte\t\t2.34\t365363\t13219.477\t1381.913\tbb\t\t\t25492.299\tNO\t0\t2.466\t4:B,3\t23-feb-22\t14:03:57\t"
      
      [[48]]
      [1] "23\t20220725_RH_Index_023\tLoperamide_60_cell_2\tAnalyte\t\t2.34\t350150\t12235.257\t1430.904\tbb\t\t\t19779.976\tNO\t0\t2.486\t4:B,4\t23-feb-22\t14:10:00\t"
      
      [[49]]
      [1] "24\t20220725_RH_Index_024\tLoperamide_90_cell_2\tAnalyte\t\t2.33\t318057\t11992.935\t1326.017\tbb\t\t\t11914.696\tNO\t0\t2.571\t4:B,5\t23-feb-22\t14:16:03\t"
      
      [[50]]
      [1] "25\t20220725_RH_Index_025\tblank\tBlank\t\t2.34\t21\t23774.311\t0.044\tbb\t\t\t2.396\tNO\t0\t0.951\t4:G,10\t23-feb-22\t14:22:05\t"
      
      [[51]]
      [1] "Compound 3:  Paracetamol\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
      
      [[52]]
      [1] "\tName\tSample Text\tType\tStd. Conc\tRT\tArea\tIS Area\tResponse\tPrimary Flags\tnM\t%Dev\tS/N\tConc. Dev. Flagged\tConc. Dev. Allowed\tPk Width\tVial\tAcq.Date\tAcq.Time\tID"
      
      [[53]]
      [1] "1\t20220725_RH_Index_001\tblank\tBlank\t\t2.19\t28\t25432.006\t0.056\tbb\t\t\t0.674\tNO\t0\t2.083\t4:G,10\t23-feb-22\t11:56:26\t"
      
      [[54]]
      [1] "2\t20220725_RH_Index_002\tblank\tBlank\t\t\t\t25071.139\t\t\t\t\t\tNO\t0\t\t4:G,10\t23-feb-22\t12:03:04\t"
      
      [[55]]
      [1] "3\t20220725_RH_Index_003\tblank\tBlank\t\t\t\t25559.383\t\t\t\t\t\tNO\t0\t\t4:G,10\t23-feb-22\t12:09:07\t"
      
      [[56]]
      [1] "4\t20220725_RH_Index_004\tblank\tBlank\t\t\t\t25054.875\t\t\t\t\t\tNO\t0\t\t4:G,10\t23-feb-22\t12:15:09\t"
      
      [[57]]
      [1] "5\t20220725_RH_Index_005\tblank\tBlank\t\t\t\t25825.176\t\t\t\t\t\tNO\t0\t\t4:G,10\t23-feb-22\t12:21:12\t"
      
      [[58]]
      [1] "6\t20220725_RH_Index_006\tParacetamol_0_STD_\tAnalyte\t\t2.22\t430\t11170.61\t1.925\tbb\t\t\t49.218\tNO\t0\t2.429\t4:A,1\t23-feb-22\t12:27:15\t"
      
      [[59]]
      [1] "7\t20220725_RH_Index_007\tParacetamol_0_STD_\tAnalyte\t\t2.22\t2402\t10069.771\t11.928\tbb\t\t\t184.334\tNO\t0\t2.498\t4:A,2\t23-feb-22\t12:33:17\t"
      
      [[60]]
      [1] "8\t20220725_RH_Index_008\tParacetamol_0_STD_\tAnalyte\t\t2.22\t9111\t14645.96\t31.103\tbb\t\t\t659.181\tNO\t0\t2.51\t4:A,3\t23-feb-22\t12:39:20\t"
      
      [[61]]
      [1] "9\t20220725_RH_Index_009\tParacetamol_0_STD_\tAnalyte\t\t2.22\t21724\t11718.259\t92.692\tbb\t\t\t791.477\tNO\t0\t2.558\t4:A,4\t23-feb-22\t12:45:23\t"
      
      [[62]]
      [1] "10\t20220725_RH_Index_010\tParacetamol_0_STD_\tAnalyte\t\t\t\t11347.764\t\t\t\t\t\tNO\t0\t\t4:A,5\t23-feb-22\t12:51:26\t"
      
      [[63]]
      [1] "END"
      

# splitDataLines.ML() splits into data frame list

    Code
      dataLines = loadFile.ML(sourceFile = system.file("extdata",
        "Example_MLOutput.txt", package = "aquaras", mustWork = TRUE))
      splitDataLines.ML(dataLines)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
    Output
      $`Compound 1 Ibuprofen\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t`
      # A tibble: 19 x 20
         ...1  Name    Sampl~1 Type  Std. ~2 RT    Area  IS Ar~3 Respo~4 Prima~5 nM   
         <chr> <chr>   <chr>   <chr> <chr>   <chr> <chr> <chr>   <chr>   <chr>   <chr>
       1 1     202207~ blank   Blank 50      3.97  25432 ""      25432.~ bb      23.4~
       2 2     202207~ blank   Blank 50      3.96  25071 ""      25071.~ bb      23.1~
       3 3     202207~ blank   Blank 50      3.95  25559 ""      25559.~ bb      23.5~
       4 4     202207~ blank   Blank 50      3.96  25055 ""      25054.~ bb      23.0~
       5 5     202207~ blank   Blank 50      3.96  25825 ""      25825.~ bb      23.7~
       6 6     202207~ Ibupro~ Anal~ 50      3.96  11171 ""      11170.~ bb      10.2~
       7 7     202207~ Ibupro~ Anal~ 50      3.96  10070 ""      10069.~ bb      9.27~
       8 8     202207~ Ibupro~ Anal~ 50      3.96  14646 ""      14645.~ bb      13.4~
       9 9     202207~ Ibupro~ Anal~ 50      3.96  11718 ""      11718.~ bb      10.7~
      10 10    202207~ Ibupro~ Anal~ 50      3.96  11348 ""      11347.~ bb      10.4~
      11 11    202207~ blank   Blank 50      3.96  25342 ""      25341.~ bb      23.3~
      12 12    202207~ blank   Blank 50      3.96  25093 ""      25093.~ bb      23.1~
      13 13    202207~ Ibupro~ Anal~ 50      3.96  10373 ""      10373.~ bb      9.55~
      14 14    202207~ Ibupro~ Anal~ 50      3.96  11953 ""      11952.~ bb      11.0~
      15 15    202207~ Ibupro~ Anal~ 50      3.96  10112 ""      10112.~ bb      9.31~
      16 16    202207~ Ibupro~ Anal~ 50      3.96  9893  ""      9892.5~ bb      9.11~
      17 17    202207~ Ibupro~ Anal~ 50      3.96  8445  ""      8444.8~ bb      7.78~
      18 18    202207~ blank   Blank 50      3.96  7735  ""      7735.3~ bb      7.12~
      19 19    202207~ blank   Blank 50      3.96  8134  ""      8133.5~ bb      7.49~
      # ... with 9 more variables: `%Dev` <chr>, `S/N` <chr>,
      #   `Conc. Dev. Flagged` <chr>, `Conc. Dev. Allowed` <chr>, `Pk Width` <chr>,
      #   Vial <chr>, Acq.Date <chr>, Acq.Time <chr>, ID <chr>, and abbreviated
      #   variable names 1: `Sample Text`, 2: `Std. Conc`, 3: `IS Area`, 4: Response,
      #   5: `Primary Flags`
      # i Use `colnames()` to see all variable names
      
      $`Compound 2 Loperamide\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t`
      # A tibble: 25 x 20
         ...1  Name    Sampl~1 Type  Std. ~2 RT    Area  IS Ar~3 Respo~4 Prima~5 nM   
         <chr> <chr>   <chr>   <chr> <chr>   <chr> <chr> <chr>   <chr>   <chr>   <chr>
       1 1     202207~ blank   Blank ""      ""    ""    25432.~ ""      ""      ""   
       2 2     202207~ blank   Blank ""      ""    ""    25071.~ ""      ""      ""   
       3 3     202207~ blank   Blank ""      ""    ""    25559.~ ""      ""      ""   
       4 4     202207~ blank   Blank ""      ""    ""    25054.~ ""      ""      ""   
       5 5     202207~ blank   Blank ""      ""    ""    25825.~ ""      ""      ""   
       6 6     202207~ Lopera~ Anal~ ""      "2.3~ "288~ 11170.~ "1289.~ "bb"    ""   
       7 7     202207~ Lopera~ Anal~ ""      "2.3~ "269~ 10069.~ "1339.~ "bb"    ""   
       8 8     202207~ Lopera~ Anal~ ""      "2.3~ "337~ 14645.~ "1152.~ "bb"    ""   
       9 9     202207~ Lopera~ Anal~ ""      "2.3~ "314~ 11718.~ "1340.~ "bb"    ""   
      10 10    202207~ Lopera~ Anal~ ""      "2.3~ "296~ 11347.~ "1305.~ "bb"    ""   
      # ... with 15 more rows, 9 more variables: `%Dev` <chr>, `S/N` <chr>,
      #   `Conc. Dev. Flagged` <chr>, `Conc. Dev. Allowed` <chr>, `Pk Width` <chr>,
      #   Vial <chr>, Acq.Date <chr>, Acq.Time <chr>, ID <chr>, and abbreviated
      #   variable names 1: `Sample Text`, 2: `Std. Conc`, 3: `IS Area`, 4: Response,
      #   5: `Primary Flags`
      # i Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
      
      $`Compound 3 Paracetamol\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t`
      # A tibble: 10 x 20
         ...1  Name    Sampl~1 Type  Std. ~2 RT    Area  IS Ar~3 Respo~4 Prima~5 nM   
         <chr> <chr>   <chr>   <chr> <chr>   <chr> <chr> <chr>   <chr>   <chr>   <chr>
       1 1     202207~ blank   Blank ""      "2.1~ "28"  25432.~ "0.056" "bb"    ""   
       2 2     202207~ blank   Blank ""      ""    ""    25071.~ ""      ""      ""   
       3 3     202207~ blank   Blank ""      ""    ""    25559.~ ""      ""      ""   
       4 4     202207~ blank   Blank ""      ""    ""    25054.~ ""      ""      ""   
       5 5     202207~ blank   Blank ""      ""    ""    25825.~ ""      ""      ""   
       6 6     202207~ Parace~ Anal~ ""      "2.2~ "430" 11170.~ "1.925" "bb"    ""   
       7 7     202207~ Parace~ Anal~ ""      "2.2~ "240~ 10069.~ "11.92~ "bb"    ""   
       8 8     202207~ Parace~ Anal~ ""      "2.2~ "911~ 14645.~ "31.10~ "bb"    ""   
       9 9     202207~ Parace~ Anal~ ""      "2.2~ "217~ 11718.~ "92.69~ "bb"    ""   
      10 10    202207~ Parace~ Anal~ ""      ""    ""    11347.~ ""      ""      ""   
      # ... with 9 more variables: `%Dev` <chr>, `S/N` <chr>,
      #   `Conc. Dev. Flagged` <chr>, `Conc. Dev. Allowed` <chr>, `Pk Width` <chr>,
      #   Vial <chr>, Acq.Date <chr>, Acq.Time <chr>, ID <chr>, and abbreviated
      #   variable names 1: `Sample Text`, 2: `Std. Conc`, 3: `IS Area`, 4: Response,
      #   5: `Primary Flags`
      # i Use `colnames()` to see all variable names
      

# cleanDF.ML() looks as intended

    Code
      dataLines = loadFile.ML(sourceFile = system.file("extdata",
        "Example_MLOutput.txt", package = "aquaras", mustWork = TRUE))
      listDF = splitDataLines.ML(dataLines)
    Message <rlib_message_name_repair>
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
      New names:
      * `` -> `...1`
    Code
      cleanDF.ML(listDF)
    Output
      $`Compound 1 Ibuprofen\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t`
      # A tibble: 10 x 26
         ...1  Date     Signature Index Intern~1 Compo~2 Timep~3 Well_~4 Repli~5 Type 
         <chr> <chr>    <chr>     <chr> <chr>    <chr>   <chr>   <chr>   <chr>   <chr>
       1 6     20220725 RH        Index 006      Ibupro~ 0       cell    "2"     Anal~
       2 7     20220725 RH        Index 007      Ibupro~ 15      cell    "2"     Anal~
       3 8     20220725 RH        Index 008      Ibupro~ 30      cell    "2"     Anal~
       4 9     20220725 RH        Index 009      Ibupro~ 60      cell    "2"     Anal~
       5 10    20220725 RH        Index 010      Ibupro~ 90      cell    "2"     Anal~
       6 13    20220725 RH        Index 013      Ibupro~ 0       STD     ""      Anal~
       7 14    20220725 RH        Index 014      Ibupro~ 0       STD     ""      Anal~
       8 15    20220725 RH        Index 015      Ibupro~ 0       STD     ""      Anal~
       9 16    20220725 RH        Index 016      Ibupro~ 0       STD     ""      Anal~
      10 17    20220725 RH        Index 017      Ibupro~ 0       STD     ""      Anal~
      # ... with 16 more variables: `Std. Conc` <chr>, RT <chr>, Area <chr>,
      #   `IS Area` <chr>, Response <chr>, `Primary Flags` <chr>, nM <chr>,
      #   `%Dev` <chr>, `S/N` <chr>, `Conc. Dev. Flagged` <chr>,
      #   `Conc. Dev. Allowed` <chr>, `Pk Width` <chr>, Vial <chr>, Acq.Date <chr>,
      #   Acq.Time <chr>, ID <chr>, and abbreviated variable names 1: `Internal row`,
      #   2: Compound, 3: Timepoint, 4: Well_Type, 5: Replicate
      # i Use `colnames()` to see all variable names
      
      $`Compound 2 Loperamide\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t`
      # A tibble: 15 x 26
         ...1  Date     Signature Index Intern~1 Compo~2 Timep~3 Well_~4 Repli~5 Type 
         <chr> <chr>    <chr>     <chr> <chr>    <chr>   <chr>   <chr>   <chr>   <chr>
       1 6     20220725 RH        Index 006      Lopera~ 0       bead    ""      Anal~
       2 7     20220725 RH        Index 007      Lopera~ 0       bead    ""      Anal~
       3 8     20220725 RH        Index 008      Lopera~ 0       bead    ""      Anal~
       4 9     20220725 RH        Index 009      Lopera~ 0       bead    ""      Anal~
       5 10    20220725 RH        Index 010      Lopera~ 0       bead    ""      Anal~
       6 13    20220725 RH        Index 013      Lopera~ 0       medium  ""      Anal~
       7 14    20220725 RH        Index 014      Lopera~ 15      medium  ""      Anal~
       8 15    20220725 RH        Index 015      Lopera~ 30      medium  ""      Anal~
       9 16    20220725 RH        Index 016      Lopera~ 60      medium  ""      Anal~
      10 17    20220725 RH        Index 017      Lopera~ 90      medium  ""      Anal~
      11 20    20220725 RH        Index 020      Lopera~ 0       cell    "2"     Anal~
      12 21    20220725 RH        Index 021      Lopera~ 15      cell    "2"     Anal~
      13 22    20220725 RH        Index 022      Lopera~ 30      cell    "2"     Anal~
      14 23    20220725 RH        Index 023      Lopera~ 60      cell    "2"     Anal~
      15 24    20220725 RH        Index 024      Lopera~ 90      cell    "2"     Anal~
      # ... with 16 more variables: `Std. Conc` <chr>, RT <chr>, Area <chr>,
      #   `IS Area` <chr>, Response <chr>, `Primary Flags` <chr>, nM <chr>,
      #   `%Dev` <chr>, `S/N` <chr>, `Conc. Dev. Flagged` <chr>,
      #   `Conc. Dev. Allowed` <chr>, `Pk Width` <chr>, Vial <chr>, Acq.Date <chr>,
      #   Acq.Time <chr>, ID <chr>, and abbreviated variable names 1: `Internal row`,
      #   2: Compound, 3: Timepoint, 4: Well_Type, 5: Replicate
      # i Use `colnames()` to see all variable names
      
      $`Compound 3 Paracetamol\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t`
      # A tibble: 5 x 26
        ...1  Date     Signature Index Interna~1 Compo~2 Timep~3 Well_~4 Repli~5 Type 
        <chr> <chr>    <chr>     <chr> <chr>     <chr>   <chr>   <chr>   <chr>   <chr>
      1 6     20220725 RH        Index 006       Parace~ 0       STD     ""      Anal~
      2 7     20220725 RH        Index 007       Parace~ 0       STD     ""      Anal~
      3 8     20220725 RH        Index 008       Parace~ 0       STD     ""      Anal~
      4 9     20220725 RH        Index 009       Parace~ 0       STD     ""      Anal~
      5 10    20220725 RH        Index 010       Parace~ 0       STD     ""      Anal~
      # ... with 16 more variables: `Std. Conc` <chr>, RT <chr>, Area <chr>,
      #   `IS Area` <chr>, Response <chr>, `Primary Flags` <chr>, nM <chr>,
      #   `%Dev` <chr>, `S/N` <chr>, `Conc. Dev. Flagged` <chr>,
      #   `Conc. Dev. Allowed` <chr>, `Pk Width` <chr>, Vial <chr>, Acq.Date <chr>,
      #   Acq.Time <chr>, ID <chr>, and abbreviated variable names 1: `Internal row`,
      #   2: Compound, 3: Timepoint, 4: Well_Type, 5: Replicate
      # i Use `colnames()` to see all variable names
      

