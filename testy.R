library(relationshipMatrix)
df<-tibble(indepvar1=1:10, indepvar2=1:10, indepvar3=1:10, ivargrp1_1=1:10, ivargrp1_2=1:10, ivargrp1_3=1:10,
           depvar1=1:10, depvar2=1:10, depvar3=1:10, depvar4=1:10, depvar5=1:10, depvar6=1:10,
           dvargrp1_1=1:10, dvargrp1_2=1:10, dvargrp1_3=1:10,
           dvargrp2_1=1:10, dvargrp2_2=1:10, dvargrp2_3=1:10)
df_structure<-danesurowe::create_df_from_df_structure(df, flag_include_vartype = TRUE)
tododf<-read_matrix('tests/testthat/test-00_matrix.xlsx', df_structure)

debugonce(danesurowe::create_df_from_df_structure)

danesurowe::class2vartype(df$indepvar1)
purrr::map_chr(as.list(df), ~danesurowe::class2vartype(.))
