library(viridis)
library(yaml)
library(httr)
library(jsonlite)
library(runjags)

source("functions.R")

run_model(execute = FALSE)

results_text()
table1()
fig1()
fig2()
fig5()


# Session Info
# 
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19041)

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.1252 
# [2] LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] runjags_2.0.4-6   jsonlite_1.7.2    httr_1.4.2        yaml_2.2.1       
# [5] viridis_0.5.1     viridisLite_0.3.0

# loaded via a namespace (and not attached):
#  [1] magrittr_2.0.1   tidyselect_1.1.0 munsell_0.5.0    lattice_0.20-41 
#  [5] colorspace_2.0-0 R6_2.5.0         rlang_0.4.10     dplyr_1.0.2     
#  [9] parallel_4.0.3   grid_4.0.3       gtable_0.3.0     coda_0.19-3     
# [13] ellipsis_0.3.1   tibble_3.0.4     lifecycle_0.2.0  crayon_1.3.4    
# [17] gridExtra_2.3    purrr_0.3.3      ggplot2_3.3.3    vctrs_0.3.6     
# [21] glue_1.4.2       compiler_4.0.3   pillar_1.4.7     generics_0.1.0  
# [25] scales_1.1.1     pkgconfig_2.0.3 