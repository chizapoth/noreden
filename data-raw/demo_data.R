# demo data loaded in the current package
# might remove at a later point 


data_newdiet <- readRDS('./inst/rawdata/demo_12foods_res1.rda')
data_contrib <- readRDS('./inst/rawdata/demo_12foods_res2.rda')

save(data_newdiet, file = "data/data_newdiet.rda", compress = 'xz')
save(data_contrib, file = "data/data_contrib.rda", compress = 'xz')

# usethis::use_data(data_newdiet, overwrite = T, compress = 'xz')
