# food information: current diet and per unit contribution

all_diet <- readRDS('./inst/rawdata/d_diet.rda')
contrib_per_unit <- readRDS('./inst/rawdata/d_perunit_contrib.rda')

save(all_diet, file = "data/all_diet.rda", compress = 'xz')
save(contrib_per_unit, file = "data/contrib_per_unit.rda", compress = 'xz')
