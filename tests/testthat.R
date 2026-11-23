# Tests of the gta package: Rscript -e 'testthat::test_local()' from the
# repository (the control datasets use tests/sample_data and config/entities).
library(testthat)
library(gta)
test_check("gta")
