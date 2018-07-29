# the tests are realized using the "testthat" package
# and can be found in ./testthat

require(testthat)
# tm is only imported
require(tm)
test_check("tm.plugin.koRpus")
