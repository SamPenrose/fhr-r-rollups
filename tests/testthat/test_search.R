source("../../lib/search.R")
library(rjson)

test_that("allSearches() works", {
  expect_equal(allSearches({}), NULL)
    days = fromJSON('{"2015-01-01":null}')
    expect_equal(allSearches(days), NULL)
    days = fromJSON('{"2015-01-01":{"org.mozilla.searches.counts":null}}')
    expect_equal(allSearches(days), NULL)

    ## Simple valid record.
    days = fromJSON(file="data/valid_activity.json")
    ## expect_equal(allSearches(days), list("2015-01-02"=list(totalsec = 100, activesec = 50)))
})