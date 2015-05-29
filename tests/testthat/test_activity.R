source("../../lib/activity.R")
library(rjson)

test_that("allActivity works", {
    ## Simple garbage values.
    days = fromJSON("{}")
    expect_equal(allActivity(days), NULL)
    days = fromJSON('{"2015-01-01":null}')
    expect_equal(allActivity(days), NULL)
    days = fromJSON('{"2015-01-01":{"org.mozilla.appSessions.previous":null}}')
    expect_equal(allActivity(days), NULL)

    ## Simple correct values.
    days = fromJSON(file="data/valid_activity.json")
    result = list("2015-01-01"=list(totalsec = c(100, 200, 300, 400, 60),
                                    activesec = c(50, 100, 150, 200, 30)),
		  "2015-01-02"=list(totalsec = 100, activesec = 50))
    expect_equal(allActivity(days), result)

})

## Add Date objects
test_that("last.month.day works", {
    dates = c("2014-12-31", "2015-01-01", "2016-02-26")
    expect_equal(last.month.day(dates),
                 c("2014-12-31", "2015-01-31", "2016-02-29"))
})

## Add Date objects
test_that("is.weekday works", {
    days <- c("2015-05-26", "2015-05-24")
    expect_equal(is.weekday(days), c(TRUE, FALSE))
    expect_equal(is.weekday(c("")), c(FALSE))
})
