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

    ## Simple valid record.
    days = fromJSON(file="data/valid_activity.json")
    result = list("2015-01-01"=list(totalsec = c(100, 200, 300, 400, 60),
                                    activesec = c(50, 100, 150, 200, 30)),
		  "2015-01-02"=list(totalsec = 100, activesec = 50))
    expect_equal(allActivity(days), result)

    ## Now test all the error detection statements by adding triggering
    ## values one by one and expecting them to be discarded.
    days = list("2015-01-02"=days$"2015-01-02")
    result = list("2015-01-02"=list(totalsec = 100, activesec = 50))
    expect_equal(allActivity(days), result)

    days$"2015-01-03" <- list(purpose="Empty day does not change results")
    expect_equal(allActivity(days), result)

    days$"2015-01-04" <- list(
        purpose = "Days with differing time/tick lengths are discarded",
        org.mozilla.appSessions.previous = list(
            cleanTotalTime=c(10, 20),
            cleanActiveTicks=c(1)
        )
    )
    expect_equal(allActivity(days), result)
    expect_equal(length(allActivity(days)), 1)

    two_ticks = c(0, 0)
    # Fix the length mismatch, also testing 0 is a valid ticks value ...
    days$"2015-01-04"$org.mozilla.appSessions.previous$cleanActiveTicks <- two_ticks
    expect_equal(allActivity(days)$"2015-01-04"$totalsec, c(10, 20))

    # ... now break the tick values in a couple ways, which tests both value
    # detection and that we strip out the corresponding time value.
    has_na = c(1, '')
    days$"2015-01-04"$org.mozilla.appSessions.previous$cleanActiveTicks = has_na
    expect_equal(allActivity(days)$"2015-01-04"$totalsec, 10)
    expect_equal(allActivity(days)$"2015-01-04"$activesec, 5)
    too_big = c(3110401, 2)
    days$"2015-01-04"$org.mozilla.appSessions.previous$cleanActiveTicks = too_big
    expect_equal(allActivity(days)$"2015-01-04"$totalsec, 20)
    expect_equal(allActivity(days)$"2015-01-04"$activesec, 10)
    too_small = c(1, -2)
    days$"2015-01-04"$org.mozilla.appSessions.previous$cleanActiveTicks = too_small
    expect_equal(allActivity(days)$"2015-01-04"$totalsec, 10)
    expect_equal(allActivity(days)$"2015-01-04"$activesec, 5)

    # time values must be > 0 and < 15552000
    days$"2015-01-04"$org.mozilla.appSessions.previous$cleanActiveTicks <- two_ticks
    zero_time = c(0, 20)
    days$"2015-01-04"$org.mozilla.appSessions.previous$cleanTotalTime = zero_time
    expect_equal(allActivity(days)$"2015-01-04"$totalsec, 20)
    big_time = c(10, 15552000+1)
    days$"2015-01-04"$org.mozilla.appSessions.previous$cleanTotalTime = big_time
    expect_equal(allActivity(days)$"2015-01-04"$totalsec, 10)
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
