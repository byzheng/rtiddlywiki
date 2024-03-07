test_that("utils", {
    expect_equal(split_field("[[tag 1]] tag2"), c("tag 1", "tag2"))
    v <- "[[tag 1 & 2]] tag2 [tag] [[[[tag 4]]]]"
    expect_equal(split_field("[[tag 1 & 2]] tag2 [tag] [[[[tag 4]]]]"), c("tag 1 & 2", "tag2"))
})


