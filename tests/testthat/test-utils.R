test_that("utils", {
    expect_equal(split_field(""), NULL)
    expect_error(split_field(NULL))
    expect_equal(split_field(" [[tag 1]] tag2"), c("tag 1", "tag2"))
    expect_equal(split_field(" [[tag 1]] tag2 "), c("tag 1", "tag2"))
    expect_equal(split_field("[[tag 1]] tag2"), c("tag 1", "tag2"))
    expect_equal(split_field("tag0 [[tag 1]] tag2"), c("tag0", "tag 1", "tag2"))
    expect_equal(split_field("[[tag 1 & 2]] tag2 [tag] [[[[tag 4]]]]"),
                 c("tag 1 & 2", "tag2", "[tag]", "[[tag 4]]"))
    expect_equal(split_field("[[[[tag 4]]]] [[tag 1 & 2]] tag2 [tag]"),
                 c("[[tag 4]]", "tag 1 & 2", "tag2", "[tag]"))

    expect_equal(split_field(" [[tag 1 & 2]] tag2"), c("tag 1 & 2", "tag2"))

})


