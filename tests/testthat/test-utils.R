test_that("split_field", {
    expect_equal(split_field(""), NULL)
    expect_equal(split_field(NULL), NULL)
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


test_that("save_base64", {
    library(ggplot2)
    p <- cars |>
        ggplot() +
        geom_point(aes(speed, dist))
    p_base64 <- p |> save_base64()
    expect_length(p_base64, 1)
    expect_true(grepl("data:image/png;base64", p_base64))
})




test_that("tw_table", {
    t_table <- cars |>
        dplyr::slice(1:10) |>
        tw_table()
    expect_length(t_table, 1)
    expect_true(grepl("speed", t_table))
})


