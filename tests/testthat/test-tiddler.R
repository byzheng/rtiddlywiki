test_that("tiddler", {
    skip_if(!is_test_tw())
    expect_null(get_tiddler("test-missing"))
    expect_no_error(delete_tiddler("test1"))
    expect_no_error(put_tiddler("test1", "This is a test tiddler",
                                type = "text/x-tiddlywiki",
                                tags = c("TAG 1", "TAG2"),
                                fields = list("f1" = "f1", "f2" = "f 2")))
    new_tiddler <- get_tiddler("test1")
    expect_equal(new_tiddler$title, "test1")
    expect_equal(new_tiddler$text, "This is a test tiddler")
    expect_equal(new_tiddler$type, "text/x-tiddlywiki")
    expect_equal(new_tiddler$tags, c("TAG 1", "TAG2"))
    expect_equal(new_tiddler$fields$f1, "f1")
    expect_equal(new_tiddler$fields$f2, "f 2")

    # title = "test1"
    # text = "This is a test tiddler"
    # type = "text/x-tiddlywiki"
    # tags = c("TAG 1", "TAG2")
    # fields = c("f2" = "f 2 again")
    expect_no_error(put_tiddler(title = "test1",
                                tags = c("TAG 3", "TAG2"),
                                fields = list("f2" = "f 2 again", "f3" = "f 3")))
    new_tiddler <- get_tiddler("test1")
    expect_equal(new_tiddler$title, "test1")
    expect_equal(new_tiddler$text, "This is a test tiddler")
    expect_equal(new_tiddler$type, "text/vnd.tiddlywiki")
    expect_equal(new_tiddler$tags, c("TAG 3", "TAG2", "TAG 1"))
    expect_equal(new_tiddler$fields$f1, "f1")
    expect_equal(new_tiddler$fields$f2, "f 2 again")
    expect_equal(new_tiddler$fields$f3, "f 3")
    tiddlers <- get_tiddlers("[all[tiddlers]!is[system]sort[title]]")
    expect_true(length(tiddlers) > 0)

    # Remove an existing field
    remove_fields("test1", "f1")
    expect_error(remove_fields("test1", "f1"))
    expect_error(remove_fields("test13333", "f33"))
    tiddler <- get_tiddler("test1")
    expect_equal(tiddler$fields$f1, NULL)
    expect_equal(tiddler$fields$f2, "f 2 again")
    expect_equal(tiddler$fields$f3, "f 3")
    # title = "test1"
    # text = "This is a test tiddler"
    # type = "text/x-tiddlywiki"
    # tags = c("TAG 1", "TAG2")
    # fields = "f2"

})


