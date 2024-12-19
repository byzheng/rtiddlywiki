test_that("Get title", {
    expect_null(.get_title("test"))
    expect_equal(.get_title("title: test"), "test")
    expect_equal(.get_title("title: \"test\""), "test")
    expect_equal(.get_title("title: \"this is a test\""), "this is a test")
    expect_equal(.get_title("title: \"this is a \"test\"\""),
                 "this is a \"test\"")
})


test_that("pretty link", {
    text <- c("See this \\[\\[This is a test\\]\\], link again",
              "this is a new line",
              "this line has \\[\\[link one\\]\\] and \\[\\[label|link two\\]\\]")
    text2 <- .pretty_link(text)
    expect_equal(text2[1], "See this [[This is a test]], link again")
    expect_equal(text2[2], "this is a new line")
    expect_equal(text2[3], "this line has [[link one]] and [[label|link two]]")
})


test_that("markdown link", {
    text <- c("See this [This is a link](), link again",
              "this is a new line",
              "this line has [link one](#link 1) and [link two](#link2)",
              "[a mornal link](https://example.org)")
    text2 <- .markdown_link(text)
    expect_equal(text2[1], "See this [This is a link](), link again")
    expect_equal(text2[2], "this is a new line")
    expect_equal(text2[3], "this line has [link one](#link%201) and [link two](#link2)")
    expect_equal(text2[4], "[a mornal link](https://example.org)")
})


test_that("rmarkdown", {


    skip_on_cran()
    skip_if_not_pandoc <- function() {
        if (!rmarkdown::pandoc_available()) {
            skip("Pandoc is not available")
        }
    }

    render_rmd <- function(..., .env = parent.frame()) {
        temp_input_file <- tempfile(fileext = ".Rmd")
        text <- c(...)
        writeLines(text, temp_input_file)
        temp_output_file <- paste0(tools::file_path_sans_ext(temp_input_file), ".json")
        rmarkdown::render(temp_input_file,
                          quiet = TRUE)
        r <- jsonlite::read_json(temp_output_file)
        file.remove(temp_output_file, temp_input_file)
        r
    }

    skip_if_not_pandoc()
    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "---", "",
                        "# Section 1",
                        "This is a test"))
    expect_equal(rmd$tags, "[[tag1]] [[tag 2]]")
    expect_equal(rmd$title, "test")

    rmd <- render_rmd(c("---", "title: test",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "---", "",
                        "# Section 1",
                        "This is a test"))
    expect_equal(rmd$tags, "[[tag1]] [[tag 2]]")
    expect_equal(rmd$title, "test")

    rmd <- render_rmd(c("---", "title: test",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "    use_bookdown: true",
                        "    number_sections: false",
                        "---", "",
                        "# Section 1",
                        "This is a test"))
    expect_equal(rmd$tags, "[[tag1]] [[tag 2]]")
    expect_equal(rmd$title, "test")
    expect_equal(rmd$type, "text/x-markdown")


    # Test renderwikitext
    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "---", "",
                        "[[This is a test]]"))
    expect_equal(rmd$text, "[[This is a test]]")


    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "---", "",
                        "{{This is a test}}"))
    expect_equal(rmd$text, "{{This is a test}}")

    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "---", "",
                        "test [md link](#tiddler 1) and [link](#tiddler2)"))
    expect_equal(rmd$text, "test [md link](#tiddler%201) and [link](#tiddler2)")

    # Test fields
    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "    fields:",
                        "      \"field1\": \"V1\"",
                        "      \"field 2\": \"Value 2\"",
                        "---", "",
                        "{{This is a test}}"))
    expect_equal(rmd$fields$field1, "V1")
    expect_equal(rmd$fields$`field 2`, "Value 2")

    # Test graph

    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "    fields:",
                        "      \"field1\": \"V1\"",
                        "      \"field 2\": \"Value 2\"",
                        "---", "",
                        "```{r static-images-1}",
                        "",
                        "library(ggplot2)",
                        "cars |> ",
                        "    ggplot() +",
                        "    geom_point(aes(speed, dist))",
                        "```"))
    expect_equal(grepl("data:image", rmd$text), TRUE)


    # Test macro

    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "    fields:",
                        "      \"field1\": \"V1\"",
                        "      \"field 2\": \"Value 2\"",
                        "---", "",
                        "",
                        "<<macro>>",
                        '<<macro p1:"P">>',
                        "<<macro ",
                        ">>"
                        ))
    expect_equal(grepl("<<macro>>", rmd$text), TRUE)


    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "    fields:",
                        "      \"field1\": \"V1\"",
                        "      \"field 2\": \"Value 2\"",
                        "---", "",
                        "",
                        '<<alert primary """',
                        'This is a test',
                        '""">>'
    ))
    expect_equal(grepl('"""', rmd$text), TRUE)
    expect_equal(grepl("<<alert", rmd$text), TRUE)

    # Test variant: gfm
    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "    variant: gfm",
                        "    fields:",
                        "      \"field1\": \"V1\"",
                        "      \"field 2\": \"Value 2\"",
                        "---", "",
                        "",
                        '<<alert primary """',
                        'This is a test',
                        '""">>'
    ))
    expect_equal(grepl('"""', rmd$text), TRUE)
    expect_equal(grepl("<<alert", rmd$text), TRUE)

    # Test widget
    rmd <- render_rmd(c("---", "title: \"test\"",
                        "output: ",
                        "  tiddler_document:",
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "    variant: gfm",
                        "    fields:",
                        "      \"field1\": \"V1\"",
                        "      \"field 2\": \"Value 2\"",
                        "---", "",
                        "",
                        '<$widget p="C" />'
    ))
    expect_equal(grepl('<\\$widget', rmd$text), TRUE)




    # Test Tddlywiki
    skip_if(!is_test_tw())
    rmd <- render_rmd(c("---", "title: \"rmarkdown test\"",
                        "output: ",
                        "  tiddler_document:",
                        sprintf('    host: "%s"', tw_options("host")),
                        "    tags: [\"tag1\", \"tag 2\"]",
                        "    fields:",
                        "      \"field1\": \"V1\"",
                        "      \"field 2\": \"Value 2\"",
                        "---", "",
                        "```{r static-images-1}",
                        "",
                        "library(ggplot2)",
                        "cars |> ",
                        "    ggplot() +",
                        "    geom_point(aes(speed, dist))",
                        "```"))
    tiddler <- get_tiddler("rmarkdown test")
    expect_equal(tiddler$fields$field1, "V1")
    expect_equal(tiddler$fields$`field 2`, "Value 2")
    expect_equal(tiddler$tags, c("tag1", "tag 2"))
    expect_equal(tiddler$title, "rmarkdown test")
    expect_equal(tiddler$type, "text/x-markdown")
    expect_no_error(delete_tiddler("rmarkdown test"))
})

