test_that("rmarkdown", {
    skip_on_cran()
    skip_if_not_pandoc <- function() {
        if (!rmarkdown::pandoc_available()) {
            skip("Pandoc is not available")
        }
    }

    render_rmd <- function(..., .env = parent.frame()) {
        temp_input_file <- tempfile(fileext = ".Rmd")
        writeLines(c(...), temp_input_file)
        temp_output_file <- paste0(tools::file_path_sans_ext(temp_input_file), ".json")
        rmarkdown::render(temp_input_file,
                          quiet = TRUE)
        r <- jsonlite::read_json(temp_output_file)
        file.remove(temp_output_file, temp_input_file)
        r
    }

    skip_if_not_pandoc()
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
  })
