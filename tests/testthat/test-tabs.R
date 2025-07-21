
test_that("tabs", {

    tab_labels <- c("Tab1", "Tab2", "Tab3")

    tab_content_fun <- function(i, extra_text = "") {
        htmltools::tagList(
            htmltools::tags$p(paste("Content for tab:", tab_labels[i], extra_text)),
            htmltools::tags$img(src = paste0("plot_", i, ".png"), width = "100%")
      )
    }

    html <- tabs(tab_labels, tab_content_fun, checked = 2, extra_text = "Additional details")

    expect_equal(length(html), 3)
})

