# rtiddlywiki
![R-CMD-check](https://github.com/byzheng/rtiddlywiki/workflows/R-CMD-check/badge.svg)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rtiddlywiki)](https://cran.r-project.org/package=rtiddlywiki)

[![](http://cranlogs.r-pkg.org/badges/grand-total/rtiddlywiki?color=green)](https://cran.r-project.org/package=rtiddlywiki)
[![](http://cranlogs.r-pkg.org/badges/last-month/rtiddlywiki?color=green)](https://cran.r-project.org/package=rtiddlywiki)
[![](http://cranlogs.r-pkg.org/badges/last-week/rtiddlywiki?color=green)](https://cran.r-project.org/package=rtiddlywiki)


R interface for [tiddlywiki](https://tiddlywiki.com/) which is a unique non-linear notebook for capturing, organising and sharing complex information.

## Communicate with WebServer API for node.js only

[WebServer APIs](https://tiddlywiki.com/static/WebServer%2520API.html) of tiddlywiki are implemented in R interface including `GET` list of tiddlers by filters, a tiddler, and file; `PUT` a new tiddler.

```r
tw_options(host = "http://127.0.0.1:8080/")
# Get all tiddlers
get_tiddlers()
# Get a tiddler by title 
get_tiddler(title = "GettingStarted")

# Put a new tiddler 
title <- "New tiddler"
text <- c("!! Section",
          "This is a new tiddler")
type <- "text/vnd.tiddlywiki"
tags <- c("Tag1", "Tag 2")
put_tiddler(title = title, 
            text = text,
            type = type, 
            tags = tags)
```

## Generate markdown tiddler from Rmarkdown file

A new output format `tiddler_document` is exported to generate markdown tiddler from [Rmarkdown file](https://rmarkdown.rstudio.com/) and then `PUT` into tiddlywiki WebServer. The image files generated by `knitr` are copied into local folder of tiddlywiki as no `PUT` `file` API in tiddlywiki 5.2.2. This is a temporary solution. 

Two parameter are used to specify the `host` of tiddlywiki server and `path` in the local file system. 

A new tiddler in json format is generated in the Rmd folder and can be used to import into tiddlywiki in the server or single file. The tiddler `PUT` into server if `host` is specified. All images copy into local file system of tiddlywiki if `path` is specified.

The `yaml` header can be specified to export `tiddler_document`.

```yaml
title: "R Markdown file"
output: 
  rtiddlywiki::tiddler_document:
    path: "full-path-to-tiddlywiki-project"
    host: "http://127.0.0.1:8080/"
    tags: ["tag 1", "tag 2"]
```

The `title` in the Rmarkdown file is used as tiddler name.


## Installation

Install the developing version from [Github](https://github.com/byzheng/rtiddlywiki).

```r
devtools::install_github('byzheng/rtiddlywiki')
```
