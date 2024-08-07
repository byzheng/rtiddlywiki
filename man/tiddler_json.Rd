% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tiddler_json}
\alias{tiddler_json}
\title{Generate tiddler in json format}
\usage{
tiddler_json(
  title,
  text,
  type = c("text/vnd.tiddlywiki", "text/x-tiddlywiki", "text/x-markdown", "text/html",
    "text/plain", "application/json"),
  tags = NULL,
  fields = NULL,
  format = c("json", "list")
)
}
\arguments{
\item{title}{tiddler title}

\item{text}{tiddler text}

\item{type}{tiddler type}

\item{tags}{a vector for tiddler tags}

\item{fields}{a named vector for tiddler fields.}

\item{format}{export format as json or list}
}
\value{
New tiddler in json format
}
\description{
Generate tiddler in json format
}
