

request <- function(method = "GET",
                    path = '/',
                    query = NULL,
                    body = NULL,
                    auto_unbox = TRUE) {
    # Check major arguments
    stopifnot(length(method) == 1)
    stopifnot(is.character(method))
    stopifnot(method %in% c("GET", "PUT", "POST", "DELETE"))

    host <- TW_OPTIONS("host")
    stopifnot(length(host) == 1)

    # Create request
    req <- httr2::request(host) |>
        httr2::req_options(ssl_verifypeer = 0) |> # self sigined ssl
        httr2::req_url_path_append(utils::URLencode(path)) |>
        httr2::req_method(method)
    if (!is.null(query) && is.list(query)) {
        query$.req <- req
        req <- do.call(httr2::req_url_query, query)
    }


    # add x-auth-key header for specific permission. not general usage
    http_x_auth_key <- TW_OPTIONS("http_x_auth_key")

    if (nchar(http_x_auth_key) > 0) {
        req <- req |>
            httr2::req_headers(`X-Auth-Key` = http_x_auth_key)
    }

    # add header for x-request-with for put request
    if (method %in% c("PUT", "DELETE")) {
        req <- req |>
            httr2::req_headers(`x-requested-with` = "TiddlyWiki")
    }

    # Add body
    if (!is.null(body)) {
        req <- req |>
            httr2::req_body_json(data = body, auto_unbox = auto_unbox)
    }

    # Perform the actual request
    resp <- req |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()

    # Return null for empty body/response
    if (length(resp$body) == 0) {
        return(NULL)
    }

    status_code <- httr2::resp_status(resp)
    if (status_code == 403) {
        stop("403 Forbidden")
    }
    c_type <- resp$headers$`Content-Type`



    if (!is.null(c_type) && grepl("text/plain", c_type)) {
        content <- resp |>
            httr2::resp_body_string()
    } else {
        content <- resp |>
            httr2::resp_body_json()
    }

    content



}


