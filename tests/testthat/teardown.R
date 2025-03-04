


cat("Stopping test widdlywiki server...\n")

pid <- Sys.getenv("TEST_TIDDLYWIKI_SERVER_PID")
if (nzchar(pid)) {
    tools::pskill(as.numeric(pid))
}

cat("Cleanup tempory test folder")
tw_folder <- test_folder()
unlink(tw_folder, recursive = TRUE)

