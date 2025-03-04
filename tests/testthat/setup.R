
cat("Starting test tiddlywiki server...\n")


user_home <- Sys.getenv("USERPROFILE")   # Works on Windows
npm_path <- file.path(user_home, "AppData", "Roaming", "npm")
tiddlywiki_path <- file.path(npm_path, 'tiddlywiki')
if (.Platform$OS.type == "windows") {
    tiddlywiki_path <- paste0(tiddlywiki_path, ".cmd")
}
tw_folder <- test_folder()
system(paste0(tiddlywiki_path, " ", tw_folder, " --init server"))

tiddlers <- list.files(testthat::test_path("tiddlers"), full.names = TRUE)

file.copy(tiddlers, file.path(tw_folder, "tiddlers"))
Sys.sleep(2)
server_process <- processx::process$new(
    tiddlywiki_path,
      c(tw_folder, "--listen", "port=9090"),
    # stdout = "wiki-stdout.txt",
    # stderr = "wiki-stderr.txt",
    cleanup_tree = TRUE
)
Sys.sleep(4)

Sys.setenv(TEST_TIDDLYWIKI_SERVER_PID = server_process$get_pid())
