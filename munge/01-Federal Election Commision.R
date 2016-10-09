source(file.path("src", "lib", "utils.R"))

get.excel.to.rda.if.required <- function(url.addr, data.path, table.name, file.format) {
    # Fetch excel spreadsheet and save all sheets listed in the first sheet of a given file
    dest.path <- normalizePath(data.path, mustWork = F)
    if (!file.exists(dest.path, showWarnings = F)[1]) {
        file.form <- tail(strsplit(basename(url.addr), "[.]")[[1]], n = 1)
        if (nchar(file.form) > 4) {
            if(missing(file.format)) {
                file.form <- "xls"
            } else {
                file.form <- file.format
            }
        }
    
    tmp <- paste0(tempfile(), ".", file.form)
    tryCatch({
        user.os <- Sys.info()['sysname'][[1]]
        if (user.os == "Windows") {
            download.file(url.addr, destfile = tmp, mode = "wb")
        } else if (user.os == "Linux") {
            download.file(url.addr, destfile = tmp, mode = "wb", method = "wget")
        } else if (user.os == "Darwin") {
            download.file(url.addr, destfile = tmp, mode = "wb", method = "curl")
        }
    }, error = function(cond) {
        warning("Couldn't download the file, please check the link")
        warning(cond)
    })
    
    if (missing(table.name)) {
        # Reading all the sheets specified in the first sheet, only makes sense for this exersise
        info <- as.data.frame(read_excel(tmp, 1))[c(2:19),]
        for (i in 1:nrow(info)) {
            data <- as.data.frame(read_excel(tmp, i))
            save.to.globalenv(info[i, 2], data)
        }
        save(tab, file = dest.path)
    } else {
        data <- read_excel(tmp, table.name)
        save(data, file = dest.path)
    }
    unlink(tmp)
  }
}

data.file.path <- file.path("data", "election_2012.Rda")
data.url <- "http://www.fec.gov/pubrec/fe2012/federalelections2012.xls"
get.excel.to.rda.if.required(data.url, data.file.path)
rm(list = ls())