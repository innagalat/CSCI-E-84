source(file.path("src", "lib", "utils.R"))

get.excel.to.rda.if.required <- function(url.addr, data.path, table.name, file.format) {
    
    tmp <- paste0(tempfile(), ".", file.form)
    tryCatch({
    }, error = function(cond) {
    })
    
    if (missing(table.name)) {
    } else {
    }
    unlink(tmp)
  }
}

data.file.path <- file.path("data", "election_2012.Rda")
data.url <- "http://www.fec.gov/pubrec/fe2012/federalelections2012.xls"
get.excel.to.rda.if.required(data.url, data.file.path)
rm(list = ls())
