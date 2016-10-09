save.to.globalenv <- function(name, data) {
    # Saving objects to the global environment, by attaching them to a list
    if(is.null(get0("tab", e = globalenv()))) {
        tab <<- list()
    }
    tab[[name]] <<- data
}