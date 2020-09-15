#' @title Simple wrapper for graphics.off()
#' @importFrom grDevices graphics.off
#' @export

gof <- function(){
    graphics.off()
    ## ## If rgl is loaded
    ## if("package:rgl" %in% search()){
    ##     rgl::rgl.quit()
    ## }
}
