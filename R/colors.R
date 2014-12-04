
#' An Inspector Derrick palette generator
#'
#' This code is inspired by the magnificent \href{https://github.com/karthik/wesanderson} {Wes Anderson package}. 
#' @param n Number of colors desired. Unfortunately most palettes now only have 4 or 5 colors. But hopefully we'll add more palettes soon. All color schemes are derived from the most excellent Tumblr blog: \href{http://wesandersonpalettes.tumblr.com/}{Wes Anderson Palettes}
#' @param  name Name of desired palette. Choices are: \code{Office}, \code{StefansCostumeYoung}, \code{StefansCostumeOld}, \code{HarrysCostume}, \code{BestOf70s}, \code{BestOf80s}, \code{BestOf90s}
#' @param type Set to continuous if you require a gradient of colors similar to how heat map works.
#' @export
#' @keywords colors
#' @examples 
#' derrick.palette(3, "Office")
#' derrick.palette(3, "HarrysCostume")
#' derrick.palette(5, "BestOf70s")
#' # You can also request a continuous range of colors
#' pal <- derrick.palette(name = "StefansCostumeYoung", type = "continuous")
#' image(volcano, col = pal(21))

derrick.palette <- function(n, name, type = FALSE) {
	
Office <- c ("#989D7B", "#626A53", "#28593F", "#437E8F", "#886348", "#4B2C14")
StefansCostumeYoung <- c ("", "", "", "", "", "")
StefansCostumeOld <- c ("", "", "", "", "", "")
HarrysCostume <- c ("", "", "", "", "", "")
BestOf70s <- c ("#DFD2BA", "#B1825A", "#71543F", "#504A37")
BestOf80s <- c ("", "", "", "", "", "")
BestOf90s <- c ("", "", "", "", "", "")


if(!name %in% namelist$movies)
	stop("Palette not found.")

if(type == "continuous") {
colorRampPalette(get(name))
} else {
if(!type) {
if(n > namelist[which(namelist$movies == name), 2])
	stop("Number of requested colors greater than what palette can offer")

get(name)[1:n]
} 
}

}


#' Display a palette
#'
#' @param n Number of colors desired. Unfortunately most palettes now only have 4 or 5 colors. But hopefully we'll add more palettes soon. All color schemes are derived from the most excellent Tumblr blog: \href{http://wesandersonpalettes.tumblr.com/}{Wes Anderson Palettes}
#' @param  name Name of desired palette. Choices are: \code{Office}, \code{StefansCostumeYoung},  \code{StefansCostumeOld},  \code{HarrysCostume}, \code{BestOf70s},  \code{BestOf80s}, \code{BestOf90s}
#' @export
#' @examples \dontrun{
#'	display.derrick.palette(3, "Office")
#'}
display.derrick.palette <- function(n, name) {

if(!name %in% namelist$movies)
	stop("Palette not found.")

if(n > namelist[which(namelist$movies == name), 2])
	stop("Number of requested colors greater than what palette can offer")


   image(1:n,1,as.matrix(1:n),col= derrick.palette(n,name),
       xlab=name, ylab = "",xaxt = "n",yaxt = "n", bty = "n")

}



#' heatmap
#'
#' A heatmap example
#' @docType data
#' @keywords datasets
#' @name heatmap
NULL
