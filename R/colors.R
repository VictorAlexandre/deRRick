
#' An Inspector Derrick palette generator
#'
#' This code is inspired by the magnificent \href{https://github.com/karthik/wesanderson} {Wes Anderson package}. 
#' @param n Number of colors desired. Unfortunately most palettes now only have 4 or 5 colors. But hopefully we'll add more palettes soon. All color schemes are derived from the most excellent Tumblr blog: \href{http://wesandersonpalettes.tumblr.com/}{Wes Anderson Palettes}
#' @param  name Name of desired palette. Choices are: \code{GrandBudapest}, \code{Moonrise1},  \code{Royal1},  \code{Moonrise2}, \code{Cavalcanti},  \code{Royal2}, \code{GrandBudapest2},  \code{Moonrise3},  \code{Chevalier}, \code{BottleRocket}, \code{darjeeling}, \code{darjeeling2} 
#' @param type Set to continuous if you require a gradient of colors similar to how heat map works.
#' @export
#' @keywords colors
#' @examples 
#' wes.palette(3, "Office")
#' wes.palette(3, "HarrysCostume")
#' wes.palette(5, "BestOf70s")
#' # You can also request a continuous range of colors
#' pal <- wes.palette(name = "StefansCostumeYoung", type = "continuous")
#' image(volcano, col = pal(21))

wes.palette <- function(n, name, type = FALSE) {
	
Office <- c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236")
StefansCostumeYoung <- c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A")
StefansCostumeOld <- c("#899DA4", "#C93312", "#FAEFD1", "#DC863B")
HarrysCostume <- c("#798E87", "#C27D38", "#CCC591", "#29211F")
BestOf70s <- c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15")
BestOf80s <- c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089")
BestOf90s <- c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4")

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
#' @param  name Name of desired palette. Choices are: \code{GrandBudapest}, \code{Moonrise1},  \code{Royal1},  \code{Moonrise2}, \code{Cavalcanti},  \code{Royal2}, \code{GrandBudapest2},  \code{Moonrise3},  \code{Chevalier} , \code{BottleRocket} , \code{darjeeling}, \code{darjeeling2}
#' @export
#' @examples \dontrun{
#'	display.wes.palette(3, "Royal1")
#'}
display.wes.palette <- function(n, name) {

if(!name %in% namelist$movies)
	stop("Palette not found.")

if(n > namelist[which(namelist$movies == name), 2])
	stop("Number of requested colors greater than what palette can offer")


   image(1:n,1,as.matrix(1:n),col= wes.palette(n,name),
       xlab=name, ylab = "",xaxt = "n",yaxt = "n", bty = "n")

}



#' heatmap
#'
#' A heatmap example
#' @docType data
#' @keywords datasets
#' @name heatmap
NULL
