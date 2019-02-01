#' rtomahawk
#' 
#' Description of your package
#' 
#' @docType package
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#' @import Rcpp
#' @import GenomicRanges
#' @importFrom Rcpp evalCpp
#' @useDynLib rtomahawk, .registration = TRUE 
#' @name rtomahawk
NULL  

# Todo:
# haplotype(twk, ...)
# importTomahawk(input, output) < return twk with char-pointer to output

# Colors ------------------------------

twk_color_validator <- function(n, alpha, begin, end, direction)
{
    if (begin < 0 | begin > 1 | end < 0 | end > 1) {
        stop("begin and end must be in [0,1]")
    }
    if (abs(direction) != 1) {
        stop("direction must be 1 or -1")
    }
}

twk_color_mapper <- function(name, n, alpha = 1, begin = 0, end = 1, direction = 1)
{
    map <- rtomahawk:::twk_colors[rtomahawk:::twk_colors$opt == name, ]
    map_cols <- grDevices::rgb(map$R, map$G, map$B)
    fn_cols <- grDevices::colorRamp(map_cols, space = "Lab",  interpolate = "spline")
    cols <- fn_cols(seq(begin, end, length.out = n))/255
    return(grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = alpha))
}

#' @export
viridis <- function(n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
    rtomahawk:::twk_color_validator(n, alpha, begin, end, direction)

    if (direction == -1) {
        tmp <- begin
        begin <- end
        end <- tmp
    }

    return(rtomahawk:::twk_color_mapper("viridis", n, alpha, begin, end, direction))
}

#' @export
cividis <- function(n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
    rtomahawk:::twk_color_validator(n, alpha, begin, end, direction)

    if (direction == -1) {
        tmp <- begin
        begin <- end
        end <- tmp
    }

    return(rtomahawk:::twk_color_mapper("cividis", n, alpha, begin, end, direction))
}

#' @export
plasma <- function(n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
    rtomahawk:::twk_color_validator(n, alpha, begin, end, direction)

    if (direction == -1) {
        tmp <- begin
        begin <- end
        end <- tmp
    }

    return(rtomahawk:::twk_color_mapper("plasma", n, alpha, begin, end, direction))
}

#' @export
inferno <- function(n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
    rtomahawk:::twk_color_validator(n, alpha, begin, end, direction)

    if (direction == -1) {
        tmp <- begin
        begin <- end
        end <- tmp
    }

    return(rtomahawk:::twk_color_mapper("inferno", n, alpha, begin, end, direction))
}

#' @export
magma <- function(n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
    rtomahawk:::twk_color_validator(n, alpha, begin, end, direction)

    if (direction == -1) {
        tmp <- begin
        begin <- end
        end <- tmp
    }

    return(rtomahawk:::twk_color_mapper("magma", n, alpha, begin, end, direction))
}

#' Displays the color schemes available in rtomahawk
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#' 
#' @export
#' @examples
#' displayColors()
displayColors <- function(){
    cur_par <- par()
    par(mar=c(1,1,1,1),mfrow=c(1,2))
    # 100 steps
    plot(-100,-100,xlim=c(0,101),ylim=c(0,450),axes=F,xlab="",ylab="",main="Gradient")
    for(i in 0:100) rect(xleft = i, ybottom = 0, xright = i+1,ytop = 50,col = colorRampPalette(c("red","blue"))(100)[i],border=NA)
    for(i in 0:100) rect(xleft = i, ybottom = 75, xright = i+1,ytop = 125,col = cividis(100)[i],border=NA)
    for(i in 0:100) rect(xleft = i, ybottom = 150, xright = i+1,ytop = 200,col = inferno(100)[i],border=NA)
    for(i in 0:100) rect(xleft = i, ybottom = 225, xright = i+1,ytop = 275,col = magma(100)[i],border=NA)
    for(i in 0:100) rect(xleft = i, ybottom = 300, xright = i+1,ytop = 350,col = plasma(100)[i],border=NA)
    for(i in 0:100) rect(xleft = i, ybottom = 375, xright = i+1,ytop = 425,col = viridis(100)[i],border=NA)
    text(x = 50, y = 50+10, labels = "default")
    text(x = 50, y = 125+10, labels = "cividis")
    text(x = 50, y = 200+10, labels = "inferno")
    text(x = 50, y = 275+10, labels = "magma")
    text(x = 50, y = 350+10, labels = "plasma")
    text(x = 50, y = 425+10, labels = "viridis")

    # 11 steps
    plot(-100,-100,xlim=c(0,12),ylim=c(0,450),axes=F,xlab="",ylab="",main="Default: 11 steps")
    for(i in 0:11) rect(xleft = i, ybottom = 0, xright = i+1,ytop = 50,col = colorRampPalette(c("red","blue"))(11)[i],border=NA)
    for(i in 0:11) rect(xleft = i, ybottom = 75, xright = i+1,ytop = 125,col = cividis(11)[i],border=NA)
    for(i in 0:11) rect(xleft = i, ybottom = 150, xright = i+1,ytop = 200,col = inferno(11)[i],border=NA)
    for(i in 0:11) rect(xleft = i, ybottom = 225, xright = i+1,ytop = 275,col = magma(11)[i],border=NA)
    for(i in 0:11) rect(xleft = i, ybottom = 300, xright = i+1,ytop = 350,col = plasma(11)[i],border=NA)
    for(i in 0:11) rect(xleft = i, ybottom = 375, xright = i+1,ytop = 425,col = viridis(11)[i],border=NA)
    text(x = 6, y = 50+10, labels = "default")
    text(x = 6, y = 125+10, labels = "cividis")
    text(x = 6, y = 200+10, labels = "inferno")
    text(x = 6, y = 275+10, labels = "magma")
    text(x = 6, y = 350+10, labels = "plasma")
    text(x = 6, y = 425+10, labels = "viridis")
    suppressWarnings(par(cur_par))
}


# Support --------------------------

#' Print Tomahawk version string
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @return Prints the used version of rtomahawk and the C/C++ 
#'    shared object versions used by Tomahawk.
#' 
#' @useDynLib rtomahawk, .registration = TRUE 
#' @importFrom Rcpp evalCpp
#' @export
#' @examples
#' tomahawkVersion()
setGeneric("tomahawkVersion", function(x="ANY",...) standardGeneric("tomahawkVersion"))
setMethod("tomahawkVersion",
    signature(x="ANY"),
    definition = function(x,...){
        cat(
            sprintf("rtomahawk: %s\n%s\n", 
                as.character(packageVersion("rtomahawk")), 
                rtomahawk:::.twk_version()
            ), 
        sep="")
        invisible(NULL)
    }
)

#' Plots a cytoband
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param chr Target chromosome name.
#' @param release Target release name. Currently supported values are "hg19" and "hg38".
#' @param zoom Tuple (from,to) for a region to highlight.
#'
#' @export
#' @examples
#' plotCytoband("chr6", "hg19")
#' plotCytoband("chr6", "hg19", zoom = c(10e6,20e6))
plotCytoband <- function(chr, release = "hg19", zoom = NULL){
    if(class(chr) != "character") stop("chr must be a string")
    if(class(release) != "character") stop("release must be a string")
    if(!chr %in% cytobands$chr) stop("chr does not exist in set")
    if(!release %in% cytobands$release) stop("release not provided")
    has_valid_zoom <- FALSE
    if(!is.null(zoom)){
        if(class(zoom) != "numeric") stop("zoom has to be numeric")
        if(length(zoom) != 2) stop("zoom has to have two values (from,to)")
        has_valid_zoom <- TRUE
    }
    
    c1 <- cytobands[cytobands$chr==chr & cytobands$release == release,]
    plot(-100,-100,ylim=c(0,1),xlim=c(0, max(c1$to)), xlab="", xaxt="n", ylab="", yaxt="n", xaxs="i", yaxs="i", bty="n")
    
    # Colour scheme from circos.
    color.table <- c(gneg   = "#FFFFFF", gpos25  = "#C8C8C8", 
                     gpos33 = "#D2D2D2", gpos50  = "#C8C8C8", gpos66 = "#A0A0A0", 
                     gpos75 = "#828282", gpos100 = "#000000", gpos   = "#000000", 
                     stalk  = "#647FA4", acen    = "#D92F27", gvar   = "#DCDCDC")
    
    acen <- c1[c1$stain=="acen",]
    c1 <-c1[c1$stain!="acen",]
    
    for(i in 1:nrow(c1)){
        rect(xleft = c1$from[i], ybottom = 0.1, xright = c1$to[i], ytop = 0.9, col = color.table[c1$stain[i]])
    }
    
    # Add special triangular polygon to indicate the centromere.
    polygon(x = c(acen$from[1],acen$to[1],acen$from[1]), y = c(0.9,0.45,0.1), col="red")
    polygon(x = c(acen$to[2],acen$from[2],acen$to[2]), y = c(0.9,0.45,0.1), col="red")
    mtext(chr,side=2,las=2,cex=.8)
    
    if(has_valid_zoom) rect(zoom[1],0,zoom[2],1,col=rgb(colorRamp("yellow")(1)/255,alpha=0.7))
}


# Classes ------------------------------------

#' Tomahawk output data class
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @slot data Main storage for data 
#' @slot offset Data frame of offsets vectors
#' 
#' @seealso \code{\link{twk_index}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
twk_data <- setClass("twk_data",
    slots = c(data = "data.frame",
              offset = "vector"))

#' Tomahawk output index class
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @slot state Integer representing the sorted state of the file
#' @slot records Data frame of index records corresponding to the boundaries of each TWO block
#' @slot meta Data frame of meta index records (collapsed records for each chromosome). This slot will only have values if the input file is sorted.
#' 
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
twk_index <- setClass("twk_index",
    slots = c(state = "numeric",
              records = "data.frame",
              meta = "data.frame"))

#' Tomahawk output header class
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @slot contigs Integer representing the sorted state of the file
#' @slot samples Data frame of index records corresponding to the boundaries of each TWO block
#' @slot literals Data frame of meta index records (collapsed records for each chromosome). This slot will only have values if the input file is sorted.
#' 
#' @seealso \code{\link{twk_index}}, \code{\link{twk_data}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
twk_header <- setClass("twk_header",
    slots = c(contigs = "data.frame", 
              samples = "vector",
              literals = "vector"))

#' Tomahawk output filter class
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @slot flagInclude Integer representing FLAGs to include.
#' @slot flagExclude Integer representing FLAGs to exclude.
#' @slot minD Smallest Lewontin D value.
#' @slot maxD Largest Lewontin D value.
#' @slot minDprime Smallest scaled Lewontin D value.
#' @slot minDprime Largest scald Lewontin D value.
#' @slot minR Smallest Pearson's correlation coefficient value.
#' @slot minR Largest Pearson's correlation coefficient value.
#' @slot minR2 Smallest scaled Pearson's correlation coefficient value.
#' @slot minR2 Largest scaled Pearson's correlation coefficient value.
#' @slot minP Smallest Fisher's exact test P-value.
#' @slot minP Largest Fisher's exact test P-value.
#' @slot minP1 Smallest count of REF-REF.
#' @slot minP1 Largest count of REF-REF.
#' @slot minP2 Smallest count of REF-ALT.
#' @slot minP2 Largest count of REF-ALT.
#' @slot minQ1 Smallest count of ALT-REF.
#' @slot minQ1 Largest count of ALT-REF.
#' @slot minQ2 Smallest count of ALT-ALT.
#' @slot minQ2 Largest count of ALT-ALT.
#' @slot minChiSqFisher Smallest Chi-squared test of the 2x2 haplotype table.
#' @slot minChiSqFisher Largest Chi-squared test of the 2x2 haplotype table.
#' @slot minChiSqModel Smallest Chi-squared test of the 4x4 genotype table.
#' @slot minChiSqModel Largest Chi-squared test of the 4x4 genotype table.
#' @slot upperOnly Logical flag indicating whether upper triangular values only should be kept.
#' @slot lowerOnly Logical flag indicating whether lower triangular values only should be kept.
#'
#' @seealso \code{\link{twk_index}}, \code{\link{twk_data}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
twk_filter <- setClass("twk_filter",
    slots = c(flagInclude = "numeric",
              flagExclude = "numeric",
              minD = "numeric",
              maxD = "numeric",
              minDprime = "numeric",
              maxDprime = "numeric",
              minR = "numeric",
              maxR = "numeric",
              minR2 = "numeric",
              maxR2 = "numeric",
              minP = "numeric",
              maxP = "numeric",
              minP1 = "numeric",
              maxP1 = "numeric",
              minP2 = "numeric",
              maxP2 = "numeric",
              minQ1 = "numeric",
              maxQ1 = "numeric",
              minQ2 = "numeric",
              maxQ2 = "numeric",
              minChiSqFisher = "numeric",
              maxChiSqFisher = "numeric",
              minChiSqModel = "numeric",
              maxChiSqModel = "numeric",
              upperOnly = "logical",
              lowerOnly = "logical"))


#' Set filters for a Tomahawk output records
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @slot flagInclude Integer representing FLAGs to include.
#' @slot flagExclude Integer representing FLAGs to exclude.
#' @slot minD Smallest Lewontin D value.
#' @slot maxD Largest Lewontin D value.
#' @slot minDprime Smallest scaled Lewontin D value.
#' @slot minDprime Largest scald Lewontin D value.
#' @slot minR Smallest Pearson's correlation coefficient value.
#' @slot minR Largest Pearson's correlation coefficient value.
#' @slot minR2 Smallest scaled Pearson's correlation coefficient value.
#' @slot minR2 Largest scaled Pearson's correlation coefficient value.
#' @slot minP Smallest Fisher's exact test P-value.
#' @slot minP Largest Fisher's exact test P-value.
#' @slot minP1 Smallest count of REF-REF.
#' @slot minP1 Largest count of REF-REF.
#' @slot minP2 Smallest count of REF-ALT.
#' @slot minP2 Largest count of REF-ALT.
#' @slot minQ1 Smallest count of ALT-REF.
#' @slot minQ1 Largest count of ALT-REF.
#' @slot minQ2 Smallest count of ALT-ALT.
#' @slot minQ2 Largest count of ALT-ALT.
#' @slot minChiSqFisher Smallest Chi-squared test of the 2x2 haplotype table.
#' @slot minChiSqFisher Largest Chi-squared test of the 2x2 haplotype table.
#' @slot minChiSqModel Smallest Chi-squared test of the 4x4 genotype table.
#' @slot minChiSqModel Largest Chi-squared test of the 4x4 genotype table.
#' @slot upperOnly Logical flag indicating whether upper triangular values only should be kept.
#' @slot lowerOnly Logical flag indicating whether lower triangular values only should be kept.
#'
#' @seealso \code{\link{twk_index}}, \code{\link{twk_data}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
setGeneric("setFilters", 
    function(x="ANY", 
             flagInclude = "numeric",
             flagExclude = "numeric",
             minD = "numeric",
             maxD = "numeric",
             minDprime = "numeric",
             maxDprime = "numeric",
             minR = "numeric",
             maxR = "numeric",
             minR2 = "numeric",
             maxR2 = "numeric",
             minP = "numeric",
             maxP = "numeric",
             minP1 = "numeric",
             maxP1 = "numeric",
             minP2 = "numeric",
             maxP2 = "numeric",
             minQ1 = "numeric",
             maxQ1 = "numeric",
             minQ2 = "numeric",
             maxQ2 = "numeric",
             minChiSqFisher = "numeric",
             maxChiSqFisher = "numeric",
             minChiSqModel = "numeric",
             maxChiSqModel = "numeric",
             upperOnly = "logical",
             lowerOnly = "logical",
             ...)
    { 
        standardGeneric("setFilters")
    }
)

setMethod("setFilters",
    signature(x="ANY"),
    definition = function(x = NULL, 
                          flagInclude = 4294967295, flagExclude = 0, 
                          minD = -999999999, maxD = 999999999, 
                          minDprime = 0, maxDprime = 999999999, 
                          minR = -999999999, maxR = 999999999, 
                          minR2 = 0, maxR2 = 999999999, 
                          minP = 0, maxP = 999999999, 
                          minP1 = 0, maxP1 = 999999999, 
                          minP2 = 0, maxP2 = 999999999, 
                          minQ1 = 0, maxQ1 = 999999999, 
                          minQ2 = 0, maxQ2 = 999999999, 
                          minChiSqFisher = 0, maxChiSqFisher = 999999999, 
                          minChiSqModel = 0, maxChiSqModel = 999999999, 
                          upperOnly = FALSE, lowerOnly = FALSE, 
                          ...)
    {
        y <- new("twk_filter")
        y@flagInclude = flagInclude
        y@flagExclude = flagExclude
        y@minD = minD
        y@maxD = maxD
        y@minDprime = minDprime
        y@maxDprime = maxDprime
        y@minR = minR
        y@maxR = maxR
        y@minR2 = minR2
        y@maxR2 = maxR2
        y@minP = minP
        y@maxP = maxP
        y@minP1 = minP1
        y@maxP1 = maxP1
        y@minP2 = minP2
        y@maxP2 = maxP2
        y@minQ1 = minQ1
        y@maxQ1 = maxQ1
        y@minQ2 = minQ2
        y@maxQ2 = maxQ2
        y@minChiSqFisher = minChiSqFisher
        y@maxChiSqFisher = maxChiSqFisher
        y@minChiSqModel = minChiSqModel
        y@maxChiSqModel = maxChiSqModel
        if(upperOnly & lowerOnly){
            upperOnly <- FALSE
            lowerOnly <- FALSE
        }
        y@upperOnly = upperOnly
        y@lowerOnly = lowerOnly
        return(y)
    }
)

setMethod("setFilters",
    signature(x="twk_filter"),
    definition = function(x, 
                          flagInclude = 4294967295, flagExclude = 0, 
                          minD = -999999999, maxD = 999999999, 
                          minDprime = 0, maxDprime = 999999999, 
                          minR = -999999999, maxR = 999999999, 
                          minR2 = 0, maxR2 = 999999999, 
                          minP = 0, maxP = 999999999, 
                          minP1 = 0, maxP1 = 999999999, 
                          minP2 = 0, maxP2 = 999999999, 
                          minQ1 = 0, maxQ1 = 999999999, 
                          minQ2 = 0, maxQ2 = 999999999, 
                          minChiSqFisher = 0, maxChiSqFisher = 999999999, 
                          minChiSqModel = 0, maxChiSqModel = 999999999, 
                          upperOnly = FALSE, lowerOnly = FALSE, 
                          ...)
    {
        x@flagInclude = flagInclude
        x@flagExclude = flagExclude
        x@minD = minD
        x@maxD = maxD
        x@minDprime = minDprime
        x@maxDprime = maxDprime
        x@minR = minR
        x@maxR = maxR
        x@minR2 = minR2
        x@maxR2 = maxR2
        x@minP = minP
        x@maxP = maxP
        x@minP1 = minP1
        x@maxP1 = maxP1
        x@minP2 = minP2
        x@maxP2 = maxP2
        x@minQ1 = minQ1
        x@maxQ1 = maxQ1
        x@minQ2 = minQ2
        x@maxQ2 = maxQ2
        x@minChiSqFisher = minChiSqFisher
        x@maxChiSqFisher = maxChiSqFisher
        x@minChiSqModel = minChiSqModel
        x@maxChiSqModel = maxChiSqModel
        x@upperOnly = upperOnly
        x@lowerOnly = lowerOnly
        return(x)
    }
)

#' Tomahawk output class
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @slot file.path Character file path to the target TWO file
#' @slot index Instance of \code{\link{twk_index}}
#' @slot header Instance of \code{\link{twk_header}}
#' @slot data Instance of \code{\link{twk_data}}
#' 
#' @seealso \code{\link{twk_index}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
twk <- setClass("twk",
    slots = c(file.path = "character",
              index = "twk_index",
              header = "twk_header",
              data = "twk_data"))

# Show methods
setMethod("show",
    signature = "twk_header",
    definition = function(object){
        cat("An object of class ", class(object), "\n", sep = "")
        cat("Number of samples ", length(object@samples), " and contigs ", nrow(object@contigs), "\n", sep = "")
        invisible(NULL)
    }
)

setMethod("show",
    signature = "twk_index",
    definition = function(object){
        cat("An object of class ", class(object), "\n", sep = "")
        if(object@state == 1) cat("State: sorted", "\n", sep="")
        else cat("State: unsorted", "\n", sep="")
        cat("Number of blocks ", nrow(object@records),"\nrecords ", sum(as.numeric(object@records$n)), " \nUncompressed ", sum(as.numeric(object@records$b_unc)), "b\nCompressed ", sum(as.numeric(object@records$b_cmp)), "b\n", sep = "")
        invisible(NULL)
    }
)

setMethod("show",
    signature = "twk_data",
    definition = function(object){
        cat("An object of class ", class(object), "\n", sep = "")
        cat("Data dimension [", ncol(object@data), ",", nrow(object@data), "]", "\n", sep = "")
        if(nrow(object@data)){
            if(nrow(object@data) > 11){
                print(object@data[1:5,])
                cat("...", "\n", sep = "")
                print(object@data[(nrow(object@data)-5):nrow(object@data),])
            } else {
                print(object@data)
            }
        }
        invisible(NULL)
    }
)

setMethod("show",
    signature = "twk",
    definition = function(object){
        cat("An object of class ", class(object), "\n", sep = "")
        if(nrow(object@data@data)){
            cat("Has internal data: ", nrow(object@data@data), " records\n", sep = "")
            if(nrow(object@data@data) > 11){
                print(object@data@data[1:5,])
                cat("...", "\n", sep = "")
                print(object@data@data[(nrow(object@data@data)-5):nrow(object@data@data),])
            } else {
                print(object@data@data)
            }
        }
        invisible(NULL)
    }
)

setGeneric("head", function(x="twk", ...){ standardGeneric("head") })
setMethod("head",
    signature(x="twk"),
    definition = function(x, n = 10, ...){
        args1 <- list(n = n)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(nchar(x@file.path) == 0) stop("class file.path is empty")
        if(args1$n <= 0) return(NULL)
        return(rtomahawk:::.twk_head(x, args1$n));
    }
)

setGeneric("tail", function(x="twk", ...){ standardGeneric("tail") })
setMethod("tail",
    signature(x="twk"),
    definition = function(x, n = 10, ...){
        args1 <- list(n = n)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(nchar(x@file.path) == 0) stop("class file.path is empty")
        if(args1$n <= 0) return(NULL)
        return(rtomahawk:::.twk_tail(x, args1$n));
    }
)

setMethod("[",
    signature(x="twk"),
    definition = function(x, i, j, drop = if (missing(i)) TRUE else FALSE)
    {
        if (missing(j)) return(x@data@data[i,,drop=drop])
        if (missing(i)) return(x@data@data[,j,drop=drop])
        return(x@data@data[i,j,drop=drop])
    }
)

# Todo
setMethod("[<-",
    signature(x="twk"),
    definition = function(x, i, j, ..., value){
        #args1 <- list(x = x, i = i, j = j, drop = drop)
        #inargs <- list(...)
        #args1[names(inargs)] <- inargs
        return(TRUE)
    }
)

setMethod("summary",
    signature(object="twk"),
    definition = function(object){
        if(nrow(object@data@data) == 0) return(NULL)
        return(do.call("rbind",lapply(object@data@data, summary, 2)))
    }
)

setMethod("dim",
    signature(x="twk"),
    definition = function(x){
        if(nrow(x@data@data) == 0) return(c(0,0))
        return(dim(x@data@data))
    }
)

# Import --------------------------

#' Imports a htslib-compatible variant-call file into Tomahawk
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param input Input string pointing to the htslib-compatible file.
#' @param output Output string pointing to the Tomahawk file.
#' @param missnigness Numerical value in the range [0, 1] representing the
#'    largest fraction of missing values that are allowed. If the threshold is
#'    violated then the variant site is filtered out.
#' @param block_size Advanced use: number of variants packed in each internal Tomahawk
#'    block. This could have potential impact on the parallel throughput capabilities
#'    of Tomahawk on large cohorts.
#' @param compression_level Compressing level in the range [1, 22] for compressing the
#'    Tomahawk file. Larger numbers results in smaller resting file size at the expense
#'    of importing speeds. Decompressing speeds are largely unaffected by the compression 
#'    level.
#' @param filter_univariate Logical (boolean) flag set to \code{TRUE} if univariate (monomorphic)
#'    should be filtered out. If this flag is set to \code{FALSE} then monomorphic sites
#'    are left in the file. The recommendation is to remove these as they contribute no
#'    information.
#'
#' @return Returns a \code{twk} class with the file handle pointing to the newly imported
#'    Tomahawk file.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @useDynLib rtomahawk, .registration = TRUE 
#' @importFrom Rcpp evalCpp
#' @examples
#' # This example assumes you have a Tomahawk file called "1kgp3_chr20.bcf" in
#' # your current working directory.
#' twk <- import("1kgp3_chr20.bcf","~/Downloads/1kgp3_chr20")
#' twk
setGeneric("import", function(input="character", output="character", missingness = "numeric", block_size = "integer", compression_level = "integer", filter_univariate = "logical", ...){ standardGeneric("import") })
setMethod("import",
    signature(input="character"),
    definition = function(input, output, missingness = 0.95, block_size = 500, compression_level = 1, filter_univariate = TRUE, ...){
        if(nchar(input) == 0) stop("no input provided")
        if(nchar(output) == 0) stop("no output provided")
        if(missingness < 0 || missingness > 1) stop("missingness must be in range [0,1]")
        if(block_size < 2) stop("block_size must be > 1")
        if(compression_level < 1) stop("compressing level must be > 0")

        return(rtomahawk:::.twk_import(input, output, missingness, block_size, compression_level, filter_univariate));
    }
)

# Support -------------------------

addGenomicAxis <- function(limit, at = 1, las1 = 1, scaleMax = FALSE){
    if(class(limit) != "numeric") stop("limit must be numeric")
    if(length(limit) != 2) stop("limit must be of size 2")
    
    if(abs(limit[2] - limit[1]) < 1e3){ 
        print("in < 1e3")
        tick_pos <- seq(round(min(limit),-2),round(max(limit),-2), by = 1e2)
        print(tick_pos)
        if(round(max(limit),-3) > 1e6) tick_labels <- paste0(tick_pos/1e6,"Mb")
        else tick_labels <- paste0(tick_pos/1e3,"Kb")
        axis(at, at=tick_pos, labels=tick_labels, las = las1)
        
        tick_pos_add <- seq(round(min(limit),-2),round(max(limit),-2), by = 10)
        tick_pos_add <- tick_pos_add[!tick_pos_add%in%tick_pos]
        suppressWarnings(rug(x = tick_pos_add, ticksize = -0.02, side = at, col = "darkgrey"))
    }
    else if(abs(limit[2] - limit[1]) < 10e3){ 
        print("in < 10e3")
        tick_pos <- seq(round(min(limit),-3),round(max(limit),-3), by = 1e3)
        if(round(max(limit),-3) > 1e6) tick_labels <- paste0(tick_pos/1e6,"Mb")
        else tick_labels <- paste0(tick_pos/1e3,"Kb")
        axis(at, at=tick_pos, labels=tick_labels, las = las1)
        
        tick_pos_add <- seq(round(min(limit),-3),round(max(limit),-3), by = 100)
        tick_pos_add <- tick_pos_add[!tick_pos_add%in%tick_pos]
        suppressWarnings(rug(x = tick_pos_add, ticksize = -0.02, side = at, col = "darkgrey"))
    }
    else if(abs(limit[2] - limit[1]) < 100e3){ 
        print("in < 100e3")
        tick_pos <- seq(round(min(limit),-4),round(max(limit),-4), by = 10e3)
        drop_lower <- FALSE
        if(length(tick_pos) < 4){
            tick_pos <- unique(c(round(min(limit),-3)-1e3, seq(round(min(limit),-3),round(max(limit),-3), by = 1e3), round(max(limit),-3)+1e3))
            drop_lower <- TRUE
        }
        
        if(round(max(limit),-3) > 1e6) tick_labels <- paste0(tick_pos/1e6,"Mb")
        else tick_labels <- paste0(tick_pos/1e3,"Kb")
        
        if(!scaleMax) axis(at, at=tick_pos, labels=tick_labels, las = las1)
        else axis(at, at=tick_pos/max(limit), labels=tick_labels, las = las1)
        
        if(!drop_lower) tick_pos_add <- seq(round(min(limit),-4),round(max(limit),-4), by = 1e3)
        else tick_pos_add <- seq(min(tick_pos), max(tick_pos), by = 100)
        
        tick_pos_add <- tick_pos_add[!tick_pos_add%in%tick_pos]
        if(!scaleMax) suppressWarnings(rug(x = tick_pos_add, ticksize = -0.02, side = at, col = "darkgrey"))
        else suppressWarnings(rug(x = tick_pos_add / max(limit), ticksize = -0.02, side = at, col = "darkgrey"))
    }
    else if(abs(limit[2] - limit[1]) < 1e6){ 
        print("in < 1e6")
        tick_pos <- seq(round(min(limit),-5),round(max(limit),-5), by = 100e3)
        drop_lower <- FALSE
        if(length(tick_pos) < 4){
            tick_pos <- unique(c(round(min(limit),-4)-10e3, seq(round(min(limit),-4),round(max(limit),-4), by = 10e3), round(max(limit),-4)+10e3))
            drop_lower <- TRUE
        }
        
        if(round(max(limit),-3) > 1e6) tick_labels <- paste0(tick_pos/1e6,"Mb")
        else tick_labels <- paste0(tick_pos/1e3,"Kb")
        
        if(!scaleMax) axis(at, at=tick_pos, labels=tick_labels, las = las1)
        else axis(at, at=tick_pos/max(limit), labels=tick_labels, las = las1)
        
        if(!drop_lower) tick_pos_add <- seq(round(min(limit),-5),round(max(limit),-5), by = 10e3)
        else tick_pos_add <- seq(min(tick_pos), max(tick_pos), by = 1e3)
        
        tick_pos_add <- tick_pos_add[!tick_pos_add%in%tick_pos]
        if(!scaleMax) suppressWarnings(rug(x = tick_pos_add, ticksize = -0.02, side = at, col = "darkgrey"))
        else suppressWarnings(rug(x = tick_pos_add / max(limit), ticksize = -0.02, side = at, col = "darkgrey"))
    }
    else if(abs(limit[2] - limit[1]) < 10e6){ 
        print("in < 10e6")
        tick_pos <- seq(round(min(limit),-6),round(max(limit),-6), by = 1e6)
        drop_lower <- FALSE
        if(length(tick_pos) < 4){
            tick_pos <- unique(c(round(min(limit),-5)-100e3, seq(round(min(limit),-5),round(max(limit),-5), by = 100e3), round(max(limit),-5)+100e3))
            drop_lower <- TRUE
        }
        
        if(round(max(limit),-3) > 1e6) tick_labels <- paste0(tick_pos/1e6,"Mb")
        else tick_labels <- paste0(tick_pos/1e3,"Kb")
        
        if(!scaleMax) axis(at, at=tick_pos, labels=tick_labels, las = las1)
        else axis(at, at=tick_pos/max(limit), labels=tick_labels, las = las1)
        
        if(!drop_lower) tick_pos_add <- seq(round(min(limit),-6),round(max(limit),-6), by = 100e3)
        else tick_pos_add <- seq(min(tick_pos), max(tick_pos), by = 10e3)
        
        tick_pos_add <- tick_pos_add[!tick_pos_add%in%tick_pos]
        if(!scaleMax) suppressWarnings(rug(x = tick_pos_add, ticksize = -0.02, side = at, col = "darkgrey"))
        else suppressWarnings(rug(x = tick_pos_add / max(limit), ticksize = -0.02, side = at, col = "darkgrey"))
    }
    else if(abs(limit[2] - limit[1]) < 100e6){ 
        print("in < 100e6")
        tick_pos <- seq(round(min(limit),-7),round(max(limit),-7), by = 10e6)
        drop_lower <- FALSE
        if(length(tick_pos) < 4){
            tick_pos <- unique(c(round(min(limit),-6)-1e6, seq(round(min(limit),-6),round(max(limit),-6), by = 1e6), round(max(limit),-6)+1e6))
            drop_lower <- TRUE
        }

        if(round(max(limit),-3) > 1e6) tick_labels <- paste0(tick_pos/1e6,"Mb")
        else tick_labels <- paste0(tick_pos/1e3,"Kb")
        
        if(!scaleMax) axis(at, at=tick_pos, labels=tick_labels, las = las1)
        else axis(at, at=tick_pos/max(limit), labels=tick_labels, las = las1)
        
        if(!drop_lower) tick_pos_add <- seq(round(min(limit),-7),round(max(limit),-7), by = 1e6)
        else tick_pos_add <- seq(min(tick_pos), max(tick_pos), by = 100e3)
        
        tick_pos_add <- tick_pos_add[!tick_pos_add%in%tick_pos]
        if(!scaleMax) suppressWarnings(rug(x = tick_pos_add, ticksize = -0.02, side = at, col = "darkgrey"))
        else suppressWarnings(rug(x = tick_pos_add / max(limit), ticksize = -0.02, side = at, col = "darkgrey"))
    }
}

# Basic ---------------------------

#' Open a file-handle to a Tomahawk object on disk
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param input Input string pointing to the Tomahawk output file.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @useDynLib rtomahawk, .registration = TRUE 
#' @importFrom Rcpp evalCpp
#' @examples
#' # This example assumes you have a Tomahawk file called "1kgp3_chr6.two" in
#' # your current working directory.
#' f<-setFilters(minR2=0.5)
#' twk<-openTomahawkOutput("1kgp3_chr6.two")
#' twk
setGeneric("openTomahawkOutput", function(input="character", ...){ standardGeneric("openTomahawkOutput") })
setMethod("openTomahawkOutput",
    signature(input="character"),
    definition = function(input, ...){
        if(nchar(input) == 0) stop("no input provided")
        
        args1 <- list(input = input)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        return(rtomahawk:::.OpenTomahawkOutput(input));
    }
)

#' Reads Tomahawk output records into memory
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param x Input Tomahawk class.
#' @param intervals Filter the records using the provided intervals. The
#'    intervals can be in either character form or as a \code{GenomicRanges}
#'    object.
#' @param filters Filter the records using this \code{twk_filter} class.
#'    This parameter can be set to \code{null} to skip all filtering
#'    procedures and read all records as is.
#' @param really Logical flag to prevent plotting too many points.
#'    Set this to \code{TRUE} at your own peril.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @useDynLib rtomahawk, .registration = TRUE 
#' @importFrom Rcpp evalCpp
#' @importFrom GenomicRanges GRanges
#' @examples
#' # This example assumes you have a Tomahawk file called "1kgp3_chr6.two" in
#' # your current working directory.
#' f<-setFilters(minR2=0.5)
#' twk<-openTomahawkOutput("1kgp3_chr6.two")
#' y<-readRecords(twk,"6:5e6-10e6",f,really=TRUE)
#' y
#' # Example using GenomicRanges class.
#' require(GenomicRanges)
#' g <- GRanges("6", IRanges(6e6, 7e6))
#' y<-readRecords(twk,g,f,really=TRUE)
#' y
setGeneric("readRecords", function(x="twk", intervals = "ANY", filters = "ANY", really = "logical", ...){ standardGeneric("readRecords") })
setMethod("readRecords",
    signature(x="twk", intervals = "ANY"),
    definition = function(x, intervals, filters, really = FALSE, ...){
        if(missing(filters)) filters = setFilters()
        if(class(filters) != "twk_filter") stop("incorrect class")
        
        if(!missing(intervals)){
            if(class(intervals) == "character"){
                for(i in 1:length(intervals)){
                    interval_type <- rtomahawk:::.checkInterval(intervals[i])
                    if(interval_type <= 0) stop("illegal interval")
                }
            } else stop("illegal interval type: only character accepted")
        }

        args1 <- list(x = x, really = really)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        if(missing(intervals)) return(rtomahawk:::.ReadRecords(x, filters, really))
        else return(rtomahawk:::.ReadRecordsIntervals(x, filters, intervals, really))
    }
)

setMethod("readRecords",
    signature(x="twk", intervals = "GRanges"),
    definition = function(x, intervals, filters, really = FALSE, ...){
        if(missing(filters)) filters = setFilters()
        if(class(filters) != "twk_filter") stop("incorrect class")

        if(!missing(intervals)){
            if(class(intervals) == "GRanges"){
                intervals <- paste0(rep(intervals@seqnames@values,intervals@seqnames@lengths),":",intervals@ranges@start,"-",intervals@ranges@start+g@ranges@width)
            } else stop("illegal class of intervals")
        }

        args1 <- list(x = x, really = really)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        if(missing(intervals)) return(rtomahawk:::.ReadRecords(x, filters, really))
        else return(rtomahawk:::.ReadRecordsIntervals(x, filters, intervals, really))
    }
)

#' Plot LD triangle
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param x Input Tomahawk class.
#' @param from Start position in base-pairs.
#' @param to End position in base-pairs.
#' @param colors Vector of colors used to plot with. These are different from \code{col}
#'    as these colors are internally parsed to add opacity.
#' @param opacity Logical flag/numerical value controlling opacity levels of colors. If
#'    set to the logical \code{FALSE}, colours will have a uniform opacity of 1. If set
#'    to a numerical value, colours will uniformly share that opacity level. If unset,
#'    opacity will be a gradient from [0.1, 1] starting at R2 > 0.1.
#' @param annotate Logical flag for setting header title and subtitle overlay on plot.
#' @param really Logical flag to prevent plotting too many points.
#'    Set this to \code{TRUE} at your own peril.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @useDynLib rtomahawk, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom grDevices col2rgb rgb
#' @examples
#' # This example assumes you have a Tomahawk file called "1kgp3_chr6.two" in
#' # your current working directory.
#' f<-setFilters(minR2=0.5)
#' twk<-openTomahawkOutput("1kgp3_chr6.two")
#' y<-readRecords(twk,f,"6:5e6-10e6",really=TRUE)
#' plotLDTriangular(y, ylim=c(0, 500e3), xlim=c(6e6, 7e6))
setGeneric("plotLDTriangular", function(x="twk", from = "ANY", to = "ANY", orientation = "numeric", colors = "ANY", bg = "ANY", opacity = "ANY", annotate = "logical", really = "logical", ...){ standardGeneric("plotLDTriangular") })
setMethod("plotLDTriangular",
    signature(x="twk"),
    definition = function(x, from = NULL, to = NULL, orientation = 1, colors = NULL, bg = NULL, opacity = TRUE, annotate = TRUE, really = FALSE, ...){
        if(nrow(x@data@data) == 0) stop("no data available")
        if(length(table(y@data@data$ridA)) != 1) stop("non-unqique ridA")
        if(length(table(y@data@data$ridB)) != 1) stop("non-unqique ridB")
        if(all(y@data@data$ridA==y@data@data$ridB) == FALSE) stop("all ridA != ridB")

        if(orientation < 1 | orientation > 4) stop("illegal orientation. must be in [1,4]")

        # Prepare colours.
        # By default, alpha levels range from 10% to 100% starting at a 
        # cut-off of 0.1.
        if(is.null(colors)){
            n_colors <- 10
            colors<-c("blue", "red")
            alpha = c(0.1, seq(0.1, 1, length.out = n_colors + 1))
            if(!is.null(opacity)){
                if(!class(opacity) %in% c("logical", "numeric")) stop("unknown class type of opacity")
                if(class(opacity) == "logical" & !opacity) alpha = 1
                if(class(opacity) == "numeric"){
                    if (opacity > 1L | opacity <= 0L) stop('opacity must be in (0, 1]')
                    alpha <- opacity
                }
            }
            raw_cols = colorRampPalette(colors)(n_colors)
            raw_cols_rgb = col2rgb(raw_cols)
            alpha_cols = rgb(
                raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
                alpha = alpha * 255L, names = names(raw_cols),
                maxColorValue = 255L)
        } else {
            if(length(colors) == 0) stop("empty colors provided")
            n_colors <- length(colors)
            alpha = c(0.1, seq(0.1, 1, length.out = n_colors + 1))
            if(!is.null(opacity)){
                if(!class(opacity) %in% c("logical", "numeric")) stop("unknown class type of opacity")
                if(class(opacity) == "logical" & !opacity) alpha = 1
                if(class(opacity) == "numeric"){
                    if (opacity > 1L | opacity <= 0L) stop('opacity must be in (0, 1]')
                    alpha <- opacity
                }
            }
            raw_cols_rgb = col2rgb(colors)
            alpha_cols = rgb(
                raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
                alpha = alpha * 255L, names = names(colors),
                maxColorValue = 255L)
        }

        # Arguments
        args1 <- list(col = alpha_cols, pch = 16, cex = .2, xaxs = "i", yaxs = "i", las = 2, axes = FALSE)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        
        # Allow user to override internal plotting parameters.
        custom_xlab = TRUE
        custom_ylab = TRUE
        custom_xaxt = TRUE
        custom_yaxt = TRUE
        if(is.null(args1$xlab)){ args1$xlab = "";  custom_xlab = FALSE; }
        if(is.null(args1$ylab)){ args1$ylab = "";  custom_ylab = FALSE; }
        if(is.null(args1$xaxt)){ args1$xaxt = "n"; custom_xaxt = FALSE; }
        if(is.null(args1$yaxt)){ args1$yaxt = "n"; custom_yaxt = FALSE; }

        # Infer slice range from xlim parameter if available and from/to is missing.
        # Otherwise assume slice range is infinite.
        if(is.null(from) & !is.null(args1$xlim)) from <- args1$xlim[1]
        else if(is.null(from)) from <- min(x@data@data$posA)
        if(is.null(to) & !is.null(args1$xlim)) to <- args1$xlim[2]
        else if(is.null(to)) to <- max(x@data@data$posA)
        if(is.null(args1$xlim)) args1$xlim <- c(from, to)

        # If from < to we swap the (from, to) variables.
        if(from > to){
            temp_from <- from
            from <- to
            to <- temp_from
        }
        
        # Assumes all the data is from the same chromosome
        rid <- x@data@data$ridA[1]
        b <- x@data@data[x@data@data$posA >= from 
                         & x@data@data$posA <= to 
                         & x@data@data$posB >= from 
                         & x@data@data$posB <= to 
                         & x@data@data$posA < x@data@data$posB, c("posA","posB","R2")]
        if(!is.null(args1$ylim)) b <- b[(b$posB - b$posA) < args1$ylim[2], ] # limit for y-axis
        b <- b[order(b$R2,decreasing = F),] # sort for Z-stack

        if(nrow(b) == 0) stop("no data after filter")

        xd <- b$posA + ((b$posB - b$posA) / 2)
        yd <- b$posB - b$posA
        args1$col <- args1$col[cut(b$R2,breaks=seq(0, 1, length.out = (length(alpha_cols)-1)), include.lowest = T)]
        if(is.null(args1$ylim)) args1$ylim <- c(0, max(b$posB - b$posA))
        
        # If orientation is in state 2 then flip y-axis upside down.
        if(orientation == 2 | orientation == 4){
            print("flipping in orientation")
            print(args1$ylim)
            #if(args1$ylim[2] < args1$ylim[1]){
                #temp_ylim <- args1$ylim
                args1$ylim <- c(args1$ylim[2], args1$ylim[1])
            #}
            print(args1$ylim)
        }

        # If orientation is in state 3 then swap the x,y values and the 
        # x-axis and y-axis limits.
        if(orientation == 3 | orientation == 4){
            temp_ylim <- args1$ylim
            temp_xlim <- args1$xlim
            args1$ylim <- temp_xlim
            args1$xlim <- temp_ylim
        }

        # Setup plot.
        yd_fake <- -1
        xd_fake <- -1
        do.call("plot", c(yd_fake~xd_fake, args1))
        # Add title
        if(annotate){
            if(is.null(args1$main)){
                title(sprintf("Linkage-disequilibrium for rid: %s", rid), adj=0)
                mtext(sprintf("Points in view: %s",nrow(b)),adj=0,cex=1)
            }
        }
        
        # Add background.
        if(!is.null(bg)) rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg)

        args1[["axes"]] <- NULL
        # Add points.
        if(orientation == 1 | orientation == 2){
            do.call("points", c(yd~xd, args1))
            
        } else {
            do.call("points", c(xd~yd, args1))
        }
        
        # Add axis if annotate and not user-specified custom parameters.
        if(annotate){
            if(custom_xaxt == FALSE) rtomahawk:::addGenomicAxis(args1$xlim, 1, 1)
            if(custom_xlab == FALSE) mtext(side = 1, line = 3, "Position (from)")
            if(custom_yaxt == FALSE) rtomahawk:::addGenomicAxis(args1$ylim, 2, 2)
            if(custom_ylab == FALSE) mtext(side = 2, line = 3, "Position (to)")
        }
    }
)

#' Plot LD square
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param x Input Tomahawk class.
#' @param from Start position in base-pairs.
#' @param to End position in base-pairs.
#' @param colors Vector of colors used to plot with. These are different from \code{col}
#'    as these colors are internally parsed to add opacity.
#' @param opacity Logical flag/numerical value controlling opacity levels of colors. If
#'    set to the logical \code{FALSE}, colours will have a uniform opacity of 1. If set
#'    to a numerical value, colours will uniformly share that opacity level. If unset,
#'    opacity will be a gradient from [0.1, 1] starting at R2 > 0.1.
#' @param upper Plot only the upper triangular of values.
#' @param lower Plot only the lower triangular of values.
#' @param really Logical flag to prevent plotting too many points.
#'    Set this to \code{TRUE} at your own peril.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @useDynLib rtomahawk, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom grDevices col2rgb rgb
#' @examples
#' # This example assumes you have a Tomahawk file called "1kgp3_chr6.two" in
#' # your current working directory.
#' f<-setFilters(minR2=0.1)
#' twk<-openTomahawkOutput("1kgp3_chr6.two")
#' y<-readRecords(twk,f,"6:5e6-10e6",really=TRUE)
#' plotLD(y, ylim=c(5e6, 10e6), xlim=c(5e6, 10e6))
#' plotLD(y, ylim=c(5e6, 10e6), xlim=c(5e6, 10e6), upper = TRUE)
#' plotLD(y, ylim=c(5e6, 10e6), xlim=c(5e6, 10e6), lower = TRUE)
setGeneric("plotLD", function(x="twk", from = "ANY", to = "ANY", colors = "ANY", bg = "ANY", opacity = "ANY", upper = "logical", lower = "logical", annotate = "logical", really = "logical", ...){ standardGeneric("plotLD") })
setMethod("plotLD",
    signature(x="twk"),
    definition = function(x, from = NULL, to = NULL, colors = NULL, bg = NULL, opacity = TRUE, upper = FALSE, lower = FALSE, annotate = TRUE, really = FALSE, ...){
        if(nrow(x@data@data) == 0) stop("no data available")
        if(length(table(y@data@data$ridA)) != 1) stop("non-unqique ridA")
        if(length(table(y@data@data$ridB)) != 1) stop("non-unqique ridB")
        if(all(y@data@data$ridA==y@data@data$ridB) == FALSE) stop("all ridA != ridB")
        if(upper & lower){
            upper = FALSE
            lower = FALSE
        }

        # Prepare colours.
        # By default, alpha levels range from 10% to 100% starting at a 
        # cut-off of 0.1.
        if(is.null(colors)){
            n_colors <- 10
            colors<-c("blue", "red")
            alpha = c(0.1, seq(0.1, 1, length.out = n_colors + 1))
            if(!is.null(opacity)){
                if(!class(opacity) %in% c("logical", "numeric")) stop("unknown class type of opacity")
                if(class(opacity) == "logical" & !opacity) alpha = 1
                if(class(opacity) == "numeric"){
                    if (opacity > 1L | opacity <= 0L) stop('opacity must be in (0, 1]')
                    alpha <- opacity
                }
            }
            raw_cols = colorRampPalette(colors)(n_colors)
            raw_cols_rgb = col2rgb(raw_cols)
            alpha_cols = rgb(
                raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
                alpha = alpha * 255L, names = names(raw_cols),
                maxColorValue = 255L)
        } else {
            if(length(colors) == 0) stop("empty colors provided")
            n_colors <- length(colors)
            alpha = c(0.1, seq(0.1, 1, length.out = n_colors + 1))
            if(!is.null(opacity)){
                if(!class(opacity) %in% c("logical", "numeric")) stop("unknown class type of opacity")
                if(class(opacity) == "logical" & !opacity) alpha = 1
                if(class(opacity) == "numeric"){
                    if (opacity > 1L | opacity <= 0L) stop('opacity must be in (0, 1]')
                    alpha <- opacity
                }
            }
            raw_cols_rgb = col2rgb(colors)
            alpha_cols = rgb(
                raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
                alpha = alpha * 255L, names = names(colors),
                maxColorValue = 255L)
        }

        # Arguments
        args1 <- list(col = alpha_cols, pch = 20, cex = .2, xaxs = "i", yaxs = "i", las = 2, xaxt = "n", yaxt = "n", axes = FALSE, ylab = "", xlab = "")
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        # Infer slice range from xlim parameter if available and from/to is missing.
        # Otherwise assume slice range is infinite.
        if(is.null(from) & !is.null(args1$xlim)) from <- args1$xlim[1]
        else if(is.null(from)) from <- min(x@data@data$posA, x@data@data$posB)
        if(is.null(to) & !is.null(args1$xlim)) to <- args1$xlim[2]
        else if(is.null(to)) to <- max(x@data@data$posA, x@data@data$posB)
        if(is.null(args1$xlim)) args1$xlim <- c(from, to)
        if(is.null(args1$ylim)) args1$ylim <- args1$xlim

        # If from < to we swap the variables.
        if(from > to){
            temp_from <- from
            from <- to
            to <- temp_from
        }
        
        # Assumes all the data is from the same chromosome
        rid <- x@data@data$ridA[1]
        b <- x@data@data[x@data@data$posA >= from 
                         & x@data@data$posA <= to 
                         & x@data@data$posB >= from 
                         & x@data@data$posB <= to, c("posA","posB","R2")]
        
        if(nrow(b) == 0) stop("no data after filter")
        b <- b[order(b$R2,decreasing = F),] # sort for Z-stack
        if(upper) b <- b[b$posA < b$posB,]
        if(lower) b <- b[b$posB < b$posA,]
        
        xd <- b$posA
        yd <- b$posB
        args1$col <- args1$col[cut(b$R2,breaks=seq(0, 1, length.out = (length(alpha_cols)-1)), include.lowest = TRUE, right = TRUE)]

        # Setup plot.
        yd_fake <- -1
        xd_fake <- -1
        do.call("plot", c(yd_fake~xd_fake, args1))
        # Add title
        if(annotate){
            title(sprintf("Linkage-disequilibrium for rid: %s", rid), adj=0)
            mtext(sprintf("Points in view: %s",nrow(b)),adj=0,cex=1)
        }

        # Add background.
        if(!is.null(bg)) rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg)
        # Add points.
        args1[["axes"]] <- NULL
        do.call("points", c(yd~xd, args1))

        # Add axis
        # Todo: will produce incorrect order if xaxis is flipped (descending order)
        rtomahawk:::addGenomicAxis(args1$xlim, 1, 1)
        rtomahawk:::addGenomicAxis(args1$ylim, 2, 2)
    }
)

# Decay ---------------------------

setGeneric("decay", function(x="twk", range = "numeric", nbins = "numeric", ...){ standardGeneric("decay") })

setMethod("decay",
    signature(x="twk"),
    definition = function(x, range = 1000000, nbins = 1000, ...){
        args1 <- list(x = x, range = range, nbins = nbins)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(args1$nbins <= 0) return(NULL)
        if(args1$range <= 1) return(NULL)

        return(rtomahawk:::.twk_decay(x, args1$range, args1$nbins));
    }
)

#' Plot Locuszoom-like graph for a location its GWAS P-value and LD association
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param x Integer representing the sorted state of the file.
#' @param interval This parameter can be either a character string 
#'    encoded as "CHR:POS" or a GenomicRanges object overlapping a 
#'    single position.  
#' @param snp A data.frame with the input P-values and their positions
#'    and names. The column names "SNP", "Position", and "p" MUST exist
#'    in this data frame.
#' @param gmap List of recombination hotspots for the species in question.
#'    The list entries MUST be data.frames having the columns "position", 
#'    "COMBINED_rate.cM.Mb.", and "Genetic_Map.cM.". This data is provided
#'    for human build hg19 in rtomahawk. It can be loaded as follows:
#'    data(gmap)
#' @param window Neighbourhood in base-pairs.
#' @param minP Largest P-value to report.
#' @param minR2 Smallest R-squared (R2) value to report.
#' @param threads Number of Tomahawk threads used to unpack and compute the
#'    association data.
#' @param verbose Flag triggering verbose output (written to std::cerr). This
#'    will usually, but not always, be appropriately handled by R.
#' 
#' @return Returns a \code{\link{twk}} object with the loaded data.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @useDynLib rtomahawk, .registration = TRUE 
#' @importFrom Rcpp evalCpp
#' @importFrom GenomicRanges GRanges
#' @examples
#' # This example assumes you have a Tomahawk file called "1kgp3_chr6.twk" in
#' # your current working directory.
#' twk2<-new("twk")
#' twk2@file.path <- "1kgp3_chr6.twk"
#' 
#' # Data from
#' # http://static.geneatlas.roslin.ed.ac.uk/gwas/allWhites/imputed/data.copy/imputed.allWhites.selfReported_n_1245.chr6.csv.gz
#' # http://static.geneatlas.roslin.ed.ac.uk/gwas/allWhites/snps/extended/snps.imputed.chr6.csv.gz
#' # This example assumes that these two files have been downloaded to your
#' # local Downloads directory.
#' library(data.table)
#' x<-fread("zcat ~/Downloads/imputed.allWhites.selfReported_n_1245.chr6.csv.gz", sep=" ")
#' snp<-fread("zcat ~/Downloads/snps.imputed.chr6.csv.gz", sep=" ")
#' snp<-snp[match(x$SNP,snp$SNP),]
#' # Transform raw P-values into negative log10-scaled space.
#' snp$p <- -log10(x$`PV-selfReported_n_1245`)
#'
#' # Load recombination data for human hg19.
#' data(gmap)
#' z <- plotLZ(twk2, "6:20694884", snp, gmap, window = 1000000, minR2 = 0)
#' z
#' # Example using GenomicRanges class.
#' require(GenomicRanges)
#' g <- GRanges("6", IRanges(20694884, 20694884))
#' z <- plotLZ(twk2, g, snp, gmap, window = 1000000, minR2 = 0)
#' # With verbose output.
#' z <- plotLZ(twk2, g, snp, gmap, window = 1000000, minR2 = 0, verbose=TRUE)
setGeneric("plotLZ", 
    function(x="twk", 
             interval="ANY", 
             snp="data.frame", 
             gmap="list", 
             window="numeric", 
             minP="numeric", 
             minR2="numeric", 
             threads="numeric", 
             verbose="numeric", 
             ...)
    { 
        standardGeneric("plotLZ") 
    }
)

setMethod("plotLZ",
    signature(interval="character"),
    definition = function(x, interval, snp, gmap, window = 500000, minP = 1, minR2 = 0.01, threads = 1, verbose = FALSE, ...){
        if(nrow(snp) == 0) stop("no input snp data")
        if(length(gmap) == 0) stop("no input gmap data")
        if(nchar(interval) == 0) stop("no interval provided")

        args1 <- list(progress = FALSE, xaxs = "i", yaxs = "i", las = 2, axes = FALSE, pch = 21)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        # Allow user to override internal plotting parameters.
        custom_xlab = TRUE
        custom_ylab = TRUE
        custom_xaxt = TRUE
        custom_yaxt = TRUE
        if(is.null(args1$xlab)){ args1$xlab = "";  custom_xlab = FALSE; }
        if(is.null(args1$ylab)){ args1$ylab = "";  custom_ylab = FALSE; }
        if(is.null(args1$xaxt)){ args1$xaxt = "n"; custom_xaxt = FALSE; }
        if(is.null(args1$yaxt)){ args1$yaxt = "n"; custom_yaxt = FALSE; }

        # Check the intervals for correct formatting.
        interval_type <- rtomahawk:::.checkInterval(interval)
        if(interval_type == 2){
            # good
            tgt_chr <- as.numeric(strsplit(interval, ":")[[1]][1])
            tgt_snp <- as.numeric(strsplit(interval, ":")[[1]][2])
        } else if(interval_type == 3){
            pos_temp <- strsplit(strsplit(interval,":")[[1]][2],"-")[[1]]
            if(pos_temp[1] != pos_temp[2])
                stop("illegal interval. interval may only encompass a single position")
            tgt_chr <- as.numeric(strsplit(interval, ":")[[1]][1])
            tgt_snp <- as.numeric(pos_temp[1])
        } else {
            stop("illegal interval")
        }
        
        if(grep("^[0-9]$",tgt_chr)) tgt_chr <- paste("chr", tgt_chr, sep = "", collapse = NULL)
        else {
            tgt_chr <- strtolower(tgt_chr)
        }

        if(!tgt_chr %in% names(gmap))
            stop("cannot find target chromosome in gmap")

        if(!tgt_snp%in%snp$Position){
            stop("cannot find target snp in set")
        }

        ld<-rtomahawk:::.twk_scalc(x, interval, window, minP, minR2, threads, verbose, args1$progress)
        args1[["progress"]] <- NULL
    
        from <- tgt_snp - window
        to <- tgt_snp + window
        args1$xlim <- c(from, to)
        pos <- snp$Position[snp$Position>from&snp$Position<to]
        posLD <- pos[pos%in%union(ld@data@data$posA,ld@data@data$posB)]
        pvals <- snp$p[snp$Position>from&snp$Position<to]
        pvalsLD <- pvals[which(pos%in%posLD)]
        
        # Internal copy.
        ldInternal <- ld@data@data[ld@data@data$posB==tgt_snp,]
        rownames(ldInternal) <- ldInternal$posA
        ldInternal <- ldInternal[as.character(posLD),]
        posLD <- posLD[order(ldInternal$R2, decreasing = F)]
        pvalsLD <- pvalsLD[order(ldInternal$R2, decreasing = F)]
        ldInternal <- ldInternal[order(ldInternal$R2, decreasing = F),]
        
        lzcolors<-rev(c("#D43F3AFF", "#EEA236FF", "#5CB85CFF", "#46B8DAFF", "#357EBDFF"))
        
        # Generate background plot.
        args2 <- list(type = "l", col = "blue", ylim = c(0, 100))
        args2 <- c(args2, args1)
        x_gmap <- gmap[[tgt_chr]][gmap[[tgt_chr]]$position>from & gmap[[tgt_chr]]$position<to, 1]
        y_gmap <- gmap[[tgt_chr]][gmap[[tgt_chr]]$position>from & gmap[[tgt_chr]]$position<to, 2]
        #par(mar=c(5,5,2,5))
        do.call("plot", c(y_gmap ~ x_gmap, args2))

        #plot(gmap[[tgt_chr]][gmap[[tgt_chr]]$position>from & gmap[[tgt_chr]]$position<to,c(1,2)], type="l",col="blue",ylim=c(0,100), xlim=c(from, to), args1)
        axis(side=4,las=2,col="blue",col.axis="blue")
        mtext(side = 4, line = 3, "Recombination rate (cm/Mb)",col = "blue")
        par(new = T)
        # Add plot.
        args2 <- list(col="#B8B8B8FF", xaxs="i", yaxs="i", las=2, ylab="-log10(P)", xlab=NA, xaxt = "n", ylim=c(0, round(max(pvals)+5,-1)))
        args2 <- c(args2, args1[!names(args1)%in%names(args2)]) # do not include labels defined specifically for args2
        args2[["yaxt"]] <- NULL
        args2[["axes"]] <- NULL
        if(is.null(args2$pch)) args2$pch <- 21
        x_pval <- pos[which(!pos%in%posLD)]
        y_pval <- pvals[which(!pos%in%posLD)]
        do.call("plot", c(y_pval ~ x_pval, args2))
        #plot(pos[which(!pos%in%posLD)], pvals[which(!pos%in%posLD)])
        points(posLD, pvalsLD, pch = args2$pch, cex = 1, bg = lzcolors[as.numeric(cut(ldInternal$R2, breaks = seq(0,1,length.out = 6), right = TRUE))])
        points(tgt_snp, snp[snp$Position==tgt_snp,"p"], pch=24, bg="#9632B8FF",cex=1.2)
        suppressWarnings(rug(pos, side=3, ticksize = -0.03))
        legend("topright", 
               fill = c(rev(lzcolors), "#B8B8B8FF"), 
               legend = sprintf("%s:%s", 
                                c("0.8-1.0","0.6-0.8","0.4-0.6","0.2-0.4","0.0-0.2","NA"),
                                c(nrow(ld@data@data[ld@data@data$R2>0.8 & ld@data@data$R2<=1.0,]),
                                  nrow(ld@data@data[ld@data@data$R2>0.6 & ld@data@data$R2<=0.8,]),
                                  nrow(ld@data@data[ld@data@data$R2>0.4 & ld@data@data$R2<=0.6,]),
                                  nrow(ld@data@data[ld@data@data$R2>0.2 & ld@data@data$R2<=0.4,]),
                                  nrow(ld@data@data[ld@data@data$R2>0 & ld@data@data$R2<=0.2,]),
                                  0)), 
               y.intersp = 0.5, 
               cex = 1.2,
               bty = "n", 
               title=expression("LD "  ~ R^2))
        text(x= from + ((to - from) / 2), y=round(max(pvals) + 5, -1) - 3, labels = tgt_snp)
        # Add x-axis.
        if(custom_xaxt == FALSE) rtomahawk:::addGenomicAxis(args1$xlim, 1, 1)
        if(custom_xlab == FALSE) mtext(side = 1, line = 3, "Position")

        return(ld)
    }
)

setMethod("plotLZ",
    signature(interval="GRanges"),
    definition = function(x, interval, snp, gmap, window = 500000, minP = 1, minR2 = 0.01, threads = 1, verbose = FALSE, ...){
        if(nrow(snp) == 0) stop("no input snp data")
        if(length(gmap) == 0) stop("no input gmap data")

        args1 <- list(x = x, interval = interval, window = window, snp = snp, gmap = gmap, minP = minP, minR2 = minR2, threads = threads, verbose = verbose, progress = FALSE)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        if(length(interval) != 1)
            stop("Number of intervals is not 1!")
        
        if(interval@ranges@width != 1)
            stop("Interval length is not 1!")

        tgt_snp <- interval@ranges@start
        tgt_chr <- as.character(interval@seqnames@values)
        if(grep("^[0-9]$",tgt_chr)) tgt_chr <- paste("chr", tgt_chr, sep = "", collapse = NULL)
        else {
            tgt_chr <- strtolower(tgt_chr)
        }

        if(!tgt_snp%in%snp$Position){
            stop("cannot find target snp in set")
        }

        interval_string <- sprintf("%s:%s",interval@seqnames@values,interval@ranges@start)
        ld<-rtomahawk:::.twk_scalc(x,interval_string,threads=threads,window=window,minR2=minR2,verbose=args1$verbose)

        from<-tgt_snp - window
        to<-tgt_snp + window
        pos<-snp$Position[snp$Position>from&snp$Position<to]
        posLD<-pos[pos%in%union(ld@data@data$posA,ld@data@data$posB)]
        pvals<-snp$p[snp$Position>from&snp$Position<to]
        pvalsLD<-pvals[which(pos%in%posLD)]
        
        # Internal copy.
        ldInternal<-ld@data@data[ld@data@data$posB==tgt_snp,]
        rownames(ldInternal) <- ldInternal$posA
        ldInternal <- ldInternal[as.character(posLD),]
        posLD <- posLD[order(ldInternal$R2,decreasing = F)]
        pvalsLD <- pvalsLD[order(ldInternal$R2,decreasing = F)]
        ldInternal <- ldInternal[order(ldInternal$R2,decreasing = F),]
        
        lzcolors<-rev(c("#D43F3AFF", "#EEA236FF", "#5CB85CFF", "#46B8DAFF", "#357EBDFF"))
        
        #par(mar=c(5,5,2,5))
        plot(gmap[[tgt_chr]][gmap[[tgt_chr]]$position>from&gmap[[tgt_chr]]$position<to,c(1,2)],type="l",col="blue",ylim=c(0,100),xaxs="i",yaxs="i",axes=F, xlab=NA, ylab=NA)
        axis(side=4,las=2,col="blue",col.axis="blue")
        mtext(side = 4, line = 3, "Recombination rate (cm/Mb)",col = "blue")
        par(new = T)
        plot(pos[which(!pos%in%posLD)],pvals[which(!pos%in%posLD)],pch=20,cex=.9,col="#B8B8B8FF",xaxs="i",yaxs="i",las=2,ylab="-log10(P)",xlab="Position",ylim=c(0, round(max(pvals)+5,-1)))
        points(posLD,pvalsLD,pch=21,cex=1,bg=lzcolors[as.numeric(cut(ldInternal$R2,breaks = seq(0,1,length.out = 6),right = T))])
        points(tgt_snp, snp[snp$Position==tgt_snp,"p"], pch=24, bg="#9632B8FF",cex=1.2)
        suppressWarnings(rug(pos,side=3,ticksize = -0.03))
        legend("topright",fill = c(rev(lzcolors),"#B8B8B8FF"), legend = c("0.8-1.0","0.6-0.8","0.4-0.6","0.2-0.4","0.0-0.2","NA"),y.intersp = 0.5,cex = 1.3, title=expression("LD "  ~ R^2))
        text(x=from+((to-from)/2),y=round(max(pvals)+5,-1)-3,labels = tgt_snp)

        return(ld)
    }
)

#' Tomahawk output aggregation class
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @slot twk Instance of a \code{\link{twk}} object. This is the 
#'    computed information that is returned from the underlying 
#'    \code{.Call} to Tomahawk. Note that no data is stored in
#'    this object in this case.
#' @slot n Total number of data points in the reduced output matrix.
#' @slot x Number of bins in the X dimension.
#' @slot y Number of bins in the Y dimension.
#' @slot bpx Base-pairs encompassed in each x-bin.
#' @slot bpy Base-pairs encompassed in each y-bin.
#' @slot aggregation The function string name used for aggregation.
#' @slot reduction The function string name used for reduction.
#' @slot n_original Total number of original data points prior to
#'    aggregation.
#' @slot range Total number of base-pairs encompassed by the aggregator.
#' @slot offsets Data frame representing the cumulative number of base-pairs
#'    used by each chromosome in the file.
#' @slot data Output aggregated matrix of size [x, y] such that x*y = n.
#' 
#' @seealso \code{\link{twk_index}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
twk_agg <- setClass("twk_agg",
    slots = c(twk = "twk",
              n = "numeric",
              x = "numeric",
              y = "numeric",
              bpx = "numeric",
              bpy = "numeric",
              aggregation = "character",
              reduction = "character",
              n_original = "numeric",
              range = "numeric",
              offsets = "data.frame",
              data = "matrix"))

# Show method
setMethod("show",
    signature = "twk_agg",
    definition = function(object){
        cat("An object of class ", class(object), "\n", sep = "")
        cat("Dimensions [", object@x, ",", object@y, "] reduced from ", object@n_original, " records", "\n", sep = "")
        cat("Aggregation: ", object@aggregation, " and reduction: ", object@reduction, "\n", sep = "")
        invisible(NULL)
    }
)

#' Aggregate billions of data points into a reduced-representation matrix
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param x Input \code{\link{twk}} class holding the target path.
#' @param aggregation String representing the aggregation function to use
#'    on the input data.
#' @param reduction String representing the reduction function to use after
#'    after data has been aggregated.
#' @param xbins Number of bins (pixels) in the x-dimension.
#' @param ybins Number of bins (pixels) in the y-dimension.
#' @param minCount Only pixels with at least this many observations are reported.
#' @param threads Number of aggregation/reduction threads to use.
#' @param verbose Flag triggering verbose output (written to std::cerr). This
#'    will usually, but not always, be appropriately handled by R.
#' @param progress This flag will trigger a detached thread internal to Tomahawk
#'    that will tick progression every 30 seconds to the console. Note that R
#'    NEVER cleans up this thread and it will tick until the R instance is closed.
#' 
#' @return Returns a \code{\link{twk_agg}} object with the aggregated matrix.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @useDynLib rtomahawk, .registration = TRUE 
#' @importFrom Rcpp evalCpp
#' @examples
#' # Assuming you have a file called "test.two" in your downloads directory.
#' twk<-openTomahawkOutput("~/Downloads/test.two")
#' agg <- aggregateOutput(twk, "r2" ,"count", 1000, 1000, 50, verbose = T, threads = 4)
#' plot(agg, normalize = TRUE)
#' plot(agg, normalize = FALSE)
#' agg
setGeneric("aggregate", 
    function(x="twk", 
             aggregation = "character", 
             reduction = "character", 
             xbins = "numeric", 
             ybins = "numeric", 
             minCount = "numeric", 
             threads = "numeric", 
             verbose = "logical", 
             progress = "logical", 
             ...)
    { 
        standardGeneric("aggregate") 
    }
)

setMethod("aggregate",
    signature(x="twk"),
    definition = function(x, aggregation = "r2", reduction = "count", xbins = 1000, ybins = 1000, minCount = 0, threads = 1, verbose = FALSE, progress = FALSE, ...){
        args1 <- list(x = x, 
                      aggregation = aggregation, 
                      reduction = reduction, 
                      xbins = xbins, 
                      ybins = ybins, 
                      minCount = minCount, 
                      n_threads = threads, 
                      verbose = verbose, 
                      progress = progress)

        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(args1$xbins < 1) stop("xbins < 1")
        if(args1$ybins < 1) stop("ybins < 1")
        if(args1$minCount < 0) stop("mincount < 0")
        if(args1$n_threads <= 0) stop("n_threads <= 0")

        return(rtomahawk:::.twk_aggregate(x, 
                                          args1$aggregation, 
                                          args1$reduction, 
                                          args1$xbins, 
                                          args1$ybins, 
                                          args1$minCount, 
                                          args1$n_threads, 
                                          args1$verbose, 
                                          args1$progress));
    }
)

#' Load aggregation object generated by tomahawk
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param input Input target file path.
#' 
#' @return Returns a \code{\link{twk_agg}}.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @useDynLib rtomahawk, .registration = TRUE 
#' @importFrom Rcpp evalCpp
#' @examples
#' # Assuming you have a file called "agg.twa" in your downloads directory.
#' agg<-loadAggregate("~/Downloads/agg.twa")
#' plot(agg, normalize = TRUE)
#' plot(agg, normalize = FALSE)
#' agg
setGeneric("loadAggregate", 
    function(input = "character", 
             ...)
    { 
        standardGeneric("loadAggregate") 
    }
)
 
setMethod("loadAggregate",
    signature(input="character"),
    definition = function(input){
        if(nchar(input) == 0) stop("no input file path was given")

        return(rtomahawk:::.twk_read_aggregate(input));
    }
)

#' General plotting method for Tomahawk aggregation.
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param x Instance of a \code{\link{twk}} object. This is the 
#'    computed information that is returned from the underlying 
#'    \code{.Call} to Tomahawk. Note that no data is stored in
#'    this object in this case.
#' @param normalize Logical flag indicating if the colour scheme
#'    should be quantile normalized to even out the ranges.
#' @param annote Logical flag indicating if titles and axes should
#'    should be added.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @examples
#' # This example assumes you have a Tomahawk file called "1kgp3_chr6.two" in
#' # your current working directory.
#' twk<-rtomahawk::OpenTomahawkOutput("1kgp3_chr6.two")
#' x<-aggregate(twk,"r2","count",1000,1000,50,verbose=T,threads=4)
#' plot(x,normalized=TRUE)
#' plot(x,normalized=FALSE)
#' x
setGeneric("plotAggregation", function(x="twk_agg", colors = "character", normalize="logical", annotate = "logical", legend = "logical", ...) standardGeneric("plotAggregation"))
setMethod("plotAggregation",
    signature(x="twk_agg"),
    definition = function(x, colors = viridis(11), normalize = TRUE, annotate = TRUE, legend = TRUE, ...){
        if(length(x@data) == 0)
            stop("no data available")

        args1 <- list(colors = colors)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        
        # Normalize
        dist <- table(x@data[x@data > 0])
        cumdist <- cumsum(dist) / sum(dist)
        col_breaks <- rep(0, length(args1$colors) - 1)
        for(j in 1:(length(args1$colors) - 1)){
            # Compute 10-percentile bins using the nearest-rank method
            col_breaks[j] = as.numeric(names(cumdist)[which.max(cumdist >= (min(cumdist) + ((max(cumdist) - min(cumdist)) / (length(args1$colors)) * j)))])
        }
        
        if(normalize){
            image(x@data, useRaster = TRUE, axes=FALSE, xaxt='n', yaxt='n', ann=FALSE, bty="n", col=args1$colors, breaks=c(0, col_breaks, max(x@data)))
            for(i in 1:5){
                legend_labels <- round(col_breaks,i)
                if(any(duplicated(legend_labels)) == FALSE) break
            }
            if(legend) legend("topleft", legend = legend_labels, fill = args1$colors, inset=c(1,0), xpd=TRUE, bty="n", y.intersp=0.7, title = expression(bold("Color key")))
        }
        else {
            image(x@data, useRaster = TRUE, axes=FALSE, xaxt='n', yaxt='n', ann=FALSE, bty="n", col=args1$colors)
            for(i in 1:5){
                legend_labels <- round(max(x@data)/(length(args1$colors):1),i)
                if(any(duplicated(legend_labels)) == FALSE) break
            }
            if(legend) legend("topleft", legend = legend_labels, fill = args1$colors, inset=c(1,0), xpd=TRUE, bty="n", y.intersp=0.7, title = expression(bold("Color key")))
        }

        valid_ranges <- x@offsets[x@offsets$max!=4294967295,]
        if(nrow(valid_ranges) == 1){
            from <- valid_ranges$min
            to <- valid_ranges$max
        }

        # Todo: fix case when passing axes=FALSE or xaxt="n" or yaxt="n"
        if(annotate){
            if(!is.null(from)){
                rtomahawk:::addGenomicAxis(c(from, to), 1, 1, scaleMax = TRUE)
                mtext(side = 1, line = 3, "Position (from)")
            }
            if(!is.null(to)){
                rtomahawk:::addGenomicAxis(c(from, to), 2, 2, scaleMax = TRUE)
                mtext(side = 2, line = 3, "Position (to)")
            }

            if(is.null(args1$main)){
                title("Aggregated linkage-disequilibrium", adj=0)
                mtext(sprintf("Points in view: %s", x@n_original),adj=0,cex=1)
            }
        }
    }
)

#' Calculate pairwise LD for a target SNV to its neighbourhood
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @author Marcus D. R. Klarqvist <\email{mk819@@cam.ac.uk}> |
#' <\href{https://mdrk.me}{https://mdrk.me}>
#'
#' @param x Integer representing the sorted state of the file.
#' @param interval This parameter can be either a character string 
#'    encoded as "CHR:POS" or a GenomicRanges object overlapping a 
#'    single position.  
#' @param window Neighbourhood in base-pairs.
#' @param minP Largest P-value to report.
#' @param minR2 Smallest R-squared (R2) value to report.
#' @param threads Number of Tomahawk threads used to unpack and compute the
#'    association data.
#' @param verbose Flag triggering verbose output (written to std::cerr). This
#'    will usually, but not always, be appropriately handled by R.
#' @param progress This flag will trigger a detached thread internal to Tomahawk
#'    that will tick progression every 30 seconds to the console. Note that R
#'    NEVER cleans up this thread and it will tick until the R instance is closed.
#' 
#' @return Returns a \code{\link{twk}} object with the loaded data.
#'
#' @seealso \code{\link{twk_data}}, \code{\link{twk_header}}, 
#' \code{\link{twk_filter}}, and \code{\link{twk}}
#' 
#' @export
#' @useDynLib rtomahawk, .registration = TRUE 
#' @importFrom Rcpp evalCpp
#' @importFrom GenomicRanges GRanges
#' @examples
#' # This example assumes you have a Tomahawk file called "1kgp3_chr6.twk" in
#' # your current working directory.
#' twk2<-new("twk")
#' twk2@file.path <- "1kgp3_chr6.twk"
#' y<-calculateLDSingle(twk2, "6:20682622", threads = 4, window = 100000)
#' # Example using GenomicRanges class.
#' require(GenomicRanges)
#' g <- GRanges("6", IRanges(20682622, 20682622))
#' y<-calculateLDSingle(twk2, g, threads = 4, window = 100000)
setGeneric("calculateLDSingle", 
    function(x="twk", 
             interval = "ANY", 
             window = "numeric", 
             minP = "numeric", 
             minR2 = "numeric", 
             threads = "numeric", 
             verbose = "logical", 
             progress = "logical", ...)
    { 
        standardGeneric("calculateLDSingle") 
    }
)

setMethod("calculateLDSingle",
    signature(interval="character"),
    definition = function(x, interval, window = 500000, minP = 1, minR2 = 0, threads = 1, verbose = FALSE, progress = FALSE, ...){
        args1 <- list(x = x, 
                      interval = interval, 
                      window = window, 
                      minP = minP, 
                      minR2 = minR2, 
                      threads = threads, 
                      verbose = verbose, 
                      progress = progress)

        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(nchar(args1$interval) == 0) stop("no interval provided")
        if(args1$window < 1) stop("window < 1")
        if(args1$minP < 0 | args1$minP > 1) stop("minP < 0 or minP > 1")
        if(args1$minR2 < 0 | args1$minR2 > 1) stop("minR2 < 0 or minR2 > 1")
        if(args1$threads <= 0) stop("threads <= 0")

        # Check the intervals for correct formatting.
        interval_type <- rtomahawk:::.checkInterval(args1$interval)
        if(interval_type == 2){
            # good
        } else if(interval_type == 3){
            pos_temp <- strsplit(strsplit(args1$interval,":")[[1]][2],"-")[[1]]
            if(pos_temp[1] != pos_temp[2])
                stop("illegal interval. interval may only encompass a single position")
        } else {
            stop("illegal interval")
        }

        return(rtomahawk:::.twk_scalc(x, 
                                      args1$interval, 
                                      args1$window, 
                                      args1$minP, 
                                      args1$minR2,  
                                      args1$threads, 
                                      args1$verbose, 
                                      args1$progress));
    }
)

setMethod("calculateLDSingle",
    signature(interval="GRanges"),
    definition = function(x, interval, window = 500000, minP = 1, minR2 = 0, n_threads = 1, verbose = FALSE, progress = FALSE, ...){
        if(length(interval) != 1)
            stop("Number of intervals is not 1!")
        
        if(interval@ranges@width != 1)
            stop("Interval length is not 1!")

        interval_string <- sprintf("%s:%s", interval@seqnames@values, interval@ranges@start)

        args1 <- list(x = x, 
                      interval = interval_string, 
                      window = window, 
                      minP = minP, 
                      minR2 = minR2, 
                      n_threads = n_threads, 
                      verbose = verbose, 
                      progress = progress)

        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(length(args1$interval) == 0) stop("no interval provided")
        if(args1$window < 1) stop("window < 1")
        if(args1$minP < 0 | args1$minP > 1) stop("minP < 0 or minP > 1")
        if(args1$minR2 < 0 | args1$minR2 > 1) stop("minR2 < 0 or minR2 > 1")
        if(args1$n_threads <= 0) stop("n_threads <= 0")

        return(rtomahawk:::.twk_scalc(x, 
                                      args1$interval, 
                                      args1$window, 
                                      args1$minP, 
                                      args1$minR2,  
                                      args1$n_threads, 
                                      args1$verbose, 
                                      args1$progress));
    }
)
