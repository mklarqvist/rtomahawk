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
        args1 <- list(col = alpha_cols, xlab = "Position", ylab = "Position", pch = 16, cex = .2, xaxs = "i", yaxs = "i", las = 2)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        # Infer slice range from xlim parameter if available and from/to is missing.
        # Otherwise assume slice range is infinite.
        if(is.null(from) & !is.null(args1$xlim)) from <- args1$xlim[1]
        else if(is.null(from)) from <- min(x@data@data$posA)
        if(is.null(to) & !is.null(args1$xlim)) to <- args1$xlim[2]
        else if(is.null(to)) to <- max(x@data@data$posA)
        if(is.null(args1$xlim)) args1$xlim <- c(from, to)

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
                         & x@data@data$posB <= to 
                         & x@data@data$posA < x@data@data$posB, c("posA","posB","R2")]
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
            title(sprintf("Linkage-disequilibrium for rid: %s", rid), adj=0)
            mtext(sprintf("Points in view: %s",nrow(b)),adj=0,cex=1)
        }
        
        # Add background.
        if(!is.null(bg)) rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = bg)

        # Add points.
        if(orientation == 1 | orientation == 2){
            do.call("points", c(yd~xd, args1))
        } else {
            do.call("points", c(xd~yd, args1))
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
        args1 <- list(col = alpha_cols, xlab = "Position", ylab = "Position", pch = 16, cex = .2, xaxs = "i", yaxs = "i", las = 2)
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
        b <- b[order(b$R2,decreasing = F),] # sort for Z-stack

        if(upper) b <- b[b$posA < b$posB,]
        if(lower) b <- b[b$posB < b$posA,]

        if(nrow(b) == 0) stop("no data after filter")
        xd <- b$posA
        yd <- b$posB
        args1$col <- args1$col[cut(b$R2,breaks=seq(0, 1, length.out = (length(alpha_cols)-1)), include.lowest = T)]

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
        do.call("points", c(yd~xd, args1))
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
        if(nchar(args1$interval) == 0) stop("no interval provided")

        args1 <- list(x = x, interval = interval, window = window, snp = snp, gmap = gmap, minP = minP, minR2 = minR2, threads = threads, verbose = verbose, progress = FALSE)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        # Check the intervals for correct formatting.
        interval_type <- rtomahawk:::.checkInterval(args1$interval)
        if(interval_type == 2){
            # good
            tgt_chr <- as.numeric(strsplit(args1$interval, ":")[[1]][1])
            tgt_snp <- as.numeric(strsplit(args1$interval, ":")[[1]][2])
        } else if(interval_type == 3){
            pos_temp <- strsplit(strsplit(args1$interval,":")[[1]][2],"-")[[1]]
            if(pos_temp[1] != pos_temp[2])
                stop("illegal interval. interval may only encompass a single position")
            tgt_chr <- as.numeric(strsplit(args1$interval, ":")[[1]][1])
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

        ld<-rtomahawk:::.twk_scalc(x, args1$interval, args1$window, args1$minP, args1$minR2, args1$threads, args1$verbose, args1$progress)
    
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
        
        par(mar=c(5,5,2,5))
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
        
        par(mar=c(5,5,2,5))
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
#' plot(agg, normalized = TRUE)
#' plot(agg, normalized = FALSE)
#' agg
setGeneric("aggregate", 
    function(x="twk", 
             aggregation = "character", 
             reduction = "character", 
             xbins = "numeric", 
             ybins = "numeric", 
             minCount = "numeric", 
             n_threads = "numeric", 
             verbose = "logical", 
             progress = "logical", ...)
    { 
        standardGeneric("aggregate") 
    }
)

setMethod("aggregate",
    signature(x="twk"),
    definition = function(x, aggregation = "r2", reduction = "count", xbins = 1000, ybins = 1000, minCount = 0, n_threads = 1, verbose = FALSE, progress = FALSE, ...){
        args1 <- list(x = x, 
                      aggregation = aggregation, 
                      reduction = reduction, 
                      xbins = xbins, 
                      ybins = ybins, 
                      minCount = minCount, 
                      n_threads = n_threads, 
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
setGeneric("plotAggregation", function(x="twk_agg", normalize="logical", ...) standardGeneric("plotAggregation"))
setMethod("plotAggregation",
    signature(x="twk_agg"),
    definition = function(x, normalize = TRUE, ...){
        if(length(x@data) == 0)
            stop("no data available")

        curpar <- par()
        
        # Viridis colors.
        colors <- c("#440154FF","#482576FF","#414487FF",
                    "#35608DFF","#2A788EFF","#21908CFF",
                    "#22A884FF","#43BF71FF","#7AD151FF",
                    "#BBDF27FF","#FDE725FF")

        args1 <- list(colors = colors, mar = c(0,0,0,0), color_range = 11)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        
        # Normalize
        dist <- table(x@data[x@data > 0])
        cumdist <- cumsum(dist) / sum(dist)
        col_breaks <- rep(0, args1$color_range - 1)
        for(j in 1:(args1$color_range - 1)){
            # Compute 10-percentile bins using the nearest-rank method
            col_breaks[j] = as.numeric(names(cumdist)[which.max(cumdist >= (min(cumdist) + ((max(cumdist) - min(cumdist)) / (args1$color_range) * j)))])
        }
        
        par(mar = args1$mar)
        if(normalize)
            image(x@data, useRaster = T, axes=F, xaxt='n', yaxt='n', ann=FALSE, bty="n", col=args1$colors, breaks=c(0, col_breaks, max(x@data)))
        else
            image(x@data, useRaster = T, axes=F, xaxt='n', yaxt='n', ann=FALSE, bty="n", col=args1$colors)

        suppressWarnings(par(curpar))
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
