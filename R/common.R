# Todo:
# view(twk, filters, really = FALSE)
# plot(twk, interval, really = FALSE)
suppressPackageStartupMessages({
    library(GenomicRanges)
})

#' Print Tomahawk version string
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.

#' @return Prints the used version of rtomahawk and the C/C++ 
#'    shared object versions used by Tomahawk.
#' 
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
#' @slot flagInclude Text
#' @slot flagExclude Text
#' @slot D Text
#' @slot Dprime Text
#' @slot R Text
#' @slot R2 Text
#' @slot P Text
#' @slot ChiSqFisher Text
#' @slot ChiSqModel Text
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
              upperOnly = "boolean",
              lowerOnly = "boolean"))


setGeneric("setFilters", 
    function(x="twk_filter", 
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
             upperOnly = "boolean",
             lowerOnly = "boolean",
             ...)
    { 
        standardGeneric("setFilters")
    }
)
# Todo: write actual set filters class

#' Tomahawk output class
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
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
        if(args1$n <= 0) return(NULL)
        return(rtomahawk:::.twk_tail(x, args1$n));
    }
)

# Todo
setMethod("[",
    signature(x="twk"),
    definition = function(x, i = NULL, j = NULL, ..., drop = FALSE){
        #args1 <- list(x = x, i = i, j = j, drop = drop)
        #inargs <- list(...)
        #args1[names(inargs)] <- inargs
        return(i)
    }
)

setMethod("[<-",
    signature(x="twk"),
    definition = function(x, i = NULL, j = NULL, ..., value){
        #args1 <- list(x = x, i = i, j = j, drop = drop)
        #inargs <- list(...)
        #args1[names(inargs)] <- inargs
        return(TRUE)
    }
)

# Basic ---------------------------

setGeneric("OpenTomahawkOutput", function(input="character", ...){ standardGeneric("OpenTomahawkOutput") })
setMethod("OpenTomahawkOutput",
    signature(input="character"),
    definition = function(input, ...){
        if(nchar(input) == 0) stop("no input provided")
        
        args1 <- list(input = input)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        return(rtomahawk:::.OpenTomahawkOutput(input));
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
    signature(interval="GenomicRanges"),
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
#' @examples
#' # Assuming you have a file called "test.two" in your downloads directory.
#' twk<-OpenTomahawkOutput("~/Downloads/test.two")
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
             verbose = "boolean", 
             progress = "boolean", ...)
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

# General plotting method for aggregation.
setGeneric("plot", function(x="twk_agg", y="ANY", ...) standardGeneric("plot"))
setMethod("plot",
    signature(x="twk_agg", y="ANY"),
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
             verbose = "boolean", 
             progress = "boolean", ...)
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
    signature(interval="GenomicRanges"),
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
