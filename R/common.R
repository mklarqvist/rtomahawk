library(GenomicRanges)

#' Print Tomahawk version string
#'
#' Some additional details about this S4 generic and its methods.
#' The extra blank line between this section and the title is
#' critical for roxygen2 to differentiate the title from the
#' description section.
#'
#' @param x Description of \code{x}. The main argument in this
#'  example. Most often has such and such properties.
#'
#' @param y Description of \code{y}. An argument that is rarely
#'  used by \code{"helloworld"} methods. 
#'
#' @param ... Additional argument list that might not ever
#'  be used.
#'
#' @return Prints the C/C++ shared object versions used by Tomahawk.
#' 
#' @export
#' @examples
#' tomahawkVersion()
setGeneric("tomahawkVersion", function(x="ANY",...) standardGeneric("tomahawkVersion"))
setMethod("tomahawkVersion",
    signature(x="ANY"),
    definition = function(x,...){
        return(rtomahawk:::.twk_version())
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
#' @slot flag Text
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
    slots = c(flag = "vector",
              D = "vector",
              Dprime = "vector",
              R = "vector",
              R2 = "vector",
              P = "vector",
              ChiSqFisher = "vector",
              ChiSqModel = "vector"))

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
        cat("Number of blocks ", nrow(object@records),"\nrecords ", sum(as.numeric(object@records$n)), " \nUncompressed ", sum(as.numeric(object@records$b_unc)), "\nCompressed ", sum(as.numeric(object@records$b_cmp)), "\n", sep = "")
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

setMethod("head",
    signature(x="twk"),
    definition = function(x, n = 50, ...){
        args1 <- list(n = n)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(args1$n <= 0) return(NULL)
        return(twk_head(x, args1$n));
    }
)

setMethod("tail",
    signature(x="twk"),
    definition = function(x, n = 50, ...){
        args1 <- list(n = n)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(args1$n <= 0) return(NULL)
        return(twk_tail(x, args1$n));
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

#' Plotting and computing target LD
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
#' data(gmap)
#' twk2<-new("twk")
#' twk2@file.path <- "1kgp3_chr6.twk"
#' plotLZ(twk2, "6:20694884", snp, gmap, window=1000000, minR2=0)
setGeneric("plotLZ", function(x="twk", interval="ANY", snp="data.frame", gmap="list", window="numeric", minP="numeric", minR2="numeric", threads="numeric", verbose="numeric", ...){ standardGeneric("plotLZ") })

setMethod("plotLZ",
    signature(interval="character"),
    definition = function(x, interval, snp, gmap, window = 500000, minP = 1, minR2 = 0.01, threads = 1, verbose = FALSE, ...){
        if(nrow(snp) == 0) stop("no input snp data")
        if(length(gmap) == 0) stop("no input gmap data")

        args1 <- list(x = x, interval = interval, window = window, snp = snp, gmap = gmap, minP = minP, minR2 = minR2, threads = threads, verbose = verbose, progress = FALSE)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        tgt_snp <- strsplit(args1$interval, ":")[[1]]
        if( length(tgt_snp) != 2){
            stop("illegal interval formatting")
        }
        tgt_chr <- tgt_snp[1]
        if(grep("^[0-9]$",tgt_chr)) tgt_chr <- paste("chr", tgt_chr, sep = "", collapse = NULL)
        else {
            tgt_chr <- strtolower(tgt_chr)
        }

        if(!tgt_chr %in% names(gmap))
            stop("cannot find target chromosome in gmap")

        tgt_snp <- as.numeric(tgt_snp[2])

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

## Aggregation
# Aggregation object.
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
              data = "matrix",
              quantiles = "numeric"))

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

setGeneric("plot", function(x="twk_agg", y="ANY", ...) standardGeneric("plot"))

# General plotting method for aggregation.
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

# single LD
setGeneric("calculateLDSingle", 
    function(x="twk", 
             interval = "ANY", 
             window = "numeric", 
             minP = "numeric", 
             minR2 = "numeric", 
             n_threads = "numeric", 
             verbose = "boolean", 
             progress = "boolean", ...)
    { 
        standardGeneric("calculateLDSingle") 
    }
)

setMethod("calculateLDSingle",
    signature(interval="character"),
    definition = function(x, interval, window = 500000, minP = 1, minR2 = 0, n_threads = 1, verbose = FALSE, progress = FALSE, ...){
        args1 <- list(x = x, 
                      interval = interval, 
                      window = window, 
                      minP = minP, 
                      minR2 = minR2, 
                      n_threads = n_threads, 
                      verbose = verbose, 
                      progress = progress)

        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(nchar(args1$interval) == 0) stop("no interval provided")
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

setMethod("calculateLDSingle",
    signature(interval="GenomicRanges"),
    definition = function(x, interval, window = 500000, minP = 1, minR2 = 0, n_threads = 1, verbose = FALSE, progress = FALSE, ...){
        if(length(interval) != 1)
            stop("Number of intervals is not 1!")
        
        if(interval@ranges@width != 1)
            stop("Interval length is not 1!")

        interval_string <- sprintf("%s:%s",interval@seqnames@values,interval@ranges@start)

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
