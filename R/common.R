# TWO data
twk_data <- setClass("twk_data",
    slots = c(data = "data.frame",
              offset = "vector"))

# Use as iterator-like object
# Last index record
# Last offset into that record

# Index
twk_index <- setClass("twk_index",
    slots = c(state = "numeric",
              records = "data.frame",
              meta = "data.frame"))

# Header
# Header.Contigs
# Header.Samples
twk_header <- setClass("twk_header",
    slots = c(contigs = "data.frame", 
              samples = "vector",
              literals = "vector"))

# Filter object
twk_filter <- setClass("twk_filter",
    slots = c(flag = "vector",
              D = "vector",
              Dprime = "vector",
              R = "vector",
              R2 = "vector",
              P = "vector",
              ChiSqFisher = "vector",
              ChiSqModel = "vector"))

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

# setMethod("show",
#     signature = "MArray",
#     definition = function(object){
#         cat("An object of class ", class(object), "\n", sep = "")
#         cat(" ", nrow(object@marray), " features by ", ncol(object@marray), " samples.\n", sep = "")
#         invisible(NULL)
#     }
# )
# 
# # Plot methods
# if (!isGeneric("plot")) {
#     setGeneric("plot", function(x="MArray", y="ANY", ...){ standardGeneric("plot") })
# }
# 
# setMethod("plot",
#    signature(x="MArray", y="ANY"),
#     definition = function(x, y){
#        plot(x@marray)
#     }
# )
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

if (!isGeneric("decay")) {
    setGeneric("decay", function(x="twk", range = "numeric", nbins = "numeric", ...){ standardGeneric("decay") })
}

setMethod("decay",
    signature(x="twk"),
    definition = function(x, range = 1000000, nbins = 1000, ...){
        args1 <- list(x = x, range = range, nbins = nbins)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        if(args1$nbins <= 0) return(NULL)
        return(twk_decay(x, args1$range, args1$nbins));
    }
)

## Plotting and computing target LD
setGeneric("plotLZ", function(x="twk", interval="ANY", snp="data.frame", gmap="list", window="numeric", minR2="numeric", threads="numeric", ...){ standardGeneric("plotLZ") })
setMethod("plotLZ",
    signature(interval="character"),
    definition = function(x, interval, snp, gmap, window = 500000, minR2 = 0.01, threads = 1, ...){
        args1 <- list(x = x, interval = interval, window = window, snp = snp, gmap = gmap, minR2 = minR2, threads = threads)
        inargs <- list(...)
        args1[names(inargs)] <- inargs
        
        ld<-rtomahawk::twk_scalc(x,interval,threads=threads,window=window,minR2=minR2)
    
        tgt_snp <- strsplit(interval, ":")[[1]]
        if( length(tgt_snp) != 2){
            stop("illegal interval formatting")
        }
        tgt_chr <- tgt_snp[1]
        if(grep("^[0-9]$",tgt_chr)) tgt_chr <- paste("chr", tgt_chr, sep = " ", collapse = NULL)
        else {
            tgt_chr <- strtolower(tgt_chr)
        }

        tgt_snp <- as.numeric(tgt_snp[2])

        if(!tgt_snp%in%snp$Position){
            stop("cannot find target snp in set")
        }
        
        #tgt_snp<-20686878
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
        plot(gmap$chr6[gmap$chr6$position>from&gmap$chr6$position<to,c(1,2)],type="l",col="blue",ylim=c(0,100),xaxs="i",yaxs="i",axes=F, xlab=NA, ylab=NA)
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
    definition = function(x, interval, snp, gmap, window = 500000, minR2 = 0.01, threads = 1, ...){
        args1 <- list(x = x, interval = interval, window = window, snp = snp, gmap = gmap, minR2 = minR2, threads = threads)
        inargs <- list(...)
        args1[names(inargs)] <- inargs

        if(length(interval) != 1)
            stop("Number of intervals is not 1!")
        
        if(interval@ranges@width != 1)
            stop("Interval length is not 1!")

        interval_string <- sprintf("%s:%s",interval@seqnames@values,interval@ranges@start)
        ld<-rtomahawk::twk_scalc(x,interval_string,threads=threads,window=window,minR2=minR2)
    
        tgt_snp <- interval@ranges@start
        tgt_chr <- as.character(interval@seqnames@values)
        if(grep("^[0-9]$",tgt_chr)) tgt_chr <- paste("chr", tgt_chr, sep = " ", collapse = NULL)
        else {
            tgt_chr <- strtolower(tgt_chr)
        }

        if(!tgt_snp%in%snp$Position){
            stop("cannot find target snp in set")
        }
        
        #tgt_snp<-20686878
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
        plot(gmap$chr6[gmap$chr6$position>from&gmap$chr6$position<to,c(1,2)],type="l",col="blue",ylim=c(0,100),xaxs="i",yaxs="i",axes=F, xlab=NA, ylab=NA)
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