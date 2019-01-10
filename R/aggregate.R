source("R/common.R") # require "twk" classes

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

# General plotting method for aggregation.
setMethod("plot",
    signature(x="twk_agg", y="ANY"),
    definition = function(x, normalize = TRUE, ...){
        if(length(x@data) == 0)
            return(FALSE)

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
        dist<-table(x@data[x@data>0])
        cumdist<-cumsum(dist)/sum(dist)
        col_breaks<-rep(0,args1$color_range-1)
        for(j in 1:(args1$color_range-1)){
            # Compute 10-percentile bins using the nearest-rank method
            col_breaks[j] = as.numeric(names(cumdist)[which.max(cumdist >= (min(cumdist) + ((max(cumdist) - min(cumdist)) / (args1$color_range) * j)))])
        }
        
        par(mar=args1$mar)
        if(normalize)
            image(x@data, useRaster = T, axes=F, xaxt='n', yaxt='n', ann=FALSE, bty="n", col=args1$colors, breaks=c(0,col_breaks, max(x@data)))
        else
            image(x@data, useRaster = T, axes=F, xaxt='n', yaxt='n', ann=FALSE, bty="n", col=args1$colors)

        suppressWarnings(par(curpar))
    }
)