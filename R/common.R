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
        cat("Number of blocks ", nrow(object@records),"\nrecords ", sum(as.numeric(object@records$n)), " \nUncompressed ", sum(as.numeric(object@records$b_unc)), "\nCompressed ", sum(as.numeric(object@records$b_cmp)), sep = "")
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
        return(twk_tail(x));
    }
)