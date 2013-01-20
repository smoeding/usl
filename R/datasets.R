##############################################################################
#' Performanced of a Sun SPARCcenter 2000 in the SPEC SDM91 benchmark
#' 
#' A dataset containing performance data for a Sun SPARCcenter 2000 (16 CPUs)
#' 
#' The SPEC SDM91 benchmark has been run on a
#' 
#' In October 1994 a Sun SPARCcenter 2000 (16 CPUs) was used for the SPEC
#' SDM91 benchmark. The benchmark simulates a certain number of users working  
#' on a UNIX server. It measures the number of script executions per hour
#' for a given number of simulated users.
#' 
#' \itemize{
#'   \item \code{load} The number of simulated users (1--216).
#'   \item \code{throughput} The achieved throughput in scripts per hour.
#' }
#' 
#' @name specsdm91
#' @docType data
#' @keywords datasets
#' @format A data frame with 7 rows on 2 variables
#' @source N. J. Gunther. Guerrilla Capacity Planning. Springer-Verlag,
#'   Heidelberg, Germany, 2007.
#'   Original dataset from
#'   \url{http://www.spec.org/osg/sdm91/results/results.html}
NULL


##############################################################################
#' Performance of a ray-tracing software on different hardware configurations
#' 
#' A dataset containing performance data for a ray-tracing benchmark.
#' 
#' The benchmark measured the number of ray-geometry intersections per second.
#' The data was gathered on an SGI Origin 2000 with 64 R12000 processors
#' running at 300 MHz.
#' 
#' \itemize{
#'   \item \code{processors} The number of CPUs used for the benchmark (1--64).
#'   \item \code{throughput} The number of operations per second.
#' }
#' 
#' @name raytracer
#' @docType data
#' @keywords datasets
#' @format A data frame with 11 rows on 2 variables
#' @source N. J. Gunther. Guerrilla Capacity Planning. Springer-Verlag,
#'   Heidelberg, Germany, 2007.
#'   Original dataset from
#'   \url{http://sourceforge.net/projects/brlcad/}
NULL
