#!/usr/bin/env Rscript
#
# Reference chart
#
# Wim Otte (w.m.otte@umcutrecht.nl)
################################################################################
library( 'ggplot2' )
library( 'ggeffects' )

################################################################################
# BEGIN FUNCTIONS
################################################################################

### 
# Helper function plot.
##
number_ticks <- function( n ) { function( limits ) pretty( limits, n ) }

###
# Get (x,y) data
##
get_xy_data <- function()
{
    # demo (x,y) data taken from ggplot2's example data set 'mpg'
    df <- data.frame( x = ggplot2::mpg$displ, y = ggplot2::mpg$hwy )   
}

###
# Fit model
##
fit_model <- function( df )
{
    # fit polynomial model
    #model <- stats::lm( y ~ poly( x, 2 ), data = df )
    
    # fit natural splines
    model <- stats::lm( y ~ splines::ns( x, 3 ), data = df )
        
    return( model )
}

###
# get prediction intervals
##
get_prediction_intervals <- function( model, intervals = c( 0.25, 0.50, 0.75 ) )
{
    # container
    all <- NULL
    
    for( interval in intervals )
    {
        # get boundaries for interval
        boundaries <- data.frame( ggeffects::ggpredict( model, ci.lvl = interval ) )
        
        # clean colnames
        colnames( boundaries ) <- stringr::str_split_fixed( colnames( boundaries ), '\\.', 2 )[ , 2 ]
        
        # add interval col
        boundaries$interval <- interval
        
        # add to container
        all <- rbind( all, boundaries )
    }
    
    return( all )
}

###
# Plot intervals.
##
plot_intervals <- function( intervals, xlab = 'X label', ylab = 'Y label', legend_title = 'Legend title' )
{
    # get colors from https://colorbrewer2.org/#type=sequential&scheme=Blues&n=9
    # add more if > 3 intervals!
    blue_colors <- rev( c( '#08519c', '#3182bd', '#6baed6' ) )
    
    # get levels to modify such that lowest values are on top of higher values
    intervals$interval <- as.factor( intervals$interval )
    l <- levels( intervals$interval )
    
    # get prefix (a,b,c...) reversed
    prefix <- rev( letters[ seq( from = 1, to = length( l ) ) ] )
    
    # add prefix to levels
    levels( intervals$interval ) <- paste0( prefix, '-', l )
    
    # regroup to have widest interval first [i.e., 'a-0.75', 'b-0.5, 'c-0.25', ...]
    intervals$interval <- as.factor( as.character( intervals$interval ) )
    
    # rename to get rid of 'c-', 'b-', 'a-', ...
    levels( intervals$interval ) <- stringr::str_split_fixed( levels( intervals$interval ), '-', 2 )[, 2 ]
    
    # get plot
    p <- ggplot( data = intervals, aes( x = x ) ) +
        geom_ribbon( aes( x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = interval, fill = interval ), alpha = 0.4 ) +
        geom_line( aes( y = predicted ), colour = 'gray30', linetype = 'dashed', alpha = 0.6 ) + 
        xlab( xlab ) +
        ylab( ylab ) +
        scale_x_continuous( breaks = number_ticks( 10 ) ) + # add more or less depending on grid detail needed
        scale_y_continuous( breaks = number_ticks( 10 ) ) + # ""
        scale_fill_manual( values = blue_colors ) +
        theme( legend.position = 'top' ) +
        guides( fill = guide_legend( title = legend_title ) )
    
    return( p )
}

################################################################################
# END FUNCTIONS
################################################################################

# output dir
outdir <- 'out.reference.chart'
dir.create( outdir, showWarnings = FALSE )

# get data
df <- get_xy_data()

# fit model
model <- fit_model( df )

# get intervals
intervals <- get_prediction_intervals( model )

# plot intervals
p <- plot_intervals( intervals )

# save plot as png / pdf
ggsave( plot = p, file = paste0( outdir, '/reference-chart.png' ), dpi = 300, height = 6, width = 8, bg = 'white' )
ggsave( plot = p, file = paste0( outdir, '/reference-chart.pdf' ), dpi = 300, height = 6, width = 8, bg = 'white' )

# save plot as raw R-object, for later use
save( p, file = paste0( outdir, '/p.Rdata' ) )

