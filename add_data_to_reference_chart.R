#!/usr/bin/env Rscript
#
# Add custom data to reference chart
#
# Wim Otte (w.m.otte@umcutrecht.nl)
################################################################################
library( 'ggplot2' )


################################################################################
# BEGIN FUNCTIONS
################################################################################

# ...

################################################################################
# END FUNCTIONS
################################################################################

# output dir
outdir <- 'out.add.data.to.reference.chart'
dir.create( outdir, showWarnings = FALSE )

# load baseline reference chart plot
load( 'out.reference.chart/p.Rdata' )

# get custom data
data <- data.frame( x = c( 2, 3, 4, 5, 6 ), 
                    y = c( 28, 20, 16.5, 19, 19.3 ) )

# overlay data on 'p'
# note that the axes re-scale if required
p_custom <- p + geom_line( data = data, aes( x = x, y = y ), colour = 'gray30', linetype = 'dashed' ) +
                geom_point( data = data, aes( x = x, y = y ), colour = '#ef3b2c', size = 2 ) 

 
# save plot as png / pdf
ggsave( plot = p_custom, file = paste0( outdir, '/reference-chart_with_custom_data.png' ), dpi = 300, height = 6, width = 8, bg = 'white' )
ggsave( plot = p_custom, file = paste0( outdir, '/reference-chart_with_custom_data.pdf' ), dpi = 300, height = 6, width = 8, bg = 'white' )

