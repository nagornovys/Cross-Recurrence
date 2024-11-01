# The template for a single Approximate Bayesian computation using MaxWiK method
#      and plot the recurrence plots to show meta-sampling 

# Libraries:

library('ggplot2')
library('MaxWiK')

library('pracma')
library('ggplot2')
library( 'crqa' )

# Folder with functions of the MaxWiK package:
folder.MaxWiK = '../../../Combinational ABC/MaxWiK/R/'

# Load the functions from the MaxWiK package:
source(paste0(folder.MaxWiK, 'lib_iKernel.R'))
source(paste0(folder.MaxWiK, 'utils.R'))



# Save to file or do not
print_to_file = TRUE





# Save plots to a PDF file
if ( print_to_file ) pdf("recurrence_plots.pdf")

# Theme for ggplot2 plots:
thm_gg <- theme_minimal() +
                theme(
                    plot.title = element_text(color="red", size=14, face="bold.italic"),
                    axis.text  = element_text(size=16), 
                    axis.title.x = element_text(color="black", size=22, face="bold"),
                    axis.title.y = element_text(color="black", size=22, face="bold"), 
                    legend.title = element_text(color="black", size=18, face="bold"),
                    legend.text  = element_text(color="black", size=18, face="bold"),
                    # legend.position="none"
                ) 




# Get input data ----------------------------------------------------------

source('./RUN.Metadata.R')

# Meta-sampling -----------------------------------------------------------


# Find the best one:

MAP  =  meta.sampling$par.best

# Make Hilbert representation of generated parameters  --------------------------------------------------------------------

network  =  do.call(rbind.data.frame, meta.sampling$spiderweb )
nr = nrow(  meta.sampling$spiderweb[[ 1 ]] )
lng = length(  meta.sampling$spiderweb )
network$step  =  rep( x = 1:lng, each = nr  )

iKernelABC_df =  meta.sampling$iKernelABC
feature_network  =  get_voronoi_feature_PART_dataset( data = rbind( par.sim, network[, c(1,2)] ), 
                                                      talkative = TRUE, start_row = nrow( par.sim ) + 1 ,  
                                                      Matrix_Voronoi = iKernelABC_df$parameters_Matrix_Voronoi )

M_iKernel  =  feature_network[["M_iKernel"]]

kernel_mean_embedding  =  iKernelABC_df[["kernel_mean_embedding"]] 

sim_network  =  iKernel_point_dataset( Matrix_iKernel = feature_network$M_iKernel, 
                                       t = iKernelABC_df$t, nr = nrow( feature_network$M_iKernel ), 
                                       iFeature_point = iKernelABC_df$kernel_mean_embedding )

network$similarity  =  sim_network  

# Plot results ------------------------------------------------------------

nm  =  names( par.sim)[ 1 ]

M.pl = MaxWiK:: MaxWiK.ggplot.density( title      = '', 
                                datafr1    = posteriori.MaxWiK, 
                                datafr2    = network, 
                                var.df     = nm, 
                                obs.true   = X.true[ 1, nm ], 
                                best.sim   = MAP[ 1, nm ]
                                    )
print( M.pl)
nm  =  names( par.sim)[ 2 ]

M.pl = MaxWiK:: MaxWiK.ggplot.density( title      = '', 
                                datafr1    = posteriori.MaxWiK, 
                                datafr2    = network, 
                                var.df     = nm, 
                                obs.true   = X.true[ 1, nm ], 
                                best.sim   = MAP[ 1, nm ]
)

print( M.pl)


# Recurrence plots --------------------------------------------------------

# Calculate the recurrence matrix
distance_threshold <- 0.1

stt_all  <-  NULL

for( i_tree  in  unique(network$step) ){
    
    if ( TRUE ){
        data1  =  NULL
        
        w  =  which( network$step == i_tree)
        for( i in w ){
            data1  = c( data1, M_iKernel[ i, ] )
        }
        
        data2  =  rep(x = kernel_mean_embedding, times = length( w ) )
    } else {
        w  =  which( network$step == i_tree)
        data1  =  M_iKernel[ w[1], ]
        data2  =  kernel_mean_embedding
    }
    
    
    result <- crqa::crqa(ts1 = data1,
                         ts2 = data2, 
                         delay = 1,
                         embed = 2,
                         rescale = 0,
                         radius = distance_threshold,
                         mindiagline = 3,
                         minvertline = 2, 
                         metric = 'euclidean', 
                         whiteline = TRUE,
                         datatype = 'Random' )
    # Basic plot
    # crqa::plot_rp(result$RP)
    
    r.pl <- crqa:: plot_rp(result$RP, 
                   xlabel = "Narrator",
                   ylabel = "Listener",
                   title = paste0( 'The meta-sampling step N ', i_tree ),
                   pcolour = "blue") +
        geom_abline(intercept = 0, slope = 1)  +
        labs( x = "Repeated kernel mean embedding", 
              y = "Hilbert representation of convex hull points" ) +
        thm_gg + 
        theme( legend.position="none" )
        
    print( r.pl )
    
    if ( FALSE ){
        recurrence_matrix <- outer(data1, data2, FUN = function(x, y) abs(x - y) <= distance_threshold)
        
        
        # Convert recurrence matrix to a data frame
        recurrence_df <- data.frame(
            x = rep(1:length(data1), each = length(data1)),
            y = rep(1:length(data2), times = length(data2)),
            recurrence = as.vector(recurrence_matrix)
        )
        
        # Create the recurrence plot using ggplot2
        recurrence_plot <- ggplot(recurrence_df, aes(x = x, y = y, fill = recurrence)) +
            geom_tile() +
            geom_abline(intercept = 0, slope = 1)  +
            scale_fill_manual(values = c("white", "blue")) +
            labs( x = "Repeated kernel mean embedding", 
                  y = "Hilbert representation of convex hull points" ) +
            thm_gg
        
        # Display the plot
        print(recurrence_plot)
    }
    
    # Get statistics from Cross Recurrence Plots:
    stt  =  as.data.frame( result[ -11 ] ) 
    stt$step  =  i_tree
    stt_all  =  rbind( stt_all, stt )
}    

print( stt_all )

stt_all = stt_all[ , c(11, 1:7) ]

# Transform the data frame to the LaTeX table:
if ( FALSE ){
  library('xtable')
  print( xtable( x = stt_all, align = rep('c', ncol( stt_all) + 1 ), type = 'latex' ), 
         include.rownames = FALSE )
}

for( i in 2:8 ){
    
    nm = names( stt_all )[ i ]
    
    s.pl = ggplot( data = stt_all ) +
        geom_line( aes_string( x = 'step', y = nm ), 
                   size = 1, color='blue' ) +
        thm_gg +
        labs( x = "Step", y = nm ) 
    
    print( s.pl )
}


# Analysis:
# nr ; lng 
if ( FALSE ){
    st_1 = 1
    st_2 = round( lng/3 )
    st_2 = ifelse( st_2 == 1, 2, st_2 )
    st_3 = round( lng/3 * 2 )
    st_3 = ifelse( st_3 == lng, lng-1, st_3 )
    st_4 = lng
}
# Another case:

st_1 = 1
st_2 = 4
st_3 = 6
st_4 = 8

if ( FALSE ){
    st_1 = 8
    st_2 = 10
    st_3 = 12
    st_4 = 13
}
for ( nm in c( 'par.sim.X1', 'par.sim.X2' ) ){
    ddt = data.frame(   X = c( network[ , nm ][ which( network$step == st_1 ) ], 
                               network[ , nm ][ which( network$step == st_2 ) ],
                               network[ , nm ][ which( network$step == st_3 ) ],
                               network[ , nm ][ which( network$step == st_4 ) ]),
                        Step = c( rep( st_1, nr ), rep( st_2, nr), rep( st_3, nr), rep( st_4, nr) )
                    )
    
    ddt$Step = as.factor( ddt$Step )
    library(tidyverse)
    if ( TRUE ){
        l1 =     geom_vline( aes(xintercept= X.true[ 1, nm ]),   
                             color='red', linetype='dashed', linewidth=0.7)
        
        l2 =     geom_vline( aes(xintercept= MAP[1, nm ]),   
                             color='blue', linetype='dotted', linewidth=0.7)
        
        d.pl <-    ggplot( data = ddt, aes( x = X, fill = Step ) ) +
             geom_density( alpha=.3 ) +
            thm_gg +
            labs( x = paste0("Parameter ", nm  ), y = 'Probability density' ) +
            l1 + l2 
        print( d.pl )
    }    

}    

# Similarity measure:

ddt = data.frame(   X = c( network$similarity[ which( network$step == 1 ) ], 
                           network$similarity[ which( network$step == st_2 ) ],
                           network$similarity[ which( network$step == st_3 ) ],
                           network$similarity[ which( network$step == lng ) ]),
                    Step = c( rep( 1, nr ), rep( st_2, nr), rep( st_3, nr), rep( lng, nr) ),
                    ID = rep( 1:nr, 4 )
)

ddt$Step = as.factor( ddt$Step )

s.pl = ggplot( data = ddt ) +
    geom_line( aes( x = ID, y = X, group=Step, color=Step, linetype = Step ), 
                 size = 1 ) +
    # stat_smooth(aes(y=X, x=ID, group=Step, color=Step, linetype = Step ), 
    #             method = lm, formula = y ~ x , se = FALSE) +
    
    # geom_point( aes( x = ID, y = X, group=Step, color=Step ), size = 3) +
    # geom_line(data = spline_int, aes(x = x, y = y ) ) +
    # geom_smooth( aes( x = ID, y = X, group=Step, color=Step, linetype = Step ), n = 10, se = F ) +
    thm_gg +
    labs( x = "ID of points", y = 'Similarity' ) 

print( s.pl )

cat("Recurrence Rate (RR) or The percentage of recurrent points falling within 
    the specified radius (range between 0 and 100):", result$RR, "\n")

cat("Determinism (DET) or Proportion of recurrent points
    forming diagonal line structures.:", result$DET, "\n")

cat( "The total number of lines in the recurrent plot", result$NRLINE, "\n" )

cat("Longest Diagonal Line (maxL) or The length of the longest diagonal 
    line segment in the plot, excluding the main diagonal: ", result$maxL, "\n")

cat("Mean Diagonal Line Length (L) or The average length 
    of line structures: ", result$L, "\n")

cat("Entropy (ENTR) or Shannon information entropy of 
    diagonal line lengths longer than the minimum length:", result$ENTR, "\n")

cat(" Entropy measure normalized by the number of lines observed in the plot. 
    Handy to compare across contexts and conditions (rENTR): ", result$rENTR, "\n" )

cat("Laminarity (LAM) Proportion of recurrent points 
    forming vertical line structures:", result$LAM, "\n")

cat("Mean Vertical Line Length (TT) or The average length 
    of vertical line structures: ", result$TT, "\n")

cat("Entropy of categorical recurrence plots based on 
    rectangular block structures", result$catH, "\n" )

cat("The Recurrence Plot sparse matrix data is presented in result$RP \n" )

if ( print_to_file ) dev.off()




