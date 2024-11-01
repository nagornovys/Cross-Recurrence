
library( 'MaxWiK' )

time.start  =  Sys.time()

if ( ! is.na( metadata$seed ) )  set.seed( metadata$seed )

# ABC MaxWiK --------------------------------------------------------------



if ( metadata$task == 'abc' ){
    
    
    Matrix.Voronoi  =   NULL
    input.file  =  file.path( 'HYPERPARAMETERS', 
                              metadata$input.folder,
                              hyper$Matrix.Voronoi.file )
    if ( length(input.file ) > 0 ){
        if ( file.exists( input.file ) ){
            Matrix.Voronoi  =   readRDS( file = input.file )
        }
    }
    
    res.MaxWik  =  get.MaxWiK( 
        psi = hyper$psi, 
        t   = hyper$t, 
        param    = par.sim, 
        stat.sim = stat.sim, 
        stat.obs = Y.true, 
        talkative      = hyper$talkative, 
        check_pos_def  = hyper$check_pos_def, 
        Matrix_Voronoi = Matrix.Voronoi
    )
    
    # Get the Posteriori where similarity more than 0
    w.sim  =  which(res.MaxWik$similarity > 0 )
    posteriori.MaxWiK  =  par.sim[ w.sim, ]
    posteriori.weights =  res.MaxWik$similarity[ w.sim ] / sum( res.MaxWik$similarity[ w.sim ] )
    
    
    ##########################  META-SAMPLING  #######################################
    
    meta.sampling  =  meta_sampling( psi = hyper$psi, 
                                     t =   hyper$t, 
                                     param    = par.sim, 
                                     stat.sim = stat.sim, 
                                     stat.obs = Y.true, 
                                     talkative     = hyper$talkative, 
                                     check_pos_def = hyper$check_pos_def, 
                                     n_bullets     = hyper$n_bullets, 
                                     n_best        = hyper$n_best, 
                                     halfwidth     = hyper$halfwidth, 
                                     epsilon       = hyper$epsilon, 
                                     rate          = hyper$rate, 
                                     max_iteration = hyper$max_iteration, 
                                     save_web      = hyper$save_web, 
                                     use.iKernelABC = res.MaxWik )
    
    network  = unique.data.frame( do.call(rbind.data.frame, meta.sampling$spiderweb ) )
    
    
    # Save the results to the data frame:
    time.end      =  Sys.time()
    
    results.save  =  data.frame( 
        method = 'MaxWiK', 
        task   = metadata$task, 
        n_simulations  =  0, 
        time_of_calculation = round( as.numeric( time.end - time.start , units="secs"), digits = 3 ),
        comment = 'metasampling'
        
    )
    
    results.save = cbind.data.frame( results.save, meta.sampling$par.best)
    results.additional = meta.sampling
    
}



# Save the results --------------------------------------------------------

file.rds  =  file.path( 'RESULTS', metadata$input.folder, 
                        paste0( 'additional_for_', metadata$current.input.ID, '_using_', metadata$method, '.RDS') )
file.res  =  file.path( 'RESULTS', metadata$input.folder, 
                        paste0( metadata$current.input.ID, '_using_', metadata$method, '.txt' ) )


saveRDS( object = results.additional, file = file.rds )
write.table(x = results.save, file = file.res, append = F, sep = '\t', col.names = T, row.names = F )

# Clean the environment
suppressWarnings(
    if( metadata$delete.method.data ){
        rm( list = ls()[ ! ls() %in% metadata$NameSpace ] )
        rm( 'hyper', 'hyper.default' )
    }
)


