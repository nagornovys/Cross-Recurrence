
# Functions -------------------------------------------------
library( 'futile.logger' )
library(  'yaml' )
library('MaxWiK')

yaml.check.and.load_file  <-  function( input, ... ){
    
    if ( file.exists( input ) ){
        out = yaml.load_file( input, ... )
        return( out )
    } else {
        flog.info( paste0('The file ', input, '  is not found, the yaml data file was skipped.' ) )
        return( NA )
    }
}

check.and.source  <-  function( input, ... ){
    
    if ( file.exists( input ) ){
        source( input, ... )
        return( NULL )
    } else {
        flog.info( paste0('The file ', input, '  is not found, script running was skipped.' ) )
        return( NA )
    }
}

add.data.set.from.yml.description  <-  function( fldr, descr.file  =  'Description.yml', out.file = 'data.sets.txt' ){
    
    descr.metadata  =  yaml.check.and.load_file( file.path( fldr, descr.file ) )
    
    if ( file.exists( out.file ) ){
        DF = read_file( file_name = out.file )
    } else {
        DF = data.frame( ID = 'DS.1', 
                         Folder = fldr,
                         File.name = 'file.txt', 
                         Comment = 'comment', 
                         Dimension = 2, 
                         Size = 2000, 
                         Feature = 'No features', 
                         Field = 'Math'
                         )
        DF = DF[ -1, ]
    }
    
    for ( i in 1:length( descr.metadata$names ) ){
        n.id = nrow( DF ) + 1
        DF[ n.id, 'ID' ]  =  paste0( 'DS_', descr.metadata$names[ i ] )
        DF[ n.id, 'Folder' ] = descr.metadata$folder
        DF[ n.id, 'File.name' ] = descr.metadata$files[ i ]
        DF[ n.id, 'Comment' ]   = descr.metadata$Comment
        DF[ n.id, 'Dimension' ]   = descr.metadata$dimension
        DF[ n.id, 'Size' ]   = descr.metadata$Size
        DF[ n.id, 'Feature' ]   = descr.metadata$noise[ i ]
        DF[ n.id, 'Field' ]   = descr.metadata$Field
    }
    
    write.table( x = DF, file = out.file, append = FALSE, sep = '\t', row.names = FALSE )
    
}

load.hyperparameters  <-  function( metadata, load.general = FALSE, add.default = TRUE ){
    
    
    if ( metadata$method == 'all' ){
        hyper =  yaml.check.and.load_file( input = file.path( 'HYPERPARAMETERS/all.default.yml' ) )
        return( hyper )
    }
    
    if ( load.general ){
        hyper =  yaml.check.and.load_file( input = file.path( 'HYPERPARAMETERS/', paste0( metadata$method, '.default.yml' ) ) )
        return( hyper )
    }
    
    # First to check file of hyperparameters 
    if ( metadata$hyperparameters != 'default' ){
        hyper  =  yaml.check.and.load_file( input = file.path( 'HYPERPARAMETERS', 
                                                               metadata$input.folder, 
                                                               metadata$hyperparameters ) )
    } else {
        # if default then load the hyperparameters from the specific folder:
        hyper  =  yaml.check.and.load_file( input = file.path( 'HYPERPARAMETERS', 
                                                               metadata$input.folder,
                                            paste0( metadata$method, '.for.', metadata$current.input.ID, '.yml' ) ) )
    }
    
    # Add hyperparameters from the default set:
    if ( add.default ){
        hyper.default  =  yaml.check.and.load_file( input = file.path( 'HYPERPARAMETERS', 
                                                            paste0( metadata$method, '.default.yml' ) ) )
        
        # Combine new values and the general default values:
        w      =  !( names( hyper.default ) %in% names( hyper ) )
        if ( is.na( hyper[1] ) ) hyper = NULL
        hyper  =  c( hyper, hyper.default[ w ] )
    }
    
    if ( is.na( hyper )[ 1 ] ){
        flog.info( paste0('Hyperparameters for the method ', metadata$method, ' are not found. '))
    }
    
    return( hyper )
}

# Metadata loading -------------------------------------------------

# READ Metadata for the simulations
suppressWarnings( rm( metadata ) )
metadata  =  yaml.check.and.load_file(input = './Metadata.yml')





metadata$current.input.ID = NULL

# START of LOOP for input datasets :
input.ID.in.the.loop  =  metadata$input.ID 

metadata$current.input.ID = input.ID.in.the.loop 
message( paste0('Start simulation for dataset in the folder: ', metadata$input.folder, '\n' ) )
message( paste0('Start simulation for the file of the dataset: ', metadata$input.file, '\n' ) )

# Load the data to get: stat.sim, par.sim, and X.true, Y.true values
check.and.source( file.path( 'Input.data', metadata$input.folder,  'Load.data.R'   ) )

# Load hyperparameters:
hyper.default  =  load.hyperparameters( metadata = metadata, load.general = TRUE )
hyper          =  load.hyperparameters( metadata = metadata, load.general = FALSE, add.default = TRUE )

# Run the simulation -------------------------------------------------

# Create the folder for the results:
for ( fldr in file.path( 'RESULTS', metadata$input.folder ) ){
    if ( !file.exists( fldr ) ) dir.create( fldr, recursive = T )
}


# Run the simulations:

if ( metadata$method != 'all' ){
    check.and.source( file.path(  'METHODS',
                        paste0( metadata$method, '.R' ) ) )
} else {
    check.and.source( file.path(  'METHODS/all.R' ) )
}



