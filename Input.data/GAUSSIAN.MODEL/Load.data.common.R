# Script to load data from the dataset
library( 'MaxWiK' )

if ( metadata$input.file == 'default' ) metadata$input.file  =  'input.data.noise.0.txt'

file_name = file.path( 'Input.data', metadata$input.folder, metadata$input.file )  
if ( file.exists( file_name ) ){
    DF  =  MaxWiK::read_file( file_name )
} else {
    stop( paste0( 'The file ', file_name, ' does noe exist!' ) )
}
stat.sim  =  DF[ , 1:metadata$dimension ]
par.sim   =  DF[ , metadata$dimension + (1:metadata$dimension) ]

true.value  =  MaxWiK::read_file( file_name = file.path( 'Input.data', metadata$input.folder, 'Gaussian.txt' ) )

X.true      =  as.data.frame( t ( true.value$x0 ) ) 
colnames( X.true )  =  colnames( par.sim )

Y.true      =  as.data.frame( t ( true.value$A ) ) 
colnames( Y.true )  =  colnames( stat.sim )  

rm( DF, true.value )

flog.info( 'Data is loaded into three data frames: stat.sim, par.sim and X.true, Y.true values' )
