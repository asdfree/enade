if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

enade_cat <-
	get_catalog( "enade" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( enade_cat ) ) / ceiling( nrow( enade_cat ) / 2 ) )

enade_cat <- unique( rbind( enade_cat[ record_categories == this_sample_break , ] , enade_cat[ enade_cat$year == 2015 , ] ) )

lodown( "enade" , enade_cat )
