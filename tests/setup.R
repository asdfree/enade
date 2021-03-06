if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
enade_cat <- get_catalog( "enade" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( enade_cat ) ) / ceiling( nrow( enade_cat ) / 2 ) )
enade_cat <- enade_cat[ record_categories == this_sample_break , ]
enade_cat <- lodown( "enade" , enade_cat )
if( any( enade_cat$year == 2016 ) ){
enade_df <- readRDS( file.path( getwd() , "2016 main.rds" ) )

enade_df <- 
	transform( 
		enade_df , 
		
		# qual foi o tempo gasto por voce para concluir a prova?
		less_than_two_hours = as.numeric( qp_i9 %in% c( 'A' , 'B' ) ) ,
		

		state_name = 
			factor( 
				co_uf_curso , 
				levels = c( 11:17 , 21:29 , 31:33 , 35 , 41:43 , 50:53 ) ,
				labels = c( "Rondonia" , "Acre" , "Amazonas" , 
				"Roraima" , "Para" , "Amapa" , "Tocantins" , 
				"Maranhao" , "Piaui" , "Ceara" , "Rio Grande do Norte" , 
				"Paraiba" , "Pernambuco" , "Alagoas" , "Sergipe" , 
				"Bahia" , "Minas Gerais" , "Espirito Santo" , 
				"Rio de Janeiro" , "Sao Paulo" , "Parana" , 
				"Santa Catarina" , "Rio Grande do Sul" , 
				"Mato Grosso do Sul" , "Mato Grosso" , "Goias" , 
				"Distrito Federal" )
			)

	)
	
nrow( enade_df )

table( enade_df[ , "tp_sexo" ] , useNA = "always" )
mean( enade_df[ , "nt_obj_fg" ] , na.rm = TRUE )

tapply(
	enade_df[ , "nt_obj_fg" ] ,
	enade_df[ , "tp_sexo" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( enade_df[ , "state_name" ] ) )

prop.table(
	table( enade_df[ , c( "state_name" , "tp_sexo" ) ] ) ,
	margin = 2
)
sum( enade_df[ , "nt_obj_fg" ] , na.rm = TRUE )

tapply(
	enade_df[ , "nt_obj_fg" ] ,
	enade_df[ , "tp_sexo" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( enade_df[ , "nt_obj_fg" ] , 0.5 , na.rm = TRUE )

tapply(
	enade_df[ , "nt_obj_fg" ] ,
	enade_df[ , "tp_sexo" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_enade_df <- subset( enade_df , qp_i1 %in% c( "A" , "B" ) )
mean( sub_enade_df[ , "nt_obj_fg" ] , na.rm = TRUE )
var( enade_df[ , "nt_obj_fg" ] , na.rm = TRUE )

tapply(
	enade_df[ , "nt_obj_fg" ] ,
	enade_df[ , "tp_sexo" ] ,
	var ,
	na.rm = TRUE 
)
t.test( nt_obj_fg ~ less_than_two_hours , enade_df )
this_table <- table( enade_df[ , c( "less_than_two_hours" , "state_name" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		nt_obj_fg ~ less_than_two_hours + state_name , 
		data = enade_df
	)

summary( glm_result )
library(dplyr)
enade_tbl <- tbl_df( enade_df )
enade_tbl %>%
	summarize( mean = mean( nt_obj_fg , na.rm = TRUE ) )

enade_tbl %>%
	group_by( tp_sexo ) %>%
	summarize( mean = mean( nt_obj_fg , na.rm = TRUE ) )
}
