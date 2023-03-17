# undergraduates
# sit for standardized testing
# exit interview
library(httr)
library(archive)

tf <- tempfile()

this_url <-	"https://download.inep.gov.br/microdados/microdados_enade_2021.zip"

GET( this_url , write_disk( tf ) )

archive_extract( tf , dir = tempdir() )

read_enade_archive <-
	function( this_regular_expression , this_directory ){
		this_filename <- 
			grep( 
				this_regular_expression , 
				list.files( 
					this_directory ,
					recursive = TRUE ,
					full.names = TRUE 
				) , 
				value = TRUE 
			)

		this_df <-
			read.table(
				this_filename ,
				header = TRUE ,
				sep = ";" ,
				na.strings = ""
			)
			
		names( this_df ) <- tolower( names( this_df ) )
		
		this_df
	}

arq1_df <- read_enade_archive( 'arq1\\.txt$' , tempdir() )

arq1_df <- unique( arq1_df[ c( 'co_curso' , 'co_uf_curso' , 'co_categad' , 'co_grupo' ) ] )

arq3_df <- read_enade_archive( 'arq3\\.txt$' , tempdir() )

enade_df <- merge( arq3_df , arq1_df )

stopifnot( nrow( enade_df ) == nrow( arq3_df ) )

# enade_fn <- file.path( path.expand( "~" ) , "ENADE" , "this_file.rds" )
# saveRDS( enade_df , file = enade_fn , compress = FALSE )
# enade_df <- readRDS( enade_fn )
enade_df <- 
	transform( 
		enade_df , 
		
		# qual foi o tempo gasto por voce para concluir a prova?
		less_than_two_hours = as.numeric( co_rs_i9 %in% c( 'A' , 'B' ) ) ,
		
		administrative_category =
			factor(
				co_categad ,
				levels = c( 1:5 , 7 ) ,
				labels = c( '1. Pública Federal' , '2. Pública Estadual' , 
				'3. Pública Municipal' , '4. Privada com fins lucrativos' , 
				'5. Privada sem fins lucrativos' , '7. Especial' )
			) ,

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

table( enade_df[ , "administrative_category" ] , useNA = "always" )
mean( enade_df[ , "nt_obj_fg" ] , na.rm = TRUE )

tapply(
	enade_df[ , "nt_obj_fg" ] ,
	enade_df[ , "administrative_category" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( enade_df[ , "state_name" ] ) )

prop.table(
	table( enade_df[ , c( "state_name" , "administrative_category" ) ] ) ,
	margin = 2
)
sum( enade_df[ , "nt_obj_fg" ] , na.rm = TRUE )

tapply(
	enade_df[ , "nt_obj_fg" ] ,
	enade_df[ , "administrative_category" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( enade_df[ , "nt_obj_fg" ] , 0.5 , na.rm = TRUE )

tapply(
	enade_df[ , "nt_obj_fg" ] ,
	enade_df[ , "administrative_category" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_enade_df <- subset( enade_df , co_rs_i1 %in% c( "A" , "B" ) )
mean( sub_enade_df[ , "nt_obj_fg" ] , na.rm = TRUE )
var( enade_df[ , "nt_obj_fg" ] , na.rm = TRUE )

tapply(
	enade_df[ , "nt_obj_fg" ] ,
	enade_df[ , "administrative_category" ] ,
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
enade_tbl <- as_tibble( enade_df )
enade_tbl %>%
	summarize( mean = mean( nt_obj_fg , na.rm = TRUE ) )

enade_tbl %>%
	group_by( administrative_category ) %>%
	summarize( mean = mean( nt_obj_fg , na.rm = TRUE ) )
library(data.table)
enade_dt <- data.table( enade_df )
enade_dt[ , mean( nt_obj_fg , na.rm = TRUE ) ]

enade_dt[ , mean( nt_obj_fg , na.rm = TRUE ) , by = administrative_category ]

it_students <- subset( enade_df , co_grupo %in% 6409 )

results <- sapply( it_students[ c( 'nt_fg' , 'nt_ce' , 'nt_ger' ) ] , mean , na.rm = TRUE )

stopifnot( round( results[ 'nt_fg' ] , 1 ) == 30.4 )
stopifnot( round( results[ 'nt_ce' ] , 1 ) == 38.2 )
stopifnot( round( results[ 'nt_ger' ] , 1 ) == 36.3 )

