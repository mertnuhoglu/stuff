source("utils.R")

ss = function() {
	#source( "" )
	rt()
	source("globals.R")
	source("utils.R")
	source("utils_scripts.R")
	source("utils_verify.R")
}

select_id = function(df, rdb_entity_name) {
	select_(df, .dots = find_id_col(rdb_entity_name))
}

find_id_col = function(rdb_entity_name) {
	paste0( toUnderscore(rdb_entity_name), "_id" )
}

undup_rows = function( rdb_entity_name ) {
	# @todo: columns için özel değer belirtilebilsin
	rdf = r_rdb2( rdb_entity_name, with_invalid = T )
	columns = find_id_col(rdb_entity_name)
	undup_rdf = rdf %>%
		unduplicate_rows(columns) 
}

exportl = function( dfl, paths ) {
	exporter = function( df, path ) export(df, path)
	Map( export, dfl, paths )
}

diff_df = function( mnn, sbt ) {
	dff = mnn %>%
		anti_join( sbt, by = "user_id" )
	export( dff, "../view/verify/difference.tsv" )
	return(dff)
}

