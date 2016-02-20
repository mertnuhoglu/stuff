library("lubridate")
library("stringr")
library("pryr")
library("data.table")
library("pipeR"); library("plyr"); library("dplyr")
library("tidyr")
library("magrittr")
library("rlist")
library("XML")
library("xml2"); library("rvest")
library("reshape2")
library("gtools")
library("rjson")
library("compare")
library("readr")
library("readxl")

# shortcuts
tb = traceback

make_rows = function(alist, indexes) {
	# convert a list that contains multiple rows of a dataframe to a dataframe
	# where multiple rows are handled properly
	df0 = lapply(alist, function(el) data.frame(val = el)) %>% rbindlist
	counts = lapply(alist, length)
	nl = rep(indexes, counts)
	result = cbind(df0, index = nl)
	return(result)
}
noop = function(...) { 
	dots = list(...)
	return(unlist(dots, recursive = F))
}

# efficient versions of set functions
intersect = dplyr::intersect
union = dplyr::union
setdiff = dplyr::setdiff
setequal = dplyr::setequal

taildt = function(x, n = 6L, ...) 
	x[ (nrow(x)-n):nrow(x) ] 

null_fun = function(fun) {
	function(...) {
		if (is.blank(...)) return()
		fun(...)
	}
}

null2na = function(mat) {
    mat[sapply(mat, is.null)] <- NA
    return(mat)
}

compareo = function(x, y) compare(x, y, ignoreOrder=T)

join_filename_cik = function(df) read_mapping_cik_filename() %>% right_join(df, by="filename")

# filter blank values out
compact = partial(Filter, Negate(is.null))

unlistr = partial(unlist, recursive = F)
pastec = partial(paste, collapse=", ")

# function parameters modified st. data arg is always the first arg (for magrittr)
sprintm = function(x, fmt, ...) sprintf(fmt, x, ...)
# map x with names(x)
mapn = function(x, fun) Map(fun, x, names(x))
# map x magrittr way
mapm = function(x, fun, ...) Map(fun, x, ...)
# map and performance tracking
mapp = function(fun, x, y, ...) 
	llplyp(seq_along(x),
		function(i, x, y)
			fun(x[[i]], y[[i]])
		, x, y, ...
	)
filterm = function(x, fun) Filter(fun, x)
triml = function(x, ch) ltrim_char(ch, x)
trimr = function(x, ch) rtrim_char(ch, x)
duplicatedv = function(x) x %>% duplicated %>% extractm(x)
reverse_args = function(fun) 
	function(arg1, arg2, ...)
		fun(arg2, arg1, ...)
mag = reverse_args
m = reverse_args
extractm = function(x, df) extract(df, x)
partialm = function(fun, x) partial(x, fun)
grepm = reverse_args(grep)
grepv = partial(grepm, value = T)
greplm = function(x, pattern, ...) {
	grepl(pattern, x, ...)
}
vgrep = function(x, patterns, ...) {
	x %>% 
		grepm( patterns %>% paste(collapse="|"), ...) %>%
		unique
}
vgrepv = partial(vgrep, value = T)
subm = function(x, pattern, replacement, ...) {
	sub(pattern, replacement, x, ...)
}
gsubm = function(x, pattern, replacement, ...) {
	gsub(pattern, replacement, x, ...)
}

pre0 = function(x, y) paste0(y, x)
"%+%" = function(...) paste0(...,sep="")

sample_dataframe = function(dt, n = 100) dt[ sample(nrow(dt), size = n), ]
sample_datatable = function(dt, n = 100) dt[ sample(nrow(dt), size = n) ]
sample_with_replace = function(v, n = 100) sample(v, size = n, replace = T)

form_name = function( urls, ext = T) {
	file_name_pattern = '\\d+/[^/]*$'
	result = sub( "/", "-", str_extract(urls, file_name_pattern) )
	if ( ext ) {
		return(result)
	} else {
		return( sub( "\\.\\w*$", "", result ) )
	}
}
.url2fn = function(url) form_name(url, ext=F)
url2fn = null_fun(.url2fn)
.url = function(filename) {
	pattern = "^(\\d+)-([^.]*)$"
	m = str_match(filename, pattern)
	cik = m[, 2]
	filing = m[, 3]
	sprintf("http://www.sec.gov/Archives/edgar/data/%s/%s.txt", cik, filing)
}
fn2url = null_fun(.url)
.cik = function(filename) {
	str_match(filename, "^(\\d+)-([^.]*)$")[, 2] %>%
		as.numeric %>%
		sprintm("%010d")
}
fn2cik = null_fun(.cik)
interactive_xbrl = function(filename) {
	pattern = "^(\\d+)-([^.]*)$"
	m = str_match(filename, pattern)
	cik = m[, 2]
	filing = m[, 3]
	sprintf("https://www.sec.gov/cgi-bin/viewer?action=view&cik=%s&accession_number=%s&xbrl_type=v", cik, filing)
}


partial_year_quarter = function(fun) {
	partial(fun, year = 2009:year(Sys.Date()), quarter = 1:4)
}

sprintf1 = function(fmt, param) sprintf(fmt, param)
sprintf_year_quarter = function(fmt, year, quarter) {
	df = CJ(as.character(year), as.character(quarter))
	sprintf(fmt, df$V1, df$V2)
}

build_periods2 = partial( sprintf_year_quarter, fmt = "%s/QTR%s" )

# output: "2013/QTR1" "2013/QTR2" "2013/QTR3" "2013/QTR4" "2014/QTR1" "2014/QTR2" "2014/QTR3" "2014/QTR4"
build_periods = function( from = 2003, to = 2015) { build_periods2(from:to, 1:4) }

build_urls2 = function(year, quarter) {
	periods = build_periods2(year, quarter)
	build.urls(periods)
}

build.urls = function(periods) {
	paste("http://www.sec.gov/Archives/edgar/full-index/", periods, "/company.zip", sep='')
}

count_isna = function(x) { sum(is.na(x)) }
count_unique = function(x) { length(unique(x)) }
cu = count_unique
ci = count_isna

get_exchange_lists = function() {
	rownames = c("otcm",
					 "otcbball",
					 "otcbbother",
					 "otcbball_other",
					 "otcbbmarket",
					 "nasdaq",
					 "nasdaq_nyse",
					 "nasdaq_amex")
	urls = c("http://www.otcmarkets.com/reports/symbol_info.csv",
				"http://www.otcbb.com/dynamic/tradingdata/download/allotcbb.txt",
				"http://www.otcbb.com/dynamic/tradingdata/download/otherotc.txt",
				"http://www.otcbb.com/dynamic/tradingdata/download/allotcbb_otherotc.txt",
				"http://www.otcbb.com/dynamic/tradingdata/download/mmids.txt",
				"http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download",
				"http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nyse&render=download",
				"http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=amex&render=download")
	files = sapply( rownames, path_array_exchange_listing_x )
	keep.files = c("otcm",
						"otcbball",
						"nasdaq",
						"nasdaq_nyse",
						"nasdaq_amex")
	separators = c(",",rep("|",4),rep(",",3))
	trim_from_end = c(0,rep(1,4),rep(0,3))
	clean.files = paste('./exchanges/clean_', keep.files, '.csv', sep='')

	# meta - specs for exchange lists
	meta.df = data.frame(urls=urls, files=files, rownames=rownames, separators=separators, trim_from_end=trim_from_end, stringsAsFactors=F)
	list( meta.df=meta.df, keep.files=keep.files, clean.files=clean.files)
}

get_us_states = function() {
	states = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "X1", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
}

is.blank <- function(x, false.triggers=FALSE){
    if(is.function(x)) return(FALSE) # Some of the tests below trigger
                                     # warnings when used on functions
	 if (is.list(x)) x = unlist(x)
    return(
        is.null(x) ||                # Actually this line is unnecessary since
        length(x) == 0 ||            # length(NULL) = 0, but I like to be clear
        all(is.na(x)) ||
        all(x=="") ||
        (false.triggers && all(!x))
    )
}

get_formd_subarray_titles = function() {
	subarray_names = c(
		'edgarPreviousNameList_previousName',
		'federalExemptionsExclusions_item',
		'issuerList_issuer',
		'issuerPreviousNameList_previousName',
		#'relatedPersonsList_relatedPersonInfo',
		'salesCompensationList_recipient',
		'signatureBlock_signature'
	)
}

get_formd_subarray_titles_with_double = function() {
	c( get_formd_subarray_titles(), 
	  "statesOfSolicitationList",
	  "relatedPersonRelationshipList",
	  "relatedPersonsList_relatedPersonInfo"
	)
}

get_formd_double_subarray_titles = function() {
	subarray_names = c(
		'statesOfSolicitationList',
		'relatedPersonRelationshipList'
	)
}

get_formd_most_recent_subarray_filenames = function() { paste0('formd_most_recent_subarray_', get_formd_subarray_titles()) }

get_formd_subarray_filenames = function() { paste0('formd_subarray_', get_formd_subarray_titles()) }

get_rules_exchange_search_strings = function() {
	# rules are applied in the order of placement 
	result = fromJSON(file='config/rules_exchange_search_strings.json')
}

get.rules = function() {
	rules = list()

	rules[['us_public']] = list(
		form.in = c('10-Q', '10-Q/A', '10-K', '10-K/A'),
		mo.in = 9,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = c('15','RW'),
		mo.out = NULL
	)

	rules[['us_ipo']] = list(
		form.in = c('S-1','S-1/A'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = c('RW','10-Q','10-Q/A','10-K','10-K/A'),
		mo.out = 12
	)

	rules[['us_funded']] = list(
		form.in = c('D', 'D/A', 'REGDEX'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = NULL,
		mo.out = NULL
	)

	rules[['us_delisted']] = list(
		form.in = c('10-K','10-K/A'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = c('10-Q', '10-Q/A'),
		mo.out = 9
	)

	rules[['sec_delinquent']] = list(
		form.in = c('NT 10-K', 'NT 10-K/A', 'NT 10-Q', 'NT 10-Q/A', 'NT 20-F', 'NT 20-F/A'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = NULL,
		mo.out = NULL
	)

	rules[['foreign_sec_ipo']] = list(
		form.in = c('F-1', 'F-1/A'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = c('RW', '20-F', '20-F/A'),
		mo.out = 12
	)

	rules[['foreign_sec_filer']] = list(
		form.in = c('20-F', '20-F/A'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = c('15F'),
		mo.out = NULL
	)

	rules[['foreign_sec_filer_delisted']] = list(
		form.in = c('20-F', '20-F/A'),
		mo.in = NULL,
		form.in.and = c('15F'),
		mo.in.and = NULL,
		form.out = NULL,
		mo.out = NULL
	)

	rules[['canadian_sec']] = list(
		form.in = c('40-F', '40-F/A', 'F-10', 'F-10/A'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = c('15F'),
		mo.out = NULL
	)

	rules[['canadian_delisted']] = list(
		form.in = c('40-F', '40-F/A', 'F-10', 'F-10/A'),
		mo.in = NULL,
		form.in.and = c('15'),
		mo.in.and = NULL,
		form.out = NULL,
		mo.out = NULL
	)

	rules[['adr']] = list(
		form.in = c('F-6', 'F-6EF'),
		mo.in = 18,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = NULL,
		mo.out = NULL
	)

	rules[['adr_delisted']] = list(
		form.in = c('F-6', 'F-6EF'),
		mo.in = NULL,
		form.in.and = c('15F'),
		mo.in.and = NULL,
		form.out = NULL,
		mo.out = NULL
	)

	rules[['private_sec_filer']] = list(
		form.in = c('10-Q', '10-K'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = c('S-1'),
		mo.out = NULL
	)

	rules[['out_spin_off']] = list(
		form.in = c('10-12B'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = NULL,
		mo.out = NULL
	)

	rules[['bankrupt']] = list(
		form.in = c('REVOKED'),
		mo.in = NULL,
		form.in.and = NULL,
		mo.in.and = NULL,
		form.out = NULL,
		mo.out = NULL
	)

	return(rules)
}

get_essential_forms = function() 
	lapply(get.rules(), '[[', 'form.in')

get_xbrl_info_to_parse = function()
	c(
		"revenue",
		"net_income"
	)

# Compare vectors while taking NA as value
# http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/
compareNA <- function(v1,v2) {
    # This function returns TRUE wherever elements are the same, including NA's,
    # and false everywhere else.
    same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}

is.true  <- function(v) {
	! is.na(v) & v == T
}

# sanitize empty vectors of character() after parsing data
san = function(char) {
	if(length(char)==0) '' else char
}

basename_noext = function(filenames) basename( str_replace(filenames, "\\.\\w+$", "" ) )
ltrim_char = function(ch, x) sub( paste0("^\\", ch, "*"),"",x)
rtrim_char = function(ch, x) sub( paste0("\\",ch,"*$"),"",x)
trim_char = function(x, ch) x %>% triml(ch) %>% trimr(ch)
trim.beforecolon = function(x) sub("^.*:\\s+","",x)
trim = function(x) str_trim(x, side="both")
trimQuotes <- function (x) gsub("^'(.*)'$", "\\1", x)

months.before = function(m) {
	d = m*30
	Sys.Date() - days(d)
}

re = function(regex, x) {
	# wrong
	m = regexpr(regex,x,perl=T)
	regmatches(x,m)
}

convert.company.class.logicals.to.character = function( filing ) {
	result = c()
	if (filing$us_public) result = c(result, 'us_public')
	if (filing$us_ipo) result = c(result, 'us_ipo')
	if (filing$us_funded) result = c(result, 'us_funded')
	if (filing$us_delisted) result = c(result, 'us_delisted')
	if (filing$sec_delinquent) result = c(result, 'sec_delinquent')
	if (filing$foreign_sec_ipo) result = c(result, 'foreign_sec_ipo')
	if (filing$foreign_sec_filer) result = c(result, 'foreign_sec_filer')
	if (filing$foreign_sec_filer_delisted) result = c(result, 'foreign_sec_filer_delisted')
	if (filing$canadian_sec) result = c(result, 'canadian_sec')
	if (filing$canadian_delisted) result = c(result, 'canadian_delisted')
	if (filing$adr) result = c(result, 'adr')
	result
}

init.data.frame = function( df, rows ) {
	df = if( length(df) == 0 ) rows else rbind(df, rows)
}


log = function( ... ) {
	print( paste0( ... ) )
}

config_strategies_for_company_exchange_parsing_from_sec = function() {
	strategies = fread('config/strategies_for_company_exchange_parsing_from_sec.csv', header=T, stringsAsFactors=F)
	setkey(strategies, order_no)
}

config_filenames_real = function() { fread('config/filenames.csv', header=T, stringsAsFactors=F) }
config_filenames = config_filenames_real
config_colclasses = function() { fromJSON(file='config/colclasses.json') }
config_xbrl_tags = function() { fromJSON(file='config/xbrl_tags.json') }

.path_array_fun = function(fun_get_filename)
	function(file) 
		function(arg)
			sprintf(fun_get_filename(file), arg)
path_array_fun = .path_array_fun(get_filename)
path_array_json_fun = .path_array_fun(get_filename_json)
.read_array_fun = function(fun_path_array, fun_read_file)
	function(file)
		function(arg, ...)
			read_file_with_path(fun_path_array(file)(arg), file, .read_file_fun = fun_read_file, ...) 

.get_directory = function(name_in, test = F, data_root = '') {
	test_in = test
	filename= config_filenames()[name==name_in & test==test_in]$filename
	paste0(data_root, filename)
}                
get_directory_real = partial(.get_directory, data_root = '')
get_directory_test = partial(.get_directory, data_root = 'data/test/')
get_directory = get_directory_real

.get_filename = function(name_in, from = '', ext = '', test = F, file_ext = 'csv', data_root = '') {
	get_filename_from = function(filename, from, ext, file_ext = 'csv'){
		file_ext = ifelse( is.blank(file_ext), '', paste0('.', file_ext) )
		filename = paste0( filename, from, file_ext )
		sub( paste0('\\.', file_ext), paste0( ext, '\\.', file_ext ), filename)
	}
	test_in = test
	filename= config_filenames()[name==name_in & test==test_in]$filename
	filename = paste0(data_root, filename)
	get_filename_from(filename, from, ext, file_ext = file_ext)
}
get_filename_real = partial(.get_filename, data_root = '')
get_filename_test = partial(.get_filename, data_root = "data/test/")
get_filename = get_filename_real
get_filename_array = path_array_fun(file)
get_filename_json = partial(get_filename, file_ext = 'json')

.read.dt = function(file.name, cols = NA, ...) {
	if (is.blank(cols)) {
		cols = NA
	}
	dt = data.table( read.csv(file.name, header=T, stringsAsFactors=F, colClasses=cols, ...) )
}
.read.fread = function(file.name, cols = NULL, ...) {
	dt = fread(file.name, colClasses = cols, ...)
}
.readLines = function(filename, ...) {
	readLines(filename)
}

.read.list.load = function(filename, ...)
	list.load(filename, ...)
.read.json = function(file.name, var_name, read_fun = .read.list.load, ...){
	read_fun(file.name, ...)
}
.read.csv = function(file.name, var_name, read_fun = .read.fread, ...){
	colclasses = config_colclasses()
	cols = NULL
	if (var_name %in% names(colclasses)) {
		cols = colclasses[[var_name]]
	}
	read_fun(file.name, cols, ...)
}
read.file = function(var_name, from = '' , ext = '', test = F, fun_get_filename = get_filename, .read_file_fun = .read.csv, ...) {
	file.name = fun_get_filename(var_name, from, ext, test)
	.read_file_fun(file.name, var_name, ...)
}
read_file_real = partial(read.file, fun_get_filename = get_filename_real)
read_file_with_path = function(file.name, var_name, from = '' , ext = '', test = F, .read_file_fun = .read.csv, ...) {
	.read_file_fun(file.name, var_name, ...)
}

.write.list.save = function(alist, filename, ...) {
	list.save(alist, filename)
}
.write.json = function( alist, file.name, use_table = F, write_fun = .write.list.save, ...) {
	write_fun(alist, file.name, ...)
}
.write.csv = function( df, file.name, use_table = F, ...) {
	dir.create( dirname(file.name), recursive = T )
	if (use_table) {
		write.table(df, file.name, sep=",", ...)
	} else {
		write.csv(df, file.name, append=F, row.names=F, ...)
	} 
}           

write.file = function( df, var_name, from = '', test = F, use_table = F, fun_get_filename = get_filename, .write_file_fun = .write.csv, ...) {
	file.name = fun_get_filename(var_name, from, test = test)
	.write_file_fun(df, file.name, use_table, ...)
}
write_file_real = partial(write.file, fun_get_filename = get_filename_real)
write_file_with_path = function( df, file.name, from = '', test = F, use_table = F, .write_file_fun = .write.csv, ...) {
	.write_file_fun(df, file.name, use_table, ...)
}

.read_data = function(data) {
	fun_name = paste0('read_', data, '()')
	fun = as.expression(parse(text=fun_name))
	df = eval(fun)
}

.write_data = function(data, df) {
	fun_name = paste0('write_', data, '(df)')
	fun = as.expression(parse(text=fun_name))
	eval(fun)
}

read_file_fun = function(file, fun_get_filename, fun_read_file)
	function(...) {
		read.file(file, fun_get_filename = fun_get_filename, .read_file_fun = fun_read_file)
	}
write_file_fun = function(file, fun_get_filename, fun_write_file)
	function(df, ...)
		write.file(df, file, fun_get_filename = fun_get_filename, .write_file_fun = fun_write_file, ...)
read_csv_fun = partial( read_file_fun, fun_get_filename = get_filename, fun_read_file = .read.csv )
write_csv_fun = partial( write_file_fun, fun_get_filename = get_filename, fun_write_file = .write.csv)
read_json_fun = partial( read_file_fun, fun_get_filename = get_filename_json, fun_read_file = .read.json)
write_json_fun = partial( write_file_fun, fun_get_filename = get_filename_json, fun_write_file = .write.json)

read_array_fun = .read_array_fun(fun_path_array = path_array_fun, fun_read_file = .read.csv)
read_array_json_fun = .read_array_fun(fun_path_array = path_array_json_fun, fun_read_file = .read.json)

.write_array_fun = function(fun_path_array, fun_write_file)
	function(file)
		function(df, arg, ...)
			write_file_with_path(df, fun_path_array(file)(arg), .write_file_fun = fun_write_file, ...) 
write_array_fun = .write_array_fun(fun_path_array = path_array_fun, fun_write_file = .write.csv)
write_array_json_fun = .write_array_fun(fun_path_array = path_array_json_fun, fun_write_file = .write.json)

write_list_fun = function(file) {
	write_file_fun = write_array_fun(file)
	function(df_l, ...) {
		for (i in seq_along(df_l)) {
			title = names(df_l)[[i]]
			write_file_fun(df_l[[i]], title)
		}
	}
}
# @deprecated
read_json_fun0 = function(file)
	function(...)
		read.file(file, fun_get_filename = partial(get_filename, file_ext = 'json'), .read_file_fun = .read.json, ...)
# @deprecated
write_json_fun0 = function(file)
	function(alist, ...)
		write.file(alist, file, fun_get_filename = partial(get_filename, file_ext = 'json'), .write_file_fun = .write.json, ...)

read_aggregated_formd = function( from = '', test = F ) {
	dt = read.file('aggregated_formd', from = from, test = test)
	setkey(dt,cik)
	dt
}

write_aggregated_formd = function( df, from = '', test = F ) {
	write.file(df, 'aggregated_formd', from = from, test = test)
}
read_aggregated_formd_total = function( from = '', test = F ) { read.file('aggregated_formd_total', from = from, test = test) }
write_aggregated_formd_total = function( df, from = '', test = F ) { write.file(df, 'aggregated_formd_total', from = from, test = test) }

read_joined_formd_most_recent = function( from = '', test = F ) {
	dt = read.file('joined_formd_most_recent', from = from, test = test)
	dt$primaryIssuer_cik = as.character(dt$primaryIssuer_cik)
	dt$offeringData_typeOfFiling_newOrAmendment_isAmendment = as.logical(dt$offeringData_typeOfFiling_newOrAmendment_isAmendment)
	dt$date = as.Date(dt$date)
	setkey(dt,filename)
	dt
}

write_joined_formd_most_recent = function( df, from = '', test = F ) {
	write.file(df, 'joined_formd_most_recent', from = from, test = test)
}

read_joined_formd_filtered = function( from = '', test = F) {
	dt = read.file('joined_formd_filtered', from = from, test = test)
	dt$primaryIssuer_cik = as.character(dt$primaryIssuer_cik)
	dt$offeringData_typeOfFiling_newOrAmendment_isAmendment = as.logical(dt$offeringData_typeOfFiling_newOrAmendment_isAmendment)
	dt$date = as.Date(dt$date)
	setkey(dt,filename)
	dt
}

write_joined_formd_filtered = function( df, from = '', test = F ) {
	write.file(df, 'joined_formd_filtered', from = from, test = test)
}

read_header_formd = function( from = '', test = F ) { 
	hf = read.file('header_formd', from = from, test = test) 
	hf$filed_as_of_date = as.Date(hf$filed_as_of_date, "%Y%m%d")
	return(hf)
}
write_header_formd = function( df, from = '', test = F ) { write.file(df, 'header_formd', from = from, test = test) }
read_header_formd_sic = function( from = '', test = F ) { read.file('header_formd_sic', from = from, test = test) }
write_header_formd_sic = function( df, from = '', test = F ) { write.file(df, 'header_formd_sic', from = from, test = test) }
read_header_formd_country_state = function( from = '', test = F ) { read.file('header_formd_country_state', from = from, test = test) }
write_header_formd_country_state = function( df, from = '', test = F ) { write.file(df, 'header_formd_country_state', from = from, test = test) }

read_header_formd_former_names = function( from = '', test = F ) { read.file('header_formd_former_names', from = from, test = test) }
write_header_formd_former_names = function( df, from = '', test = F ) { write.file(df, 'header_formd_former_names', from = from, test = test) }
read_formd_with_header = function( from = '', test = F ) { read.file('formd_with_header', from = from, test = test) }
write_formd_with_header = function( df, from = '', test = F ) { write.file(df, 'formd_with_header', from = from, test = test) }
read_formd_with_header_currentized = function( from = '', test = F ) { read.file('formd_with_header_currentized', from = from, test = test) }
write_formd_with_header_currentized = function( df, from = '', test = F ) { write.file(df, 'formd_with_header_currentized', from = from, test = test) }
read_formd_with_old_data = function( from = '', test = F ) { read.file('formd_with_old_data', from = from, test = test) }
write_formd_with_old_data = function( df, from = '', test = F ) { write.file(df, 'formd_with_old_data', from = from, test = test) }
read_formd_companies = function( from = '', test = F ) { read.file('formd_companies', from = from, test = test) }
write_formd_companies = function( df, from = '', test = F ) { write.file(df, 'formd_companies', from = from, test = test) }
read_companies_formd_old_data = function( from = '', test = F ) { read.file('companies_formd_old_data', from = from, test = test) }
write_companies_formd_old_data = function( df, from = '', test = F ) { write.file(df, 'companies_formd_old_data', from = from, test = test) }
read_companies_formd_new_data = function( from = '', test = F ) { read.file('companies_formd_new_data', from = from, test = test) }
write_companies_formd_new_data = function( df, from = '', test = F ) { write.file(df, 'companies_formd_new_data', from = from, test = test) }
read_formd_with_header_singularized = function( from = '', test = F ) { read.file('formd_with_header_singularized', from = from, test = test) }
write_formd_with_header_singularized = function( df, from = '', test = F ) { write.file(df, 'formd_with_header_singularized', from = from, test = test) }

read_most_recent_filings = function( from = '', test = F ) { read.file('most_recent_filings', from = from, test = test) }
write_most_recent_filings = function( df, from = '', test = F ) { write.file(df, 'most_recent_filings', from = from, test = test) }
read_most_recent_filings_raw = function( from = '', test = F ) { read.file('most_recent_filings_raw', from = from, test = test) }
write_most_recent_filings_raw = function( df, from = '', test = F ) { write.file(df, 'most_recent_filings_raw', from = from, test = test) }
read_joined_formd2 = function( from = '', test = F ) { read.file('joined_formd2', from = from, test = test) }
write_joined_formd2 = function( df, from = '', test = F ) { write.file(df, 'joined_formd2', from = from, test = test) }
read_joined_formd = function( from = '', test = F ) {
	dt = read.file('joined_formd', from = from, test = test)
	dt$primaryIssuer_cik = as.character(dt$primaryIssuer_cik)
	dt$offeringData_typeOfFiling_newOrAmendment_isAmendment = as.logical(dt$offeringData_typeOfFiling_newOrAmendment_isAmendment)
	setkey(dt,filename)
	dt
}

write_joined_formd = function( df, from = '', test = F ) {
	write.file(df, 'joined_formd', from = from, test = test)
}

read_joined_formd_most_recent = function( from = '', test = F ) { read.file('joined_formd_most_recent', from = from, test = test) }
write_joined_formd_most_recent = function( df, from = '', test = F ) { write.file(df, 'joined_formd_most_recent', from = from, test = test) }

read_mappings_amendment_to_overwritten = function( from = '', test = F ) { read.file('mappings_amendment_to_overwritten', from = from, test = test) }
write_mappings_amendment_to_overwritten = function( df, from = '', test = F ) { write.file(df, 'mappings_amendment_to_overwritten', from = from, test = test) }

.path_array_company_0000_qtr0 = function(year, quarter, file_ext = 'csv') { 
	sprintf_year_quarter( get_filename('company_0000_qtr0', file_ext = file_ext), year, quarter)
}
path_array_company_0000_qtr0 = partial(.path_array_company_0000_qtr0, file_ext = 'csv')
path_array_company_0000_qtr0_zip = partial(.path_array_company_0000_qtr0, file_ext = 'zip')
path_array_company_0000_qtr0_idx = partial(.path_array_company_0000_qtr0, file_ext = 'idx')
read_array_company_0000_qtr0 = function( year, quarter, from = '', test = F ) { read_file_with_path(path_array_company_0000_qtr0(year, quarter), 'company_0000_qtr0') }
write_array_company_0000_qtr0 = function( df, year, quarter, from = '', test = F, ...) { write_file_with_path(df, path_array_company_0000_qtr0(year, quarter)) }

path_array_company_0000_qtr0_clean = partial(sprintf_year_quarter, fmt = get_filename('company_0000_qtr0_clean'))
read_array_company_0000_qtr0_clean = function( year, quarter, from = '', test = F ) { read_file_with_path(path_array_company_0000_qtr0_clean(year, quarter), 'company_0000_qtr0_clean') }
write_array_company_0000_qtr0_clean = function( df, year, quarter, from = '', test = F, ...) { write_file_with_path(df, path_array_company_0000_qtr0_clean(year, quarter)) }

path_array_exchange_listing_x = partial(sprintf1, get_filename('exchange_listing_x'))
read_array_exchange_listing_x = function( exchange_name, from = '', test = F, ...) { read_file_with_path(path_array_exchange_listing_x(exchange_name), 'exchange_listing_x', read_fun = .read.dt, ...) }
write_array_exchange_listing_x = function( df, exchange_name, from = '', test = F, ...) { write_file_with_path(df, path_array_exchange_listing_x(exchange_name)) }

path_array_preclean_exchange_listing_x = function(exchange_name) { 
	sprintf(get_filename('preclean_exchange_listing_x'), exchange_name)
}
read_array_preclean_exchange_listing_x = function( exchange_name, from = '', test = F, ...) { read_file_with_path(path_array_preclean_exchange_listing_x(exchange_name), 'preclean_exchange_listing_x', ...) }
write_array_preclean_exchange_listing_x = function( df, exchange_name, from = '', test = F, ...) { write_file_with_path(df, path_array_preclean_exchange_listing_x(exchange_name)) }

path_array_clean_exchange_listing_x = function(exchange_name) { 
	sprintf(get_filename('clean_exchange_listing_x'), exchange_name)
}
read_array_clean_exchange_listing_x = function( exchange_name, from = '', test = F, ...) { read_file_with_path(path_array_clean_exchange_listing_x(exchange_name), 'clean_exchange_listing_x', ...) }
write_array_clean_exchange_listing_x = function( df, exchange_name, from = '', test = F, ...) { write_file_with_path(df, path_array_clean_exchange_listing_x(exchange_name)) }

path_array_exchanges_from_10k_filings_by_strategy_x = function(strategy) { 
	dir_filings_10k_result() %+% strategy %+% '/exchanges.csv'
	#sprintf(get_filename('exchanges_from_10k_filings_by_strategy_x'), strategy)
}
read_array_exchanges_from_10k_filings_by_strategy_x = function( strategy, from = '', test = F, ...) { 
	read_file_with_path(path_array_exchanges_from_10k_filings_by_strategy_x(strategy), 'exchanges_from_10k_filings_by_strategy_x', header=F, ...) 
}
write_array_exchanges_from_10k_filings_by_strategy_x = function( df, strategy, from = '', test = F, ...) { 
	write_file_with_path(df, path_array_exchanges_from_10k_filings_by_strategy_x(strategy)) 
}

path_array_companies_in_class_x = function(companies_in_class_x) { 
	sprintf(get_filename('companies_in_class_x'), companies_in_class_x)
}
read_array_companies_in_class_x = function( companies_in_class_x, from = '', test = F, ...) { read_file_with_path(path_array_companies_in_class_x(companies_in_class_x), 'companies_in_class_x', ...) }
write_array_companies_in_class_x = function( df, companies_in_class_x, from = '', test = F, ...) { write_file_with_path(df, path_array_companies_in_class_x(companies_in_class_x)) }
read_list_companies_in_class_x = function() {
	classes = names(get.rules()) %>%
		c('unclassified')
	result = llply(classes, read_array_companies_in_class_x)
	names(result) = classes
	result
}

path_array_formd_cleaned_subarray_x2 = function(file_title) { 
	sprintf(get_filename('formd_cleaned_subarray_x2'), file_title)
}
file_exists_formd_cleaned_subarray_x2 = compose(file.exists, path_array_formd_cleaned_subarray_x2)
read_array_formd_cleaned_subarray_x2 = function( file_title, from = '', test = F, ...) { read_file_with_path(path_array_formd_cleaned_subarray_x2(file_title), 'formd_cleaned_subarray_x2', read_fun = .read.dt, ...) }
write_array_formd_cleaned_subarray_x2 = function( df, file_title, from = '', test = F, ...) { write_file_with_path(df, path_array_formd_cleaned_subarray_x2(file_title)) }
read_list_formd_cleaned_subarray_x2 = function() {
	file_titles = get_formd_subarray_titles_with_double()
	result = llply(file_titles, read_array_formd_cleaned_subarray_x2)
	names(result) = file_titles
	result
}
write_list_formd_cleaned_subarray_x2 = function(df_list) {
	for (i in seq_along(df_list)) {
		title = names(df_list)[[i]]
		write_array_formd_cleaned_subarray_x2(df_list[[i]], title)
	}
}

path_array_formd_cleaned_subarray_x = function(file_title) { 
	sprintf(get_filename('formd_cleaned_subarray_x'), file_title)
}
file_exists_formd_cleaned_subarray_x = compose(file.exists, path_array_formd_cleaned_subarray_x)
read_array_formd_cleaned_subarray_x = function( file_title, from = '', test = F, ...) { read_file_with_path(path_array_formd_cleaned_subarray_x(file_title), 'formd_cleaned_subarray_x', ...) }
write_array_formd_cleaned_subarray_x = function( df, file_title, from = '', test = F, ...) { write_file_with_path(df, path_array_formd_cleaned_subarray_x(file_title)) }
read_list_formd_cleaned_subarray_x = function() {
	file_titles = get_formd_subarray_titles()
	result = llply(file_titles, read_array_formd_cleaned_subarray_x)
	names(result) = file_titles
	result
}

path_array_formd_most_recent_subarray_x = function(file_title) { 
	sprintf(get_filename('formd_most_recent_subarray_x'), file_title)
}
read_array_formd_most_recent_subarray_x = function( file_title, from = '', test = F, ...) { read_file_with_path(path_array_formd_most_recent_subarray_x(file_title), 'formd_most_recent_subarray_x', read_fun = .read.dt, ...) }
write_array_formd_most_recent_subarray_x = function( df, file_title, from = '', test = F, ...) { write_file_with_path(df, path_array_formd_most_recent_subarray_x(file_title)) }
read_list_formd_most_recent_subarray_x = function() {
	file_titles = get_formd_subarray_titles_with_double()
	result = llply(file_titles, read_array_formd_most_recent_subarray_x)
	names(result) = file_titles
	result
}

path_broken_filings = function() { get_filename('broken_filings') }
read_broken_filings = function( from = '', test = F ) { read.file('broken_filings', from = from, test = test) }
write_broken_filings = function( df, from = '', test = F ) { write.file(df, 'broken_filings', from = from, test = test) }
# formd_subarray data

read_merged_lists = function( from = '', test = F ) {
	# @todo: why does read.file not work?
	#dt = read.file('merged_lists', from = from, test = test)
	dt = read.csv('exchanges/merged_lists.csv',header=T, row.names=NULL, stringsAsFactor=F)
	dt$full_symbol = dt$Symbol
	dt$symbol = dt$symbol4
	dt$symbol4 = NULL
	dt$Symbol = NULL
	dt = data.table(dt)
	# todo: correct merged_lists before writing it
	setkey(dt,symbol)
	dt
}

write_merged_lists = function( df , from = '', test = F) {
	write.file(df, 'merged_lists', from = from, test = test)
}

read_us_public = function( from = '', test = F ) { dt = read_array_companies_in_class_x('us_public') }

read_header_data = function( from = '', test = F ) { dt = read.file('header_data', from = from, test = test) }
write_header_data = function( df, from = '', test = F ) { write.file(df, 'header_data', from = from, test = test) }

read_header_data_former_names = function( from = '', test = F ) { read.file('header_data_former_names', from = from, test = test) }
write_header_data_former_names = function( df, from = '', test = F ) { write.file(df, 'header_data_former_names', from = from, test = test) }

read_header_data_unduplicated_cikm = function( from = '', test = F ) { dt = read.file('header_data_unduplicated_cikm', from = from, test = test) }
write_header_data_unduplicated_cikm = function( df, from = '', test = F ) { write.file(df, 'header_data_unduplicated_cikm', from = from, test = test) }
read_header_data_per_cik = function( from = '', test = F ) { dt = read.file('header_data_per_cik', from = from, test = test) }
write_header_data_per_cik = function( df, from = '', test = F ) { write.file(df, 'header_data_per_cik', from = from, test = test) }
read_header_data_singularized = function( from = '', test = F ) { dt = read.file('header_data_singularized', from = from, test = test) }
write_header_data_singularized = function( df, from = '', test = F ) { write.file(df, 'header_data_singularized', from = from, test = test) }


read_mapping_url_filename = function( from = '', test = F ) { dt = read.file('mapping_url_filename', from = from, test = test) }
write_mapping_url_filename = function(df, from = '', test = F) { write.file(df, 'mapping_url_filename', from = from, test = test) }
read_mapping_10k_filename_interactive_xbrl = function( from = '', test = F ) { dt = read.file('mapping_10k_filename_interactive_xbrl', from = from, test = test) }
write_mapping_10k_filename_interactive_xbrl = function(df, from = '', test = F) { write.file(df, 'mapping_10k_filename_interactive_xbrl', from = from, test = test) }

read_company_exchange_listing_data = function( from = '', test = F ) {
	dt = read.file('company_exchange_listing_data', from = from, test = test)
	setkey(dt,cik)
	dt
}

write_company_exchange_listing_data = function( df , from = '', test = F) {
	write.file(df, 'company_exchange_listing_data', from = from, test = test)
}

read_company_exchange = function( from = '', test = F ) {
	dt = read.file('company_exchange', from = from, test = test)
	setkey(dt,cik)
	dt
}

write_company_exchange = function( df , from = '', test = F) {
	write.file(df, 'company_exchange', from = from, test = test)
}

read_company_symbol = function( from = '', test = F ) {
	dt = read.file('company_symbol', from = from, test = test)
}

write_company_symbol = function( df , from = '', test = F) {
	write.file(df, 'company_symbol', from = from, test = test)
}

read_company_filings_last_essential = function( from = '', test = F ) {
	dt = read.file('company_filings_last_essential', from = from)
	setkey(dt,cik)
	dt
}

write_company_filings_last_essential = function( df , from = '', test = F) {
	write.file(df, 'company_filings_last_essential', from = from, test = test)
}

read_company_classes = function( from = '' , test = F ) {
	dt = read.file('company_classes', from = from, test = test )
	setkey(dt,cik)
	dt
}

write_company_classes = function( df , from = '', test = F) {
	write.file(df, 'company_classes', from = from, test = test)
}

read_companies = function( from = '' , test = F ) {
	dt = read.file('companies', from = from, test = test )
	setkey(dt,cik)
	dt
}

write_companies = function( df , from = '', test = F) {
	write.file(df, 'companies', from = from, test = test)
}

read_associated_amendments = function( from = '' , test = F) {
	dt = read.file('associated_amendments', from = from, test = test)
}

write_associated_amendments = function( df , from = '', test = F) {
	write.file(df, 'associated_amendments', from = from, test = test)
}

read_merged = function( from = '', ext = '' , test = F, fun_read_file = read.file ) {
	dt = fun_read_file('merged', ext, from = from, test = test)
	dt$date = as.Date(dt$date)
	dt$cik = as.character(dt$cik)
	dt
}

write_merged = function( df , from = '', test = F) {
	write.file(df, 'merged', from = from, test = test)
}

read_merged_classified_optimized = function( from = '' , ext = '' , test = F) {
	dt = read.file('merged_classified_optimized', ext = ext, from = from, test = test)
	dt$date = as.Date(dt$date)
	dt$cik = as.character(dt$cik)
	#setkey(dt, form,date,cik )
	#setkey(dt, form, cik)
	dt
}

write_merged_classified_optimized = function( df , from = '', test = F) {
	write.file(df, 'merged_classified_optimized', from = from, test = test)
}

read_merged_classified = function( from = '' , ext = '' , test = F) {
	dt = read.file('merged_classified', ext = ext, from = from, test = test)
	dt$date = as.Date(dt$date)
	dt$cik = as.character(dt$cik)
	dt
}

write_merged_classified = function( df , from = '', test = F) {
	write.file(df, 'merged_classified', from = from, test = test)
}

read_classified_with_header = function( from = '' , ext = '' , test = F) { dt = read.file('classified_with_header', ext = ext, from = from, test = test) }

write_classified_with_header = function( df , from = '', test = F) {
	write.file(df, 'classified_with_header', from = from, test = test)
}

read_filings_filtered_with_header = function( from = '' , ext = '' , test = F) { dt = read.file('filings_filtered_with_header', ext = ext, from = from, test = test) }

write_filings_filtered_with_header = function( df , from = '', test = F) {
	write.file(df, 'filings_filtered_with_header', from = from, test = test)
}

read_filings_filtered_with_header_sic = function( from = '' , ext = '' , test = F) { dt = read.file('filings_filtered_with_header_sic', ext = ext, from = from, test = test) }
write_filings_filtered_with_header_sic = function( df , from = '', test = F) {
	write.file(df, 'filings_filtered_with_header_sic', from = from, test = test)
}

read_filings_with_decoded_country_state = function( from = '' , ext = '' , test = F) { read.file('filings_with_decoded_country_state', ext = ext, from = from, test = test) }
write_filings_with_decoded_country_state = function( df , from = '', test = F) {
	write.file(df, 'filings_with_decoded_country_state', from = from, test = test)
}

read_merged_filtered = function( from = '' , ext = '' , test = F) {
	dt = read.file('merged_filtered', ext = ext, from = from, test = test)
	dt$date = as.Date(dt$date)
	setkey(dt,form,cik)
	dt
}

write_merged_filtered = function( df , from = '', test = F) {
	write.file(df, 'merged_filtered', from = from, test = test)
}

write_contingency_exchange_lists = function( df, from = '' , test = F) { write.file(df, 'contingency_exchange_lists', from = from, use_table = T, test = test) }
write_contingency_table_for_sec_filings = function( df, from = '' , test = F) { write.file(df, 'contingency_table_for_sec_filings', from = from, use_table = T, test = test) }
write_contingency_table_for_exchange_listings = function( df, from = '' , test = F) { write.file(df, 'contingency_table_for_exchange_listings', from = from, use_table = T, test = test) }
read_compustat_exchanges_real = function( from = '', test = F ) { read_file_real('compustat_exchanges') }
read_compustat_exchanges = function( from = '', test = F ) { read.file('compustat_exchanges', from = from, test = test) }
write_compustat_exchanges = function( df, from = '' , test = F) { write.file(df, 'compustat_exchanges', from = from, test = test) }
read_compustat_standardized_names = function( from = '', test = F ) { read.file('compustat_standardized_names', from = from, test = test) }
write_compustat_standardized_names = function( df, from = '' , test = F) { write.file(df, 'compustat_standardized_names', from = from, test = test) }
read_joined_compustat_formd = function( from = '', test = F ) { read.file('joined_compustat_formd', from = from, test = test) }
write_joined_compustat_formd = function( df, from = '' , test = F) { write.file(df, 'joined_compustat_formd', from = from, test = test) }
read_joined_compustat_formd_differing_irs = function( from = '', test = F ) { read.file('joined_compustat_formd_differing_irs', from = from, test = test) }
write_joined_compustat_formd_differing_irs = function( df, from = '' , test = F) { write.file(df, 'joined_compustat_formd_differing_irs', from = from, test = test) }
read_joined_compustat_us_public = function( from = '', test = F ) { read.file('joined_compustat_us_public', from = from, test = test) }
write_joined_compustat_us_public = function( df, from = '' , test = F) { write.file(df, 'joined_compustat_us_public', from = from, test = test) }
read_joined_compustat_us_public_differing_irs = function( from = '', test = F ) { read.file('joined_compustat_us_public_differing_irs', from = from, test = test) }
write_joined_compustat_us_public_differing_irs = function( df, from = '' , test = F) { write.file(df, 'joined_compustat_us_public_differing_irs', from = from, test = test) }
read_merged_compustat_exchanges_with_exchange_listings = function( from = '', test = F ) { 
	ex = read.file('merged_compustat_exchanges_with_exchange_listings', from = from, test = test) 
	ex$exchg = as.character(ex$exchg)
	return(ex)
}
write_merged_compustat_exchanges_with_exchange_listings = function( df, from = '' , test = F) { write.file(df, 'merged_compustat_exchanges_with_exchange_listings', from = from, test = test) }
read_merged_compustat_exchanges_with_sec_filings = function( from = '', test = F ) { 
	ex = read.file('merged_compustat_exchanges_with_sec_filings', from = from, test = test) 
	ex$exchg = as.character(ex$exchg)
	return(ex)
}
write_merged_compustat_exchanges_with_sec_filings = function( df, from = '' , test = F) { write.file(df, 'merged_compustat_exchanges_with_sec_filings', from = from, test = test) }

path_exchanges_from_10k_filings = function() dir_filings_10k_result() %+% 'exchanges_from_10k_filings.csv'
read_exchanges_from_10k_filings = function( from = '', test = F ) { 
	#r = read.file('exchanges_from_10k_filings', from = from, test = test) 
	r = read_file_with_path(path_exchanges_from_10k_filings(), 'exchanges_from_10k_filings')
	r[complete.cases(r)]
}
write_exchanges_from_10k_filings = function( df, from = '' , test = F) { 
	write_file_with_path(df, path_exchanges_from_10k_filings(), test = test) 
}
read_exchanges_from_10k_filings_cik = function( from = '', test = F ) { r = read.file('exchanges_from_10k_filings_cik', from = from, test = test) }
write_exchanges_from_10k_filings_cik = function( df, from = '' , test = F) { write.file(df, 'exchanges_from_10k_filings_cik', from = from, test = test) }

read_last_essential_filings = function( from = '', test = F ) { read.file('last_essential_filings', from = from, test = test) }
write_last_essential_filings = function( df, from = '' , test = F) { write.file(df, 'last_essential_filings', from = from, test = test) }
read_last_essential_filings_raw = function( from = '', test = F ) { read.file('last_essential_filings_raw', from = from, test = test) }
write_last_essential_filings_raw = function( df, from = '' , test = F) { write.file(df, 'last_essential_filings_raw', from = from, test = test) }
read_last_essential_filings_x17 = function( from = '', test = F ) { read.file('last_essential_filings_x17', from = from, test = test) }
write_last_essential_filings_x17 = function( df, from = '' , test = F) { write.file(df, 'last_essential_filings_x17', from = from, test = test) }
read_most_recent_with_lef = function( from = '', test = F ) { read.file('most_recent_with_lef', from = from, test = test) }
write_most_recent_with_lef = function( df, from = '' , test = F) { write.file(df, 'most_recent_with_lef', from = from, test = test) }
read_most_recent_with_lef_raw = function( from = '', test = F ) { read.file('most_recent_with_lef_raw', from = from, test = test) }
write_most_recent_with_lef_raw = function( df, from = '' , test = F) { write.file(df, 'most_recent_with_lef_raw', from = from, test = test) }

read_sic_6189_companies = function( from = '', test = F ) { read.file('sic_6189_companies', from = from, test = test) }
write_sic_6189_companies = function( df, from = '' , test = F) { write.file(df, 'sic_6189_companies', from = from, test = test) }
read_sic_6221_companies = function( from = '', test = F ) { read.file('sic_6221_companies', from = from, test = test) }
write_sic_6221_companies = function( df, from = '' , test = F) { write.file(df, 'sic_6221_companies', from = from, test = test) }
read_company_symbol_from_exchange_listing_data = function( from = '', test = F ) { read.file('company_symbol_from_exchange_listing_data', from = from, test = test) }
write_company_symbol_from_exchange_listing_data = function( df, from = '' , test = F) { write.file(df, 'company_symbol_from_exchange_listing_data', from = from, test = test) }

read_mapping_exchanges_of_compustat_and_exchange_listings = function( from = '', test = F ) { read.file('mapping_exchanges_of_compustat_and_exchange_listings', from = from, test = test) }
write_mapping_exchanges_of_compustat_and_exchange_listings = function( df, from = '' , test = F) { write.file(df, 'mapping_exchanges_of_compustat_and_exchange_listings', from = from, test = test) }
read_mapping_exchanges_of_compustat_and_sec_filings = function( from = '', test = F ) { read.file('mapping_exchanges_of_compustat_and_sec_filings', from = from, test = test) }
write_mapping_exchanges_of_compustat_and_sec_filings = function( df, from = '' , test = F) { write.file(df, 'mapping_exchanges_of_compustat_and_sec_filings', from = from, test = test) }
read_outliers_compustat_and_exchange_listings = function( from = '', test = F ) { read.file('outliers_compustat_and_exchange_listings', from = from, test = test) }
write_outliers_compustat_and_exchange_listings = function( df, from = '' , test = F) { write.file(df, 'outliers_compustat_and_exchange_listings', from = from, test = test) }
read_outliers_compustat_and_sec_filings = function( from = '', test = F ) { read.file('outliers_compustat_and_sec_filings', from = from, test = test) }
write_outliers_compustat_and_sec_filings = function( df, from = '' , test = F) { write.file(df, 'outliers_compustat_and_sec_filings', from = from, test = test) }
read_debug_outliers_compustat_and_sec_filings = function( from = '', test = F ) { read.file('debug_outliers_compustat_and_sec_filings', from = from, test = test) }
write_debug_outliers_compustat_and_sec_filings = function( df, from = '' , test = F) { write.file(df, 'debug_outliers_compustat_and_sec_filings', from = from, test = test) }

read_symbols = function( from = '', test = F ) { read.file('symbols', from = from, test = test) }
write_symbols = function( df, from = '' , test = F) { write.file(df, 'symbols', from = from, test = test) }
read_ciks = function( from = '', test = F ) { read.file('ciks', from = from, test = test) }
write_ciks = function( df, from = '' , test = F) { write.file(df, 'ciks', from = from, test = test) }
read_url_list_10k = function( from = '', test = F ) { read.file('url_list_10k', from = from, test = test) }
write_url_list_10k = function( df, from = '' , test = F) { write.file(df, 'url_list_10k', from = from, test = test) }
read_file_list_source = function( from = '', test = F ) { read.file('file_list_source', from = from, test = test) }
write_file_list_source = function( df, from = '' , test = F) { write.file(df, 'file_list_source', from = from, use_table = T, row.names=F, col.names=F, quote=F, test = test) }
read_file_list_source_10k = function( from = '', test = F ) { read_file_real('file_list_source_10k', from = from, test = test, header=F) }
write_file_list_source_10k = function( df, from = '' , test = F) { write_file_real(df, 'file_list_source_10k', from = from, use_table = T, row.names=F, col.names=F, quote=F, test = test) }
read_file_list_source_formd = function( from = '', test = F ) { read_file_real('file_list_source_formd', from = from, test = test, header=F)[[1]] }
write_file_list_source_formd = function( df, from = '' , test = F) { write_file_real(df, 'file_list_source_formd', from = from, use_table = T, row.names=F, col.names=F, quote=F, test = test) }

read_ciks_overwritten_of_companies_with_multiple_cik = function( from = '', test = F ) { read.file('ciks_overwritten_of_companies_with_multiple_cik', from = from, test = test) }
write_ciks_overwritten_of_companies_with_multiple_cik = function( df, from = '' , test = F) { write.file(df, 'ciks_overwritten_of_companies_with_multiple_cik', from = from, test = test) }
read_companies_with_multiple_cik = function( from = '', test = F ) { read.file('companies_with_multiple_cik', from = from, test = test) }
write_companies_with_multiple_cik = function( df, from = '' , test = F) { write.file(df, 'companies_with_multiple_cik', from = from, test = test) }
read_companies_with_multiple_names = function( from = '', test = F ) { read.file('companies_with_multiple_names', from = from, test = test) }
write_companies_with_multiple_names = function( df, from = '' , test = F) { write.file(df, 'companies_with_multiple_names', from = from, test = test) }
read_mappings_cik_name = function( from = '', test = F ) { read.file('mappings_cik_name', from = from, test = test) }
write_mappings_cik_name = function( df, from = '' , test = F) { write.file(df, 'mappings_cik_name', from = from, test = test) }
read_mappings_cik_current_to_former = function( from = '', test = F ) { read.file('mappings_cik_current_to_former', from = from, test = test) }
write_mappings_cik_current_to_former = function( df, from = '' , test = F) { write.file(df, 'mappings_cik_current_to_former', from = from, test = test) }
read_test_name_matching = function( from = '', test = T ) { read.file('test_name_matching', from = from, test = test) }
write_test_name_matching = function( df, from = '' , test = F) { write.file(df, 'test_name_matching', from = from, test = test) }
read_header_formd_simple = function( from = '', test = T ) { read.file('header_formd_simple', from = from, test = test) }
write_header_formd_simple = function( df, from = '', test = T ) { write.file(df, 'header_formd_simple', from = from, test = test) }
read_joined_formd_filtered_simple = function( from = '', test = T ) { read.file('joined_formd_filtered_simple', from = from, test = test) }
write_joined_formd_filtered_simple = function( df, from = '', test = T) { write.file(df, 'joined_formd_filtered_simple', from = from, test = test) }
read_joined_formd_filtered_currentized_simple = function( from = '', test = T ) { read.file('joined_formd_filtered_currentized_simple', from = from, test = test) }
write_joined_formd_filtered_currentized_simple = function( df, from = '', test = T) { write.file(df, 'joined_formd_filtered_currentized_simple', from = from, test = test) }
read_companies_singularized = function( from = '', test = F ) { read.file('companies_singularized', from = from, test = test) }
write_companies_singularized = function( df, from = '', test = F  ) { write.file(df, 'companies_singularized', from = from, test = test) }
read_mapping_current_cik_filename = function( from = '', test = F ) { read.file('mapping_current_cik_filename', from = from, test = test) }
write_mapping_current_cik_filename = function( df, from = '', test = F ) { write.file(df, 'mapping_current_cik_filename', from = from, test = test) }

read_compustat = function( from = '', test = F ) { read.file('compustat', from = from, test = test) }
write_compustat = function( df, from = '', test = F ) { write.file(df, 'compustat', from = from, test = test) }
read_country_state_codes = function( from = '', test = F ) { read.file('country_state_codes', from = from, test = test) }
write_country_state_codes = function( df, from = '', test = F ) { write.file(df, 'country_state_codes', from = from, test = test) }
read_f5500 = function( from = '', test = F ) { read_file_real('f5500', from = from, test = test) }
write_f5500 = function( df, from = '', test = F ) { write.file(df, 'f5500', from = from, test = test) }
read_f5500_standardized_names = function( from = '', test = F ) { read.file('f5500_standardized_names', from = from, test = test) }
write_f5500_standardized_names = function( df, from = '', test = F ) { write.file(df, 'f5500_standardized_names', from = from, test = test) }
read_joined_f5500_formd = function( from = '', test = F, fun_read_file = read.file ) { 
	fun_read_file('joined_f5500_formd', from = from, test = test)
}
write_joined_f5500_formd = function( df, from = '', test = F ) { write.file(df, 'joined_f5500_formd', from = from, test = test) }
read_joined_f5500_formd_differing_irs = function( from = '', test = F ) { read.file('joined_f5500_formd_differing_irs', from = from, test = test) }
write_joined_f5500_formd_differing_irs = function( df, from = '', test = F ) { write.file(df, 'joined_f5500_formd_differing_irs', from = from, test = test) }
read_joined_f5500_formd_aggregated_variables = function( from = '', test = F ) { read.file('joined_f5500_formd_aggregated_variables', from = from, test = test) }
write_joined_f5500_formd_aggregated_variables = function( df, from = '', test = F ) { write.file(df, 'joined_f5500_formd_aggregated_variables', from = from, test = test) }
read_joined_f5500_compustat = function( from = '', test = F ) { read.file('joined_f5500_compustat', from = from, test = test) }
write_joined_f5500_compustat = function( df, from = '', test = F ) { write.file(df, 'joined_f5500_compustat', from = from, test = test) }
read_joined_f5500_compustat_differing_irs = function( from = '', test = F ) { read.file('joined_f5500_compustat_differing_irs', from = from, test = test) }
write_joined_f5500_compustat_differing_irs = function( df, from = '', test = F ) { write.file(df, 'joined_f5500_compustat_differing_irs', from = from, test = test) }
read_mapping_cik_symbol = function( from = '', test = F ) { read.file('mapping_cik_symbol', from = from, test = test) }
write_mapping_cik_symbol = function( df, from = '', test = F ) { write.file(df, 'mapping_cik_symbol', from = from, test = test) }
read_duplicated_symbol_mapping_cik_symbol = function( from = '', test = F ) { read.file('duplicated_symbol_mapping_cik_symbol', from = from, test = test) }
write_duplicated_symbol_mapping_cik_symbol = function( df, from = '', test = F ) { write.file(df, 'duplicated_symbol_mapping_cik_symbol', from = from, test = test) }
read_mapping_cik_filename = function( from = '', test = F ) { read.file('mapping_cik_filename', from = from, test = test) }
write_mapping_cik_filename = function( df, from = '', test = F ) { write.file(df, 'mapping_cik_filename', from = from, test = test) }
read_mapping_cikm_filename = function( from = '', test = F ) { read.file('mapping_cikm_filename', from = from, test = test) }
write_mapping_cikm_filename = function( df, from = '', test = F ) { write.file(df, 'mapping_cikm_filename', from = from, test = test) }
# read_data_flow_dependencies = function( from = '', test = F ) { read.file('data_flow_dependencies', from = from, test = test) }
# write_data_flow_dependencies = function( df, from = '', test = F ) { write.file(df, 'data_flow_dependencies', from = from, test = test) }
read_data_flow_dependencies = read_csv_fun('data_flow_dependencies')
write_data_flow_dependencies = write_csv_fun('data_flow_dependencies')
read_direct_deps = read_csv_fun('direct_deps')
write_direct_deps = write_csv_fun('direct_deps')
read_deep_deps = read_csv_fun('deep_deps')
write_deep_deps = write_csv_fun('deep_deps')
read_singular_xpaths = function( from = '', test = F ) { read.file('singular_xpaths', from = from, test = test) }
write_singular_xpaths = function( df, from = '', test = F ) { write.file(df, 'singular_xpaths', from = from, test = test) }
path_array_formd_subarray_xpaths_x = function(file_title) { 
	sprintf(get_filename('formd_subarray_xpaths_x'), file_title)
}
file_exists_formd_subarray_xpaths_x = compose(file.exists, path_array_formd_subarray_xpaths_x)
read_array_formd_subarray_xpaths_x = function( file_title, from = '', test = F, ...) { read_file_with_path(path_array_formd_subarray_xpaths_x(file_title), 'formd_subarray_xpaths_x', ...) }
write_array_formd_subarray_xpaths_x = function( df, file_title, from = '', test = F, ...) { write_file_with_path(df, path_array_formd_subarray_xpaths_x(file_title)) }
read_list_formd_subarray_xpaths_x = function() {
	file_titles = get_formd_subarray_titles()
	result = llply(file_titles, read_array_formd_subarray_xpaths_x)
	names(result) = file_titles
	result
}
write_list_formd_subarray_xpaths_x = function(xpath_list) {
	for (i in seq_along(xpath_list)) {
		title = names(xpath_list)[[i]]
		write_array_formd_subarray_xpaths_x(xpath_list[[i]], title)
	}
}

read_multiple_xpaths = function( from = '', test = F ) { read.file('multiple_xpaths', from = from, test = test) }
write_multiple_xpaths = function( df, from = '', test = F ) { write.file(df, 'multiple_xpaths', from = from, test = test) }
read_merged_fields = function( from = '', test = F ) { 
	path = get_filename('merged_fields', file_ext = '')
	read_file_with_path(path, 'merged_fields', read_fun = .readLines) 
}
write_merged_fields = function( df, from = '', test = F ) { 
	path = get_filename('merged_fields', file_ext = '')
	write_file_with_path(df, path) 
}
exists_merged_fields = function( from = '', test = F ) { 
	path = get_filename('merged_fields', file_ext = '')
	file.exists(path)
}
path_array_formd_subarray_fields_x = function(file_title) { 
	sprintf(get_filename('formd_subarray_fields_x', file_ext = 'fields'), file_title)
}
file_exists_formd_subarray_fields_x = compose(file.exists, path_array_formd_subarray_fields_x)
read_array_formd_subarray_fields_x = function( file_title, from = '', test = F, ...) { read_file_with_path(path_array_formd_subarray_fields_x(file_title), 'formd_subarray_fields_x', ..., read_fun = .readLines) }
write_array_formd_subarray_fields_x = function( df, file_title, from = '', test = F, ...) { write_file_with_path(df, path_array_formd_subarray_fields_x(file_title)) }
read_list_formd_subarray_fields_x = function() {
	file_titles = get_formd_subarray_titles()
	result = llply(file_titles, read_array_formd_subarray_fields_x)
	names(result) = file_titles
	result
}
exists_list_formd_subarray_fields_x = function() {
	get_formd_subarray_titles() %>%
		lapply(path_array_formd_subarray_fields_x) %>%
		lapply(file.exists) %>%
		as.logical %>%
		all
}

read_xbrl_list_all_10k_filings_raw = read_csv_fun('xbrl_list_all_10k_filings_raw')
write_xbrl_list_all_10k_filings_raw = write_csv_fun('xbrl_list_all_10k_filings_raw')
read_xbrl_list_all_10k_filings = read_csv_fun('xbrl_list_all_10k_filings')
write_xbrl_list_all_10k_filings = write_csv_fun('xbrl_list_all_10k_filings')
read_xbrl_list_all_10k_filings_with_sic = read_csv_fun('xbrl_list_all_10k_filings_with_sic')
write_xbrl_list_all_10k_filings_with_sic = write_csv_fun('xbrl_list_all_10k_filings_with_sic')

read_xbrl_list_broken_xml = read_csv_fun('xbrl_list_broken_xml')
write_xbrl_list_broken_xml = write_csv_fun('xbrl_list_broken_xml')

read_xbrl_list_missing_xbrl_10k = read_csv_fun('xbrl_list_missing_xbrl_10k')
write_xbrl_list_missing_xbrl_10k = write_csv_fun('xbrl_list_missing_xbrl_10k')

read_xbrl_list_valid_xml_10k = read_csv_fun('xbrl_list_valid_xml_10k')
write_xbrl_list_valid_xml_10k = write_csv_fun('xbrl_list_valid_xml_10k')

read_xbrl_contextref = read_csv_fun('xbrl_contextref')
write_xbrl_contextref = write_csv_fun('xbrl_contextref')

read_xbrl_list_missing_tags = read_array_fun('xbrl_list_missing_tags')
write_xbrl_list_missing_tags = write_array_fun('xbrl_list_missing_tags')

read_xbrl_list_missing_tags_sic = read_array_fun('xbrl_list_missing_tags_sic')
write_xbrl_list_missing_tags_sic = write_array_fun('xbrl_list_missing_tags_sic')

read_xbrl_list_verified_tag = read_array_fun('xbrl_list_verified_tag')
write_xbrl_list_verified_tag = write_array_fun('xbrl_list_verified_tag')

read_xbrl_list_valid_xml_10k_with_sic = read_csv_fun('xbrl_list_valid_xml_10k_with_sic')
write_xbrl_list_valid_xml_10k_with_sic = write_csv_fun('xbrl_list_valid_xml_10k_with_sic')
read_crunchbase_organizations = read_csv_fun('crunchbase_organizations')
write_crunchbase_organizations = write_csv_fun('crunchbase_organizations')
read_crunchbase_people = read_csv_fun('crunchbase_people')
write_crunchbase_people = write_csv_fun('crunchbase_people')
read_match_crunchbase_sec = read_csv_fun('match_crunchbase_sec')
write_match_crunchbase_sec = write_csv_fun('match_crunchbase_sec')
read_nomatch_crunchbase_sec = read_csv_fun('nomatch_crunchbase_sec')
write_nomatch_crunchbase_sec = write_csv_fun('nomatch_crunchbase_sec')
read_match_crunchbase_formd = read_csv_fun('match_crunchbase_formd')
write_match_crunchbase_formd = write_csv_fun('match_crunchbase_formd')
read_nomatch_crunchbase_formd = read_csv_fun('nomatch_crunchbase_formd')
write_nomatch_crunchbase_formd = write_csv_fun('nomatch_crunchbase_formd')
read_match_crunchbase_hds = read_csv_fun('match_crunchbase_hds')
write_match_crunchbase_hds = write_csv_fun('match_crunchbase_hds')
read_nomatch_crunchbase_hds = read_csv_fun('nomatch_crunchbase_hds')
write_nomatch_crunchbase_hds = write_csv_fun('nomatch_crunchbase_hds')

read_xbrl_matched_tags = read_array_fun('xbrl_matched_tags')
write_xbrl_matched_tags = write_array_fun('xbrl_matched_tags')

read_xbrl_matched_tags_with_sic = read_array_fun('xbrl_matched_tags_with_sic')
write_xbrl_matched_tags_with_sic = write_array_fun('xbrl_matched_tags_with_sic')

read_verification_xbrl_tags = read_array_fun('verification_xbrl_tags')
write_verification_xbrl_tags = write_array_fun('verification_xbrl_tags')
read_xbrl_analysis_missing_tags = read_array_fun('xbrl_analysis_missing_tags')
write_xbrl_analysis_missing_tags = write_array_fun('xbrl_analysis_missing_tags')

read_xbrl_data_all_x = read_array_fun('xbrl_data_all_x')
write_xbrl_data_all_x = write_array_fun('xbrl_data_all_x')
read_xbrl_data_x = read_array_fun('xbrl_data_x')
write_xbrl_data_x = write_array_fun('xbrl_data_x')
write_list_xbrl_data_x = write_list_fun('xbrl_data_x')
read_xbrl_data_revenue = partial(read_xbrl_data_x, arg="revenue")
write_xbrl_data_revenue = partial(write_xbrl_data_x, arg="revenue")
read_xbrl_data_net_income = partial(read_xbrl_data_x, arg="net_income")
write_xbrl_data_net_income = partial(write_xbrl_data_x, arg="net_income")

read_xbrl_data_x_decrypted_contextref_all = read_array_fun('xbrl_data_x_decrypted_contextref_all')
write_xbrl_data_x_decrypted_contextref_all = write_array_fun('xbrl_data_x_decrypted_contextref_all')
write_list_xbrl_data_x_decrypted_contextref_all = write_list_fun('xbrl_data_x_decrypted_contextref_all')
read_xbrl_data_x_decrypted_contextref = read_array_fun('xbrl_data_x_decrypted_contextref')
write_xbrl_data_x_decrypted_contextref = write_array_fun('xbrl_data_x_decrypted_contextref')
write_list_xbrl_data_x_decrypted_contextref = write_list_fun('xbrl_data_x_decrypted_contextref')
read_xbrl_data_x_annual_quarter_all = read_array_fun('xbrl_data_x_annual_quarter_all')
write_xbrl_data_x_annual_quarter_all = write_array_fun('xbrl_data_x_annual_quarter_all')
write_list_xbrl_data_x_annual_quarter_all = write_list_fun('xbrl_data_x_annual_quarter_all')
read_xbrl_data_x_annual_quarter = read_array_fun('xbrl_data_x_annual_quarter')
write_xbrl_data_x_annual_quarter = write_array_fun('xbrl_data_x_annual_quarter')
write_list_xbrl_data_x_annual_quarter = write_list_fun('xbrl_data_x_annual_quarter')
read_xbrl_data_x_dups_removed = read_array_fun('xbrl_data_x_dups_removed')
write_xbrl_data_x_dups_removed = write_array_fun('xbrl_data_x_dups_removed')
write_list_xbrl_data_x_dups_removed = write_list_fun('xbrl_data_x_dups_removed')

read_xbrl_data_x_hd = read_array_fun('xbrl_data_x_hd')
write_xbrl_data_x_hd = write_array_fun('xbrl_data_x_hd')
write_list_xbrl_data_x_hd = write_list_fun('xbrl_data_x_hd')

read_tags_in_xbrl_files = read_json_fun('tags_in_xbrl_files')
write_tags_in_xbrl_files = write_json_fun('tags_in_xbrl_files')
read_xbrl_matched_tags_all = read_array_fun('xbrl_matched_tags_all')
write_xbrl_matched_tags_all = write_array_fun('xbrl_matched_tags_all')

read_companies_header_data = read_csv_fun('companies_header_data')
write_companies_header_data = write_csv_fun('companies_header_data')
read_map_cik_cikm = read_csv_fun('map_cik_cikm')
write_map_cik_cikm = write_csv_fun('map_cik_cikm')

read_filings_x17 = read_csv_fun('filings_x17')
write_filings_x17 = write_csv_fun('filings_x17')
read_filings_sic6189 = read_csv_fun('filings_sic6189')
write_filings_sic6189 = write_csv_fun('filings_sic6189')
read_filings_usp10kq_raw = read_csv_fun('filings_usp10kq_raw')
write_filings_usp10kq_raw = write_csv_fun('filings_usp10kq_raw')
read_filings_usp10kq = read_csv_fun('filings_usp10kq')
write_filings_usp10kq = write_csv_fun('filings_usp10kq')
read_usp10kq_missing_xbrl = read_csv_fun('usp10kq_missing_xbrl')
write_usp10kq_missing_xbrl = write_csv_fun('usp10kq_missing_xbrl')

# temporary data file for keeping results of interactive computations 
read_results = read_csv_fun('results')
write_results = write_csv_fun('results')
read_verification_xbrl_tags_revenue_filled = read_csv_fun('verification_xbrl_tags_revenue_filled')
write_verification_xbrl_tags_revenue_filled = write_csv_fun('verification_xbrl_tags_revenue_filled')
read_verified_xbrl_tags = read_csv_fun('verified_xbrl_tags')
write_verified_xbrl_tags = write_csv_fun('verified_xbrl_tags')
read_tag_comparison_table_xbrl = read_csv_fun('tag_comparison_table_xbrl')
write_tag_comparison_table_xbrl = write_csv_fun('tag_comparison_table_xbrl')
read_tag_comparison_contingency_xbrl = read_csv_fun('tag_comparison_contingency_xbrl')
write_tag_comparison_contingency_xbrl = write_csv_fun('tag_comparison_contingency_xbrl')

read_study = function(name) {
	dir = get_directory('study')
	path = paste0(dir, name, '.csv')
	read_file_with_path(path, name)
}
write_study = function(df, name) {
	dir = get_directory('study')
	path = paste0(dir, name, '.csv')
	write_file_with_path(df, path)
}

dir_filings_root = function() { get_directory('filings_root') }
dir_filings_txt = function() { get_directory('filings_txt') }
dir_filings_zip = function() { get_directory('filings_zip') }
dir_filings_xbrl = function() { get_directory('filings_xbrl') }
dir_filings_header = function() { get_directory('filings_header') }
dir_filings_10k_sh_scripts_root = function() { get_directory_real('filings_10k_sh_scripts_root') }
dir_filings_10k_txt = dir_filings_txt
dir_filings_10k_data = function() { get_directory('filings_10k_data') }
dir_filings_10k_txt_cleaned = function() { dir_filings_10k_data() %+% 'txt_cleaned/' }
dir_filings_10k_result = function() { dir_filings_10k_data() %+% 'result/' }
dir_formd_txt = dir_filings_txt
dir_formd_data = function() { get_directory('formd_data') }
dir_formd_xml = function() { get_directory('formd_xml') }
dir_formd_tree = function() { get_directory('formd_tree') }
dir_formd_xml_added_new_items = function() { get_directory('formd_xml_added_new_items') }
dir_formd_json = function() { get_directory('formd_json') }
dir_formd_subarray_json = function() { get_directory('formd_subarray_json') }
dir_formd_sh_scripts_root = function() { get_directory_real('formd_sh_scripts_root') }
dir_compustat_data = function() { get_directory('compustat_data') }
dir_compustat_data_real = function() { get_directory_real('compustat_data') }
dir_filing_index = function() { get_directory('filing_index') }

log.progress = function( current.step, total.steps ) {
	is.multiple = function( current.step, step.size ) {
		remainder = current.step %% step.size
		remainder == 0 
	}
	decide.step.no = function( total.steps ) {
		if ( total.steps > 5000000 ) return( 50000 )
		if ( total.steps > 1000000 ) return( 10000 )
		if ( total.steps > 100000 ) return( 500 )
		if ( total.steps > 10000 ) return( 100 )
		return( 50 )
	}
	step.no = decide.step.no( total.steps )
	step.size = ceiling(total.steps/step.no)
	if ( is.multiple( current.step, step.size ) ) {
		print( paste0( format(Sys.time(), "%H:%M:%S"), " total.steps: ", total.steps, " current.step: ", current.step ))
	}
}

profile.fun = function(from = '', trial = '', write.csv = F, fun, ...) {
	extension = paste0( from, trial )
	fun_name = as.character( substitute(fun) )
	fun_name = gsub("\\.",'_',fun_name,perl=T)
	file_profiler_base = paste0( 'profiler/profiler_', fun_name )
	file_profiler = paste0( file_profiler_base, extension )
	csv_profiler = paste0(file_profiler, '.csv')
	Rprof(NULL)

	Rprof(file_profiler)

	result = fun( ... )

	Rprof(NULL)
	profile = summaryRprof(file_profiler)
	total = profile$by.total
	if ( write.csv ) {
		write.table(total, csv_profiler, sep=',', quote=F)
	}
	return(total)
}

log.error = function() {
}

moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

get_url_list = function(filings, target_dir = 'filings/') {
	url_list = filings[ , c('url','cik'), with = F]
	url_list$file_name = paste0( target_dir, url_list$cik, '.txt' )
	url_list = url_list[ , c(1,3,2), with=F]
}

single_zip_header_filings = function() {
	root_arg = sprintf("-r ../%s", dir_filings_root())
	arg = root_arg
	setwd('filings/')
	cmd = sprintf("./zip_filings.sh %s", arg) #@ < txt/formname.txt > zip/formname.zip
	system( cmd )
	setwd('..')
}

dir_cache_temp = function(path = 'temp') return(path)

uncache = function(path_arg = "") {
	path = file.path(dir_cache_temp(), path_arg)
	files = list.files(path = path, full.names = T, include.dirs = F) 
	print( sprintf("uncaching %s files", length(files)) )
	r = llply(files, readRDS)
	unlink(files)
	return(r)
}

cache_fun = function(fun) 
	function(...) 
		fun(...) %>% cache

cache = function(data, path_arg = "") {
	path = file.path(dir_cache_temp(), path_arg)
	saveRDS(data, cache_index(path = path))
}

cache_index = function(path = dir_cache_temp()) {
	dir.create(path, showWarnings = F, recursive=T)
	files = list.files(path = path, full.names = T, include.dirs = F) 
	filename = length(files) + 1
	file.path(path, filename %>% paste0('.rds'))
}

cache_index0 = function(path = dir_cache_temp()) {
	dir.create(path, showWarnings = F)
	if (! exists("counter_cache_index") ) {
		counter_cache_index <<- 0
	}
	counter_cache_index <<- counter_cache_index + 1
	file.path(path, counter_cache_index %>% paste0('.rds'))
}

llplyp = partial(llply, .progress = "text")

llplyc = function(.data, .fun, ...) {
	llplyp(.data, cache_fun(.fun), ...)
	result = uncache()
}

restore_cache = function(path_arg = "") {
	path = file.path(dir_cache_temp(), path_arg)
	files = list.files(path = path, full.names = T, include.dirs = F) 
	print( sprintf("uncaching %s files", length(files)) )
	r = llply(files, readRDS)
	return(r)
}

llplyc_nouncache = function(.data, .fun, ...) {
	llplyp(.data, cache_fun(.fun), ...)
	result = restore_cache()
}                 

setNamesNull = function(object, nm) {
	if ( is.blank(object) ) {
		object
	} else {
		setNames(object, nm) 
	}
}

combine.vectors.by.non.na = function(a, b) {
	a[is.na(a)]  <- b[is.na(a)]
	a
}

log_errors_llplyc = function(.fun) {
	function(filename) {
		return( 
			tryCatch(
				.fun(filename), 
				error = function(e) {
					# @todo
					if (! exists("counter_cache_index") ) {
						counter_cache_index <<- 0
					}
					write(x=counter_cache_index+1,file='error_logs.log',sep="\n", append=T)
				}
			)
		)
	}
}

# compare files in two directories and return difference
setdiff_files_in_dirs = function(dir1, dir2) {
	f1 = list.files(dir1) %>% basename_noext
	f2 = list.files(dir2) %>% basename_noext
	r = setdiff(f1, f2) %>%
		paste0(".txt")
}

zip_clean_txt_filings = function() {
	files = setdiff_files_in_dirs( dir_filings_txt(), dir_filings_zip() )
	llplyp(files, zip_remove_filing_script)
}

zip_remove_filing_script = function(filename) {
	zip_filename_base = filename %>% basename_noext %>% paste0(".zip")
	zip_filename = paste0(dir_filings_zip(), zip_filename_base)
	if (file.exists(zip_filename)) {
		unlink(filename %>% filepath_txt)
		return(filename)
	}

	filename_base = filename %>% basename_noext %>% paste0(".txt")
	arg = sprintf("-r %s -f %s", dir_filings_root(), filename_base)
	cmd = sprintf("./zip_remove_filing.sh %s", arg) #@ < txt/formname.txt > zip/formname.zip
	system( cmd )
	return(filename)
}

# @deprecated
basename_txt = function(filename) {
	filename %>% basename_ext("txt")
}

# @deprecated
filepath_txt = function(filename) {
	filename %>% path_filing("txt")
}

basename_ext = function(filename, ext) 
	filename %>% basename_noext %>% paste0('.', ext)

dir_filings_ext = function(ext)
	switch(ext,
		txt=dir_filings_txt(),
		xml=dir_filings_xbrl(),
		zip=dir_filings_zip(),
		header=dir_filings_header(),
		xbrl=dir_filings_xbrl()
	)

path_filing = function(filename, ext) 
	filename %>% 
		basename_ext(ext) %>%
		pre0(dir_filings_ext(ext))

make_trycatch = function(fun)
	function(x)
		tryCatch(
			fun(x),
			error = function(cond) x
		)

"%+%" = function(...) paste0(...)

lapply_stop = function(x, f, ...) {
	force(f)
	out = vector("list", length(x))
	for (i in seq_along(x)) {
		out[[i]] = f(x[[i]], ...)
		if ( ! is.blank(out[[i]]) ) return( out %>% unlist(recursive=F) )
	}
	out %>% unlist(recursive=F)
}


# test
generate_data = function(base_name, end) {
	1:end %>%
		sprintm("%03s") %>%
		pre0(base_name)
}

safe_extract = function(x, i) {
	if (is.blank(x))
		return(x)
	else
		return(x[i])
}

matches_all = function(x, patterns, ...) 
	lapply(patterns, grepl, x, ...) %>%
		lapply(any) %>%
		unlist %>%
		which 

matches_first = function(x, patterns, ...) 
	matches_all(x, patterns, ...) %>>%
		safe_extract(1)

process_cut_filing_fun = function(script) {
	function(filename) {
		filepath = filename %>% paste0(".txt") %>% pre0(dir_filings_header())
		if (file.exists(filepath)) return(filename)
		file_list_arg = sprintf( "-s %s", filename %>% basename_txt )
		root_arg = sprintf("-r %s", dir_filings_root())
		arg = paste0(file_list_arg, ' ', root_arg)
		cmd = sprintf( "./%s.sh %s", script, arg) #@ < txt/formname.txt > header/formname.txt
		system( cmd )
		return(filename)
	}
}

write_incr = function(data, filename, ...) {
	files = list.files(".") %>%
		grepv("^" %+% filename %+% "(\\d+)\\.csv") %>%
		str_match("^" %+% filename %+% "(\\d+)\\.csv") 
	idx = 1 + if (is.blank(files)) {
			0
		} else {
			files %>%
				extract(, 2) %>%
				as.numeric %>%
				sort(decreasing=T) %>%
				extract(1)
		} 
	filepath = filename %+% idx %+% ".csv"
		
	write_csv(data, filepath, ...)
}

list_files_xbrl = function() {
	setwd(dir_filings_xbrl())
	cmd = "find -type f"
	files = system(cmd, intern=T) %>% basename_noext
	setwd("../../..")
	return(files)
}

list_filings = function() {
	setwd(dir_filings_txt())
	cmd = "find -type f"
	files = system(cmd, intern=T) %>% basename_noext
	setwd("../../..")
	setwd(dir_filings_header())
	cmd = "find -type f"
	files = system(cmd, intern=T) %>% basename_noext %>%
		c(files)
	setwd("../../..")
	setwd(dir_filings_zip())
	cmd = "find -type f"
	files = system(cmd, intern=T) %>% basename_noext %>%
		c(files)
	setwd("../../..")
	files = unique(files)
	return(files)
}

get_empty_filings = function() {
	setwd(dir_filings_txt())
	cmd = "find -type f -size -400c"
	empty_files = system(cmd, intern=T) %>% basename_noext
	setwd("../../..")
	setwd(dir_filings_header())
	cmd = "find -type f -size -400c"
	empty_files = system(cmd, intern=T) %>% basename_noext %>%
		c(empty_files)
	setwd("../../..")
	setwd(dir_filings_zip())
	cmd = "find -type f -size -400c"
	empty_files = system(cmd, intern=T) %>% basename_noext %>%
		c(empty_files)
	setwd("../../..")
	return(empty_files)
}

unduplicate = function(df, cols) {
	dup = function(col, df) 
		duplicated(df[[col]])
	dups = lapply(cols, dup, df) %>%
		c(T)
	dupped = do.call('and', dups)
	df[! dupped]
}

# append (add,concat) a vector to each element of a list
clv = function(alist, vec)
	Map(c, alist, rep(list(vec), length(alist)))
# prepend a vector to each element of a list
cvl = function(vec, alist)
	Map(c, rep(list(vec), length(alist)), alist)

root_xml = function(filename)
	filename %>% path_filing("xml") %>% read_xml

root_xml_fun = function(filename, dir) {
	filepath = filename %>%
		pre0(dir) %>%
		paste0( ".xml" ) %>%
		xmlParse
}

root_xml2 = partial(root_xml_fun, dir = dir_formd_xml())
root_xbrl = partial(root_xml_fun, dir = dir_filings_xbrl())

read_filing = function(filename)
	filename %>% path_filing("txt") %>% readLines

read_xbrl = function(filename)
	filename %>% path_filing("xml") %>% readLines

dl_data_file = function(file) {
	filepath = get_filename(file)
	cmd = sprintf("scp mert@162.243.237.190:/home/mert/SEC_Filings/parser/%s %s", filepath, filepath)
	system(cmd)
}

dl_data_file_raw = function(filepath) {
	cmd = sprintf("scp mert@162.243.237.190:/home/mert/SEC_Filings/parser/%s %s", filepath, filepath)
	system(cmd)
}

.dl_data_file_array = function(fun_path_array) {
	function(file)
		function(arg, ...)
			dl_data_file_raw(fun_path_array(file)(arg))
}
dl_data_file_array = .dl_data_file_array(fun_path_array = path_array_fun)
dl01 = function(file, arg) {
	lapply(arg, dl_data_file_array(file))
}

length_list3 = function(list3) {
	# lengths of elements of each sublist in 3rd order list
	# length_list3(list(list(list(1,2), list(3)), list(3,5)))
	length_list2 = function(list2) {
		lapply(list2, length) %>%
			unlist %>%
			sum
	}
	lapply(list3, length_list2)
}
