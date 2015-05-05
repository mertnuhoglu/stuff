library(lubridate)
source('utils.R')
source('filing_manager.R')
source('filing_controller.R')
source('filing_functions.R')

DIR_FORMD = "form_d/data/txt/"

main_parse_formd = function() {
	mco = read.merged.classified.optimized()
	formd = find_formd(mco)
   sampled_formd = sample_filings_per_companies(formd)

	single_build_most_recent_filings()
	single_url_to_filename_mappings()
	single_download_all_formd()

	single_formd2json()

	main_formd_after_shell()
}

main_formd_after_shell = function() {
	single_add_most_recent_to_formd()

	single_build_mappings_amendment_to_overwritten()

	single_remove_overwritten_filings()

	single_aggregate_formd_variables()

	single_build_header_data_for_formd()
	single_add_header_data_to_formd()
}

single_build_stats = function() {
	fh = read_formd_with_header()
	setkey(fh, offeringData_industryGroup_industryGroupType)
	x = fh["Pooled Investment Fund"]

	result = sprintf("Pooled Investment / Total: %d/%d", nrow(x), nrow(fh))
	nrow(x)
	setkey(fh, IRS.NUMBER.)
	x = fh['000000000']
	nrow(x)
	result = c(result, sprintf("No IRS Number / Total: %d/%d", nrow(x), nrow(fh)))

	setkey(fh, offeringData_industryGroup_industryGroupType, IRS.NUMBER.)
	x = fh[J("Pooled Investment Fund",'000000000')]
	nrow(x)
	result = c(result, sprintf("No IRS Number and Pooled Investment Fund/ Total: %d/%d", nrow(x), nrow(fh)))

	cat( result, file = 'form_d/data/report/stats.txt', sep = '\n', append = F )
}

single_build_header_data_for_formd = function() {
	result = process_build_header_data_for_formd()

	write_header_formd( df = result$single )
	write_header_formd_former_names( df = result$multiple )
}

single_add_header_data_to_formd = function() {
	jff = read_joined_formd_filtered()
	hf = read_header_formd()

	fh = process_add_header_data_to_formd( jff, hf )

	write_formd_with_header(fh)
}

process_add_header_data_to_formd = function( formd, header_data ) {
	setkey(formd, filename)
	setkey(header_data, filename)
	fh = formd[ header_data, nomatch=0 ]
}

process_build_header_data_for_formd = function() {
	filenames = list.files( path = 'form_d/data/txt/', pattern = 'txt$', full.names = T )
	result = process_store_header_data( filenames )
	return(result)
}

single_aggregate_formd_variables = function() {
	jff = read_joined_formd_filtered()

	aggregated_formd = aggregate_formd_variables( jff )

	write_aggregated_formd(aggregated_formd)

	aggregated_formd_total = aggregate_formd_variables_total( jff )

	write_aggregated_formd_total(aggregated_formd_total)

}
 
single_build_most_recent_filings = function() {
	mco = read.merged.classified.optimized()

	formd = find_formd(mco)
	most_recent_filings = process_build_most_recent_filings( formd )

	write_most_recent_filings( most_recent_filings )
}

process_build_most_recent_filings = function( filings ) {
	sink(file = 'logs.txt', append = T)

	most_recent_filings = filings[ , c('cik', 'url', 'date'), with=F ]
	most_recent_filings = data.table( cik=filings$cik, filename=form_name( filings$url, ext = F ), date=filings$date )
	mrf = most_recent_filings
	setkey(mrf,cik)
	ciks = unique(mrf$cik)
	fn = character()
	total = length(ciks)
	for (i in 1:total) {
		cikno = ciks[i]
		b = mrf[cikno]
		b = b[order(date)]
		x = tail(b,1)
		fn = c(fn, x$filename)
		log.progress( i, total )
	}
	mrf$most_recent = FALSE
	mrf[ filename %in% fn ]$most_recent = TRUE

	sink(file = NULL)
	return(mrf)
}

single_add_most_recent_to_formd = function() {
	jf = read_joined_formd()
	
   jfmr = process_add_most_recent_to_formd(jf)

	write_joined_formd_most_recent(jfmr)

	formd_subarray_names = get_formd_subarray_names()
	formd_most_recent_subarray_names = get_formd_most_recent_subarray_names()
	for (i in 1:length(formd_subarray_names) ) {
		fsn = formd_subarray_names[i]
		fsnmr = formd_most_recent_subarray_names[i]
		fsd = read.file( fsn )

		fsdmr = process_add_most_recent_to_formd(fsd)

		write.file(fsdmr, fsnmr)
	}
}

process_add_most_recent_to_formd = function( formd_data ) {
	fd = formd_data
	mrf = read_most_recent_filings()
	setkey(fd, filename)
	setkey(mrf, filename)
	fdmr = fd[mrf, nomatch=0]
}

single_remove_overwritten_filings = function() {
	mappings = read_mappings_amendment_to_overwritten()
	overwritten = mappings$overwritten

	j = read_joined_formd_most_recent()

	joined_formd_filtered = remove_overwritten_filings(j, overwritten)
	write_joined_formd_filtered(joined_formd_filtered)

	# @todo: refactor to dry shouldn't repeat for all of them
	#joined_formd_previous_names = remove_overwritten_filings(j, overwritten)
	#write_joined_formd_previous_names(joined_formd_previous_names)
	#joined_formd_related_persons = remove_overwritten_filings(j, overwritten)
	#write_joined_formd_related_persons(joined_formd_related_persons)
	#joined_formd_sales_compensation = remove_overwritten_filings(j, overwritten)
	#write_joined_formd_sales_compensation(joined_formd_sales_compensation)
}

process_most_recent_ex = function(joined_formd) {
	j = joined_formd
	a = j[ , c('filename','primaryIssuer_cik','offeringData_signatureBlock_signature_signatureDate'), with = F]
	setnames( a, c('primaryIssuer_cik','offeringData_signatureBlock_signature_signatureDate'), c('cik','signatureDate') )
	setkey(a,cik)
	ciks = unique(a$cik)
	fn = character()
	for (i in 1:length(ciks)) {
		cikno = ciks[i]
		b = a[cikno]
		b = b[order(signatureDate)]
		x = tail(b,1)
		fn = c(fn, x$filename)
	}
	j$mostRecent = FALSE
	j[ filename %in% fn ]$mostRecent = TRUE
	return(j)
}

# aggregated variables of formd per year and cik
aggregate_formd_variables = function( joined_formd ){
	j = joined_formd
	j$year = year(j$date)
	amounts = j[ , c('primaryIssuer_cik','offeringData_offeringSalesAmounts_totalOfferingAmount','offeringData_offeringSalesAmounts_totalAmountSold','offeringData_offeringSalesAmounts_totalRemaining','year'), with = F]
	setnames(amounts, c('primaryIssuer_cik','offeringData_offeringSalesAmounts_totalOfferingAmount','offeringData_offeringSalesAmounts_totalAmountSold','offeringData_offeringSalesAmounts_totalRemaining'), c('cik','offering','sold','remaining'))
	g = amounts[ , list( offering = sum(offering), sold = sum(sold), remaining = sum(remaining),  offering_na = sum(offering, na.rm = T), sold_na = sum(sold, na.rm = T), remaining_na = sum(remaining, na.rm = T) ), by=list(cik, year)]
	setkey(g,cik)
	return(g)
}

# aggregated variables of formd per year and cik
aggregate_formd_variables_total = function( joined_formd ){
	j = joined_formd
	amounts = j[ , c('primaryIssuer_cik','offeringData_offeringSalesAmounts_totalOfferingAmount','offeringData_offeringSalesAmounts_totalAmountSold','offeringData_offeringSalesAmounts_totalRemaining'), with = F]
	setnames(amounts, c('primaryIssuer_cik','offeringData_offeringSalesAmounts_totalOfferingAmount','offeringData_offeringSalesAmounts_totalAmountSold','offeringData_offeringSalesAmounts_totalRemaining'), c('cik','offering','sold','remaining'))
	g = amounts[ , list( offering = sum(offering), sold = sum(sold), remaining = sum(remaining),  offering_na = sum(offering, na.rm = T), sold_na = sum(sold, na.rm = T), remaining_na = sum(remaining, na.rm = T) ), by=list(cik)]
	setkey(g,cik)
	return(g)
}

single_formd2json = function() {
	# todo: refactor the duplicate lines following:
	mco = read.merged.classified.optimized()
	formd = find_formd(mco)
   sampled_formd = sample_filings_per_companies(formd)

	file_names = form_name(sampled_formd$url)

	setwd('form_d')
	system2('./main_txt2xml.sh', args = file_names, stdout=F)
	system2('./main_xml2json.sh', args = file_names, stdout=F)
	#system2('./main_json2csv.sh', args = file_names, stdout=F)
	system2('./main_json2csv.sh', args = c('../config/mappings_formd_json2csv.txt', 'joined_formd.csv'), stdout=F)
	system2('./main_json2csv.sh', args = c('../config/mappings_formd_json2csv_previous_names.txt', 'joined_formd_previous_names.csv'), stdout=F)
	system2('./main_json2csv.sh', args = c('../config/mappings_formd_json2csv_related_persons.txt', 'joined_formd_related_persons.csv'), stdout=F)
	system2('./main_json2csv.sh', args = c('../config/mappings_formd_json2csv_sales_compensation.txt', 'joined_formd_sales_compensation.csv'), stdout=F)
	setwd('..')
}

# @todo: input?
single_build_mappings_amendment_to_overwritten = function(formd) {
	joined_formd = read_joined_formd_most_recent()

	mappings_amendment_to_overwritten = build_mappings_amendment_to_overwritten(joined_formd)

	write_mappings_amendment_to_overwritten(mappings_amendment_to_overwritten)
}

# @todo: refactoring build_mappings_amendment_to_overwritten_v1 
build_mappings_amendment_to_overwritten = function(joined_formd) {
	build_mappings_amendment_to_overwritten_v2(joined_formd)
	#form_types = unique(filings$form)
	#amendment_form_types = grep('/A', form_types, value=T)
	#setkey(filings, form)
	#amendments = filings[amendment_form_types]
	#for ()
}

# @todo: refactoring dry
find_most_recent_before = function(filings, latest.date) {
	setkey(filings, date)
	tail( filings[date <= latest.date], 1 )
}

# @todo: refactoring with build_mappings_amendment_to_overwritten
build_mappings_amendment_to_overwritten_v2 = function(joined_formd) {
	j = joined_formd
	a = j[ , c('filename','cik','offeringData_typeOfFiling_dateOfFirstSale_value', 'offeringData_typeOfFiling_newOrAmendment_isAmendment','date'), with = F]
	setnames( a, c('offeringData_typeOfFiling_dateOfFirstSale_value', 'offeringData_typeOfFiling_newOrAmendment_isAmendment'), c('dateOfFirstSale','isAmendment') )
	setkey(a, isAmendment)
	amendments = a[TRUE]
	setkey(a,cik,dateOfFirstSale)
	mappings = data.table( amendment=character(), overwritten=character() )
	for (i in 1:nrow(amendments)) {
		amend = amendments[i]
		b = a[ J(amend$cik, amend$dateOfFirstSale) ]
		b = b[order(date)]
		if (nrow(b) == 1) next
		b = b[ 1:(nrow(b)-1) ]
		m = data.table( amendment = amend$filename, overwritten = b$filename )
		mappings = rbind( mappings, m )
	}
	# return mappings
	# get J(fn) from it
	return(mappings)
}

# @todo: refactoring with build_mappings_amendment_to_overwritten
build_mappings_amendment_to_overwritten_v1 = function(filings) {
	form_types = unique(filings$form)
	amendment_form_types = grep('/A', form_types, value=T)
	setkey(filings, form)
	originals_to_be_removed = data.table()
	for (form_type in amendment_form_types) {
		amendments = filings[form_type]
		plain_form_type = sub( '/A', '', form_type )
		plains = filings[plain_form_type]
		setkey(plains, cik)
		for (i in 1:nrow(amendments) ) {
			amend = amendments[i]
			filings_cik = plains[amend$cik]
			most_recent = find_most_recent_before(filings_cik, amend$date)
			if ( nrow(most_recent) == 0 ) {
				# @todo: should i store them?
				next
			}
			originals_to_be_removed = rbind(originals_to_be_removed, most_recent)
		}
	}
	return(originals_to_be_removed)
}

single_url_to_filename_mappings = function() {
	mco = read.merged.classified.optimized()

	url_to_filename_mappings = build_url_to_filename_mappings(mco)

	write_url_to_filename_mappings(url_to_filename_mappings)
}

build_url_to_filename_mappings = function(filings) {
	url_to_filename_mappings = data.table( url = filings$url, filename = form_name_no_extension(filings$url) )
}

remove_overwritten_filings = function(joined_formd, overwritten_filenames) {
	j = joined_formd
	o_fn = overwritten_filenames
	j_fn = j$filename
	d_fn = setdiff( j_fn, o_fn )
	setkey(j,filename)
	joined_formd_valid = j[ d_fn ]
}

remove_overwritten_filings_ex = function(joined_formd, originals_to_be_removed) {
	j = joined_formd
	o = originals_to_be_removed
	o_url = o$url

	u2f = read_url_to_filename_mappings()
	setkey(u2f, filename)
	j_u2f = u2f[ j$filename ]
	j_url = j_u2f$url

	d_url = setdiff( j_url, o_url )
	setkey(j_u2f, url) 
	d_fn = j_u2f[ d_url ]$filename
	setkey( j, filename )
	joined_formd_valid = j[ d_fn ]
}

find_formd = function( filings ) {
	find.by.form.type( filings, c('D','D/A') )
}

# todo: refactoring with <url:filing_functions.R#download.urls>
# <url:company_controller.R#download.lists>
download_urls = function( urls, use.cache = F ) {
	total = length(urls)
	for(i in 1:total) {
		url = urls[i]
		file.name = form_name(url)
		if ( use.cache ) {
			if ( file.exists( file.name ) ) {
				print( paste0( 'skipping ', file.name , ' because it does exist' ))
				return()
			}
		}
		download.file(url, destfile=paste0(DIR_FORMD,file.name), method="curl")
		log.progress( i, total )
	}
}

single_download_all_formd = function( use.cache = F ) {
	mco = read.merged.classified.optimized()
	formd = find_formd(mco)

	process_download_filings(formd, use.cache = use.cache)
}

# todo: refactor with <url:formd.R#single_download_all_formd>
single_download_sampled_formd = function(use.cache = F) {
	mco = read.merged.classified.optimized()
	formd = find_formd(mco)
   sampled_formd = sample_filings_per_companies(formd)

	process_download_filings(sampled_formd, use.cache = use.cache)
}

process_download_filings = function( filings, use.cache = F ) {
	sink(file = 'logs.txt', append = T)

	download_urls( filings$url, use.cache = use.cache )

	sink(file = NULL)
}

# todo: refactor <url:filing_controller.R#main.sample.and.dl.filings>
sample_filings_per_companies = function(filings) {
   sampled_ciks = sample_companies(filings)
	setkey(filings, cik)
	filings[sampled_ciks]
}

# todo: refactoring <url:filing_controller.R#main.sample.and.dl.filings>
sample_companies = function(filings, sample_size = 100) {
	ciks = unique(filings$cik)
	set.seed(1)
	sampled_ciks = sample(ciks, sample_size)
}
