source('utils.R')

main = function() {
	study_example03()
}

study_problem03_a = function() {
	data = fread("problem03_vars.csv")
	t = readLines("problem03_template.txt") %>% paste(collapse = "\n")
	#lapply(data[,, with=F], function(x) sprintf(t, x))
	#lapply(data[,, with=T], function(x) sprintf(t, x))
	#lapply(data, function(x) sprintf(t, x))
	result = list()
	for (i in 1:nrow(data)) {
		x = data[i] %>% unlist
		result = append(result, do.call(sprintf, c(list(t), x)))
	}
	result = unlist(result)
	writeLines(paste(result), "problem03_output.txt")
}

study_problem02_c = function() {
	data = fread("problem02_vars.csv")
	t = readLines("problem02_template.txt") %>% paste(collapse = "\n")
	#lapply(data[,, with=F], function(x) sprintf(t, x))
	#lapply(data[,, with=T], function(x) sprintf(t, x))
	#lapply(data, function(x) sprintf(t, x))
	result = list()
	for (i in 1:nrow(data)) {
		x = data[i] %>% unlist
		result = append(result, do.call(sprintf, c(list(t), x)))
	}
	result = unlist(result)
	writeLines(paste(result), "temp.txt")
}

study_problem02_b = function() {
	data = fread("problem02_vars.csv")
	t = readLines("problem02_template.txt") %>% paste(collapse = "\n")
	#sprintf(t, data[[1]] %>% unlist)
	result = list()
	for (i in 1:nrow(data)) {
		x = data[i] %>% unlist
		result = append(result, do.call(sprintf, c(list(t), x)))
	}
	result = unlist(result)
}

study_problem02_a = function() {
	data = fread("problem02_vars.csv")
	x = data[1] %>% unlist
	t = readLines("problem02_template.txt") %>% paste(collapse = "\n")
	#sprintf(t, x)
	do.call(sprintf, c(list(t), x))
}

study_example03 = function() {
	pat = "book_[^.]*"
	lines = readLines("input_data.txt")
	m = str_match(lines, pat)
	r = m[, 1]
	t = readLines("template02.txt") %>% paste(collapse = "\n")
	# > t
	# [1] "%2$s\n\t%1$s"
	# > lines[1]
	# [1] "<url:/Users/mertnuhoglu/Dropbox/mynotes/book_implementing_analytics.otl>"
	# > r[1]
	# [1] "book_implementing_analytics" 
	# > r2[1]
	# [1] "book_implementing_analytics\n\t<url:/Users/mertnuhoglu/Dropbox/mynotes/book_implementing_analytics.otl>"
	r2 = sprintf(t, lines, r)
	# sprintf("%2$s %1$s", lines, r)
	writeLines(r2, "output02.txt")
}

