source('utils.R')

study_example01 = function() {
	vars = read_csv("vars01.csv")
	# make it a json
	pat = vars$var1[1]
	lines = readLines("input_data.txt")
	m = str_match(lines, pat)
	r = m[, 1]
	t = readLines("template01.txt") %>% paste(collapse = "\n")
	t2 = str_replace_all(t, "@var\\d*@", "%s")
	r2 = sprintf(t2, r, lines)
	writeLines(r2, "output01.txt")
}

study_example02 = function() {
	pat = vars$var1[1]
	lines = readLines("input_data.txt")
	m = str_match(lines, pat)
	r = m[, 1]
	t = readLines("template02.txt") %>% paste(collapse = "\n")
	r2 = sprintf(t, lines, r)
	# sprintf("%2$s %1$s", lines, r)
	writeLines(r2, "output02.txt")
}

study_example03 = function() {
	pat = "book_[^.]*"
	lines = readLines("input_data.txt")
	m = str_match(lines, pat)
	r = m[, 1]
	t = readLines("template02.txt") %>% paste(collapse = "\n")
	r2 = sprintf(t, lines, r)
	# sprintf("%2$s %1$s", lines, r)
	writeLines(r2, "output02.txt")
}

