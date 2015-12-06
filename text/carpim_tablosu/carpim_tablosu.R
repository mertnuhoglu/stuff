library("data.table")
library("readr")
library("dplyr")
library("stringr")

build_carpim_tablosu = function() {
	ilk = 1:10
	iki = 1:10
	sonuc = vector('list', 10)
	df_l = vector('list', 10)
	for (i in iki) {
		sonuc[[i]] = ilk * i
		df_l[[i]] = data.table(ilk = ilk, iki = i, sonuc = sonuc[[i]])
	}
	df = rbindlist(df_l)

	r = paste(df, collapse = ",")
	r = str_replace(r, ",", " x ")
	r = str_replace(r, ",", " = ")

	lines = vector('list', 100)
	for (i in 1:nrow(df)) {
		lines[[i]] = paste(df[i], collapse = ",") %>%
			str_replace(",", " x ") %>%
			str_replace(",", " = ")
	}
	writeLines(lines, "carpim_tablosu.txt")
}
