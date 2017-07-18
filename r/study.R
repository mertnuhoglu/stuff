
study_github = function() {
	repos = github_my_repos("owner", limit = 100)
	toJSON(repos) %>%
		writeLines( "data/repos.json" )
	df = tibble(
		name = repos %>% map_chr("name", .null = NA_character_),
		full_name = repos %>% map_chr("full_name", .null = NA_character_)
	)
	export( df, "../../../mynotes/db/github.xlsx" )
	export( df, "../../../mynotes/db/github.tsv" )
}

study_github_hadleywickham = function() {
	# ref: Hadley Wickham, My Github Commits
	# https://rpubs.com/hadley/gh
	library(gh) # devtools::install_github("gaborcsardi/gh")
	library(purrr)
	library(tibble)
	library(dplyr)
	library(readr)
	library(lubridate)

	my_repos <- function(type = c("all", "owner", "public", "private", "member"), 
											 limit = 100) {
		type <- match.arg(type)
		
		gh(
			"GET /user/repos",
			type = type, 
			sort = "updated",
			.limit = limit
		)
	}
	repos <- my_repos("owner", limit = 100)
	length(repos)
	## [1] 100

	full_name <- repos %>% map_chr("full_name")
	head(full_name, 20)

	repo_commits <- function(full_name, since = "2016-01-01") {
		message("Requesting commits for ", full_name)
		
		commits <- gh("GET /repos/:full_name/commits", 
			full_name = full_name, 
			since = since,
			.limit = Inf
		)
		
		if (length(commits) == 0) {
			return(NULL)
		}
		
		tibble(
			full_name = full_name,
			author = commits %>% map_chr(c("author", "login"), .null = NA_character_),
			datetime = commits %>% map_chr(c("commit", "author", "date"), .null = NA_character_)
		)
	}

	commits <- full_name %>% map(repo_commits) %>% compact() %>% bind_rows()
	commits
}

study_tibble = function() {
	 a <- 1:5
	 tibble(a, b = a * 2)
	 tibble(a, b = a * 2, c = 1)
	 tibble(x = runif(10), y = x * 2)

	 lst(n = 5, x = runif(n))

	 # tibble never coerces its inputs
	 str(tibble(letters))
	 str(tibble(x = list(diag(1), diag(2))))

	 # or munges column names
	 tibble(`a + b` = 1:5)

	 # With the SE version, you give it a list of formulas/expressions
	 tibble_(list(x = ~1:10, y = quote(x * 2)))

	 # data frames can only contain 1d atomic vectors and lists
	 # and can not contain POSIXlt
	 ## Not run:

	 tibble(x = tibble(1, 2, 3))
	 tibble(y = strptime("2000/01/01", "%x"))
	 ## End(Not run)

}
