# Problem

I want to list all my github projects with their urls.

I get the input data from my github profile page: https://github.com/mertnuhoglu?tab=repositories

## input

	stuff
	All kinds of stuff stuffed here.
	Updated 21 hours ago
	Shell  0   0
	classify_books
	Updated 11 days ago
	CSS  0   0

## output

	stuff	https://github.com/mertnuhoglu/stuff
	classify_books	https://github.com/mertnuhoglu/classify_books

## viml script

	function! ConvertGithubPage2ProjectList()
		" projects/stuff/text/list_github_projects
		g/forked/-2 d
		g/forked/+2 d
		g/ /d
		g/^\s*$/d
		%s#\(.\+\)#\1\thttps://github.com/mertnuhoglu/\1#
		sort
	endfunction
	command! ConvertGithubPage2ProjectList call ConvertGithubPage2ProjectList()
