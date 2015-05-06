# Problem

I have some notes written in a file. I put tags for each note at the beginning of the note. I want to get a list of all tags in all notes in the file.

## Input

	_example
	http://conversionxl.com/optimization-experts-share-their-favorite-google-analytics-reports/

	_idea _gtd
	prepare a data analysis for national elections

## Output

	 _example
	_idea _gtd

## Script

	v/^_/d
	%s/ \+$//
	sort u

I put this script into ExtractTagsWithUnderlineSymbol() function in vim-infoman.vim plugin.
