# Problem

Convert some lines into some other text using

- a template
- varying parts of the lines

## Input

An example for how to use templating:

<url:/Users/mertnuhoglu/Dropbox/mynotes/book_implementing_analytics.otl>
<url:/Users/mertnuhoglu/Dropbox/mynotes/book_for_dummies_data_mining.otl>

## Output

book_implementing_analytics
	<url:/Users/mertnuhoglu/Dropbox/mynotes/book_implementing_analytics.otl>
book_for_dummies_data_mining
	<url:/Users/mertnuhoglu/Dropbox/mynotes/book_for_dummies_data_mining.otl>

## How

	ex of problem
		<...target...>
		->
		target
			<...target...>
	how to convert it?
		match target with regex
		prepare a template for final output
			@var1@
				@var0@
		var0: existing line
		var1: specified regex in vars.csv
	input docs
		input_data.txt
		vars.csv
			var1,var2,...
			book_[^.]*,...
		template.txt
			@var1@
			 @var0@
