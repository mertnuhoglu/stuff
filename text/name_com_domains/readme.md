# Problem

I copied all my registered domains from name.com

I want to clean the data and leave only the domain names.

# Input

	imesteel.com
	Add to Cart
	08 Apr 2015
	dekoratifporselen.com
	Add to Cart
	19 Apr 2015

# Output

	imesteel.com
	dekoratifporselen.com

# viml script

	g/Cart/d
	g/\d\+/d
	v/\./d

