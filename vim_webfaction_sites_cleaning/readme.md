# Problem

I want to get the list of all my web sites in a specific format: domain_name, application_name, website_name. _

I get the html list of all web sites from webfaction's control panel. 

## input

		adamolacak26 	Domains 	Apps
		Web0139 - †89.58.75.233 	

				http://adamolacakminik.com

			

				osaom (Django on /)
				okuloncesi_blog (WordPress on /blog)

		csby 	Domains 	Apps
		Web0139 - †89.58.75.233 	

				http://byvcrm.mertnuhoglu.com

			

				csby (Static/CGI/PHP on /)
	 ...

## output
 
    http://adamolacakminik.com osaom (Django on /) okuloncesi_blog (WordPress on /blog)
    http://derinakintilar.com http://www.derinakintilar.com dawp (WordPress on /)

## viml script

	v/^./d
	g/^\s\+$/d
	g/Domains/d
	g/^Web/+1,/^Web/-1 j
	g/^Web/d
	v/WordPress/d
