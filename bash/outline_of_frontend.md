	<url:file:///~/Dropbox (Personal)/projects/stuff/bash/outline_of_frontend.md>

##### Outline of Frontend Topics

1. Chrome specific

	input types can be chrome-specific
		input('.inp',
			{
					type: 'number',
					step: '0.0001',
					name: 'ortaHundurluk'
			})

2. Kullanıcının gördüğü labellar config dosyasından gelmeli

	~/leris/leris-frontend/src/js/resource.js

Örnek kullanım

	 div('#add-herbari.row.border-def', [
					div('.col-xs-3',
							[
									label([resource.plan.determinationDate])

Tanımı:

    plan: {
				...
        determinationDate: 'Müəyyənləşdirilmiş Tarix',


