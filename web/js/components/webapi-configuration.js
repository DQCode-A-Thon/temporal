define(['knockout', 'text!./webapi-configuration.html'], function (ko, view) {
	function webapiConfiguration(params) {
		var self = this;
		self.model = params.model;


		self.setCurrentService = function (service) {
			self.currentServiceUrl(service.url);
            self.model.currentService(service);
            
            // Retrieve the summary stats from the static JSON files
			$.ajax({
				url: service.url + 'summary.json',
				success: function (results) {
                    self.model.currentSummaryStats(results);
				},
				error: function (xhr, message) {
					alert('Could not find ' + service.url + 'summary.json');
				}
			});
		}

		self.services = params.services;
		self.currentServiceUrl = params.currentServiceUrl;
	}

	var component = {
		viewModel: webapiConfiguration,
		template: view
	};

	ko.components.register('webapi-configuration', component);
	return component;
});
