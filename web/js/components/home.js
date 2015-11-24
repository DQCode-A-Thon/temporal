define(['knockout', 'text!./home.html'], function (ko, view) {
	function home(params) {
		var self = this;
		self.model = params.model;

        self.checkExecuteSearch = function (data, e) {
			if (e.keyCode == 13 || e.type == 'click') { // enter
				var query = $('#querytext').val();
                document.location = "#/search/" + encodeURI(query);
			}
		};
	}

    var component = {
		viewModel: home,
		template: view
	};

	ko.components.register('home', component);
	return component;
});