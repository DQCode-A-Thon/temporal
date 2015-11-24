requirejs.config({
	baseUrl: 'js',
    packages: [
		{
		    name: "databindings",
		    location: "databindings"
		},
        {
		    name: "extenders",
		    location: "extenders"
        }
    ],
    config: {
		text: {
			useXhr: function (url, protocol, hostname, port) {
				return true;
			}
		}
	},    
	map: {
		'*': {
			'css': 'plugins/css.min'
		}
	},
	shim: {
        "colorbrewer": {
			exports: 'colorbrewer'
		},
		"bootstrap": {
			"deps": [
				'jquery',
                'jquery-ui'
			]
		},
        "facets": {
			"deps": ['jquery'],
			exports: 'FacetEngine'
		}
	},
	paths: {
        "text": "plugins/text",
		"jquery":  "https://cdnjs.cloudflare.com/ajax/libs/jquery/1.11.2/jquery.min",
        "jquery-ui": "https://cdnjs.cloudflare.com/ajax/libs/jqueryui/1.11.4/jquery-ui.min",
		"bootstrap": "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.2/js/bootstrap.min",
		"colvis": "https://cdnjs.cloudflare.com/ajax/libs/datatables-colvis/1.1.0/js/datatables.colvis.min",
        "knockout": "https://cdnjs.cloudflare.com/ajax/libs/knockout/3.3.0/knockout-min",
        "datatables": "https://cdnjs.cloudflare.com/ajax/libs/datatables/1.10.8/js/jquery.dataTables.min",
        "director": "https://cdnjs.cloudflare.com/ajax/libs/Director/1.2.8/director.min",
        "lscache": "lscache.min",
		"d3": "d3.min",
		"d3_tip": "d3.tip",        
		"jnj_chart": "jnj.chart",
		"lodash": "lodash.min",
        "knockout.dataTables.binding": "knockout.dataTables.binding",
        "home": "components/home",
        "viewer": "components/viewer"
	}
});

requirejs(['knockout', './app', 'director', 'home', 'viewer'], function(ko, app) {
    var pageModel = new app();
    var routerOptions = {
		notfound: function () {
			pageModel.currentView('home');
		}
	}
    var routes = {
        '/viewer/:conceptid:': pageModel.viewer,
		'/viewer': function () {
			pageModel.currentView('viewer');
		}
	}
    pageModel.router = new Router(routes).configure(routerOptions);
    window.pageModel = pageModel;
    
    $.when.apply($).done(pageModel.initComplete);
	ko.applyBindings(pageModel);
});
