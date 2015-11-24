define([
	'jquery',
	'knockout',
    'jnj_chart',
    'd3',
	'components/webapi-configuration',
	'bootstrap',
    'facets',
    'databindings',
    'colvis',
    'extenders'
], function ($, ko, jnj_chart, d3) {

	var appModel = function () {
        var self = this;
		this.currentView = ko.observable('home');
		this.appHeading = ko.observable('Data Bound Panel Heading');
		this.appBody = ko.observable('Data Bound Panel Body Content');

		// configure services to include at least one valid OHDSI WebAPI endpoint
		this.services = [
			{
				name: 'CCAE',
				url: 'js/json/ccae/',
				dialect: ko.observable('loading'),
				version: ko.observable('loading')
			},
			{
				name: 'OPTUM',
				url: 'js/json/optum/',
				dialect: ko.observable('loading'),
				version: ko.observable('loading')
			},
			{
				name: 'MDCR',
				url: 'js/json/mdcr/',
				dialect: ko.observable('loading'),
				version: ko.observable('loading')
			},
			{
				name: 'MDCD',
				url: 'js/json/mdcd/',
				dialect: ko.observable('loading'),
				version: ko.observable('loading')
			},
			{
				name: 'CPRD',
				url: 'js/json/cprd/',
				dialect: ko.observable('loading'),
				version: ko.observable('loading')
			},
			{
				name: 'JMDC',
				url: 'js/json/jmdc/',
				dialect: ko.observable('loading'),
				version: ko.observable('loading')
			},
			{
				name: 'PREMIER',
				url: 'js/json/premier/',
				dialect: ko.observable('loading'),
				version: ko.observable('loading')
			}
		];

		this.currentServiceUrl = ko.observable(this.services[0].url);
        this.currentService = ko.observable(this.services[0]);
		this.currentSummaryStats = ko.observable();
        this.currentSearch = ko.observable();
        this.currentConceptId = ko.observable();

        self.initComplete = function () {
            self.router.init('/');
        }

		this.getSummaryStats = function() {
				// Retrieve the summary stats from the static JSON files
				$.ajax({
					url: self.currentServiceUrl() + 'summary.json',
					success: function (results) {
						self.currentSummaryStats(results);
					},
					error: function (xhr, message) {
						alert('Could not find ' + self.currentServiceUrl() + 'summary.json');
					}
				});        	

		}
		
        this.currentSummaryStats(this.getSummaryStats());

        this.currentService.subscribe(function (newValue) {
        	if (newValue != undefined) {        		
        		this.getSummaryStats();
        	}
        });
        
        this.viewer = function(conceptid) {
            self.currentView('viewer');
            self.currentConceptId(conceptid);
        }
	}

	return appModel;
});
