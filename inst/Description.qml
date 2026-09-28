import QtQuick
import JASP.Module

Description
{
	name		: "jaspTimeSeries"
	title		: qsTr("Time Series")
	description	: qsTr("This module offers time series analyses.")
	version			: "0.96.3"
	author		: "Sophie Berkhout, Fridtjof Petersen, Henrik Godmann"
	maintainer	: "Henrik Godmann"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-time-series.svg"
	preloadData: true

	GroupTitle
	{
		title:		qsTr("Descriptives")
		icon:		"analysis-time-series.svg"
	}

	Analysis
	{
		title:		qsTr("Time Series Descriptives")
		func:		"DescriptivesTimeSeries"
		hasWrapper:	true
	}

	GroupTitle
	{
		title:		qsTr("Classical")
		icon:		"analysis-time-series.svg"
	}

	Analysis
	{
		title:		qsTr("Stationarity")
		func:		"StationarityTimeSeries"
	}

	Analysis
	{
		title:  qsTr("ARIMA")
		func:		"ARIMATimeSeries"
	}

	Analysis
	{
		title:	qsTr("Spectral Analysis")
		func:		"SpectralTimeSeries"
	}

	GroupTitle
	{
		title:		qsTr("Bayesian")
		icon:		"bsts.svg"
	}

	Analysis
	{
		title:		qsTr("Gaussian State Space Models")
		func:		"bayesianStateSpace"
		qml:		"bayesianStateSpace.qml"
		hasWrapper:	true
		preloadData:	false
	}

}
