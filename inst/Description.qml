import QtQuick
import JASP.Module

Description
{
	name		: "jaspTimeSeries"
	title		: qsTr("Time Series")
	description	: qsTr("This module offers time series analyses.")
	version			: "0.20.0"
	author		: "Sophie Berkhout"
	maintainer	: "Sophie Berkhout <s.w.berkhout@uu.nl>"
	website		: "https://sophieberkhout.github.io/"
	license		: "GPL (>= 2)"
	icon		: "analysis-time-series.svg"
	preloadData: true

	Analysis
	{
		title:		qsTr("Descriptives")
		func:		"DescriptivesTimeSeries"
		hasWrapper:	true
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

}
