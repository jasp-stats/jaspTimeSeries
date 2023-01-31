import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspTimeSeries"
	title		: qsTr("Time Series")
	description	: qsTr("This module offers time series analyses.")
	version		: "0.1"
	author		: "Sophie Berkhout"
	maintainer	: "Sophie Berkhout <s.w.berkhout@uu.nl>"
	website		: "https://sophieberkhout.github.io/"
	license		: "GPL (>= 2)"
	icon		: "analysis-time-series.svg"

	Analysis
	{
		title:	qsTr("Descriptives")
		func:		"DescriptivesTimeSeries"
	}

	Analysis
	{
		title:		qsTr("Data Transformation")
		func:		"DataTransformationTimeSeries"
	}

	Analysis
	{
		title:  qsTr("ARIMA")
		func:		"ARIMATimeSeries"
	}

	// Analysis
	// {
	// 	title:			qsTr("Spectral Analysis")
	// 	func:			"SpectralAnalysis"
	// }

}