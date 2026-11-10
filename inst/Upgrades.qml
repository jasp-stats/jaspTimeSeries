import QtQuick
import JASP.Module

Upgrades
{
	Upgrade
	{
		functionName:	"bayesianStateSpace"
		fromVersion:	"0.1.0"
		toVersion:		"0.16.4"

		ChangeRename	{ from: "factors";										to: "fixedFactors"						}
		ChangeRename	{ from: "dates";										to: "time"								}
		ChangeRename	{ from: "postSummaryTable";								to: "posteriorSummaryTable"				}
		ChangeRename	{ from: "posteriorSummaryCoefCredibleIntervalValue";	to: "posteriorSummaryCiLevel"			}
		ChangeRename	{ from: "showCoefMeanInc";								to: "posteriorSummaryConditionalEstimates"	}
		ChangeRename	{ from: "expectedModelSize";							to: "expectedPredictors"				}

		ChangeRename	{ from: "checkboxAr";									to: "autoregressiveComponent"			}

		ChangeJS
		{
			name: "lagSelectionMethod"
			jsFunction: function(options)
			{
				switch(options["lagSelectionMethod"])
				{
					case "manualAR":		return "manual";
					case "autoAR":			return "auto";
				}
			}
		}

		ChangeRename	{ from: "lagSelectionMethod";							to: "autoregressiveLagSelectionMethod"	}

		ChangeRename	{ from: "noLags";										to: "autoregressiveLags"								}
		ChangeRename	{ from: "maxNoLags";									to: "autoregressiveMaxLags"							}

		ChangeRename	{ from: "checkboxLocalLevel";							to: "localLevelComponent"				}
		ChangeRename	{ from: "checkboxLocalLinearTrend";						to: "localLinearTrendComponent"			}
		ChangeRename	{ from: "checkboxSemiLocalLinearTrend";					to: "semiLocalLinearTrendComponent"		}

		ChangeRename	{ from: "checkboxDynReg";								to: "dynamicRegressionComponent"		}

		ChangeRename	{ from: "DynRegLags";									to: "dynamicRegregressionLags"			}

		ChangeJS
		{
			name: "seasonalities"
			jsFunction: function(options)
			{
				return options["seasonalities"].map(item=>{
					item["number"]					= item["nSeason"];
					item["duration"]				= item["seasonDuration"];

					item["inverseGammaPriorSd"]		= item["sigma.guess"];
					item["inverseGammaPriorN"]		= item["sample.size"];

					item["normalPriorMean"]			= item["mu"];
					item["normalPriorSd"]			= item["sigma"];
					return item;
				})
			}
		}



		ChangeRename	{ from: "checkboxPlotAggregatedStates";					to: "aggregatedStatesPlot"				}
		ChangeRename	{ from: "ciAggregatedStates";							to: "aggregatedStateAndErrorPlotsCiLevel"		}

		ChangeRename	{ from: "actualValuesAggregatedStates";					to: "aggregatedStatesPlotObservationsShown"		}

		ChangeRename	{ from: "checkboxPlotComponentStates";					to: "componentStatesPlot"				}


		ChangeRename	{ from: "checkBoxResidual";								to: "residualPlot"						}
		ChangeRename	{ from: "checkBoxForecastError";						to: "forecastErrorPlot"					}
		ChangeRename	{ from: "checkControlChart";							to: "controlChartPlot"					}
		ChangeRename	{ from: "controlPeriod";								to: "controlChartPeriodEnd"			}
		ChangeRename	{ from: "controlSigma";								to: "controlChartSigmaThreshold"		}
		ChangeRename	{ from: "checkControlProbPlot";							to: "controlChartProbabilisticPlot"			}

		ChangeRename	{ from: "timeout";										to: "mcmcTimeoutSeconds"				}
		ChangeRename	{ from: "burnSpecification";							to: "burninMethod"						}
		ChangeRename	{ from: "propBurnSuggested";							to: "burninAutomaticProportion"			}
		ChangeRename	{ from: "numberBurnManual";								to: "burninManualDraws"				}

		ChangeJS
		{
			name: "burninMethod"
			jsFunction: function(options)
			{
				switch(options["burninMethod"])
				{
					case "burnManual":				return "manual";
					case "burnSuggested":			return "auto";
				}
			}
		}

	}
}

