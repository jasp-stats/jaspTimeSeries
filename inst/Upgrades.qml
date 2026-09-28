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

		ChangeRename	{ from: "noLags";										to: "lags"								}
		ChangeRename	{ from: "maxNoLags";									to: "maxLags"							}

		ChangeRename	{ from: "checkboxLocalLevel";							to: "localLevelComponent"				}
		ChangeRename	{ from: "checkboxLocalLinearTrend";						to: "localLinearTrendComponent"			}
		ChangeRename	{ from: "checkboxSemiLocalLinearTrend";					to: "semiLocalLinearTrendComponent"		}

		ChangeRename	{ from: "checkboxDynReg";								to: "dynamicRegregressionComponent"		}

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
		ChangeRename	{ from: "ciAggregatedStates";							to: "aggregatedStatesPlotCiLevel"		}

		ChangeRename	{ from: "actualValuesAggregatedStates";					to: "aggregatedStatesPlotObservationsShown"		}

		ChangeRename	{ from: "checkboxPlotComponentStates";					to: "componentStatesPlot"				}


		ChangeRename	{ from: "checkBoxResidual";								to: "residualPlot"						}
		ChangeRename	{ from: "checkBoxForecastError";						to: "forecastErrorPlot"					}
		ChangeRename	{ from: "checkControlChart";							to: "controlChartPlot"					}
		ChangeRename	{ from: "checkControlProbPlot";							to: "probalisticControlPlot"			}

		ChangeRename	{ from: "mcmcDraws";									to: "samples"							}
		ChangeRename	{ from: "burnSpecification";							to: "burninMethod"						}
		ChangeRename	{ from: "propBurnSuggested";							to: "automaticBurninProportion"			}
		ChangeRename	{ from: "numberBurnManual";								to: "manualBurninAmount"				}

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

