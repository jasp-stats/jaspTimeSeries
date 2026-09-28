import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
	Formula
	{
		lhs: "dependent"
		rhs: [{ name: "modelTerms", extraOptions: "isNuisance" }]
		userMustSpecify: "covariates"
	}

	VariablesForm
	{

		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";		title: qsTr("Dependent Variable");	allowedColumns: 	["scale"];	singleVariable: true}
		AssignedVariablesList	{ name: "covariates";		title: qsTr("Covariates");			allowedColumns: 	["scale"];	id: covariates		}
		AssignedVariablesList 	{ name: "fixedFactors";		title: qsTr("Fixed Factors");		allowedColumns: 	["ordinal", "nominal"];		id: fixedFactors	}
		AssignedVariablesList	{ name: "time";				title: qsTr("Time");				allowedColumns: 	["nominal"];	singleVariable: true		}
	}

	columns: 2

	Group
	{
		title: qsTr("Output")
		columns: 1
		CheckBox
		{
			name: "posteriorSummaryTable"; label: qsTr("Posterior summary of coefficients"); id: posteriorSummaryTable
			CheckBox { name: "showCoefMeanInc"; label: qsTr("Show means across draws included") }
		}
		CIField
		{
			name: "posteriorSummaryCiLevel"
			label: qsTr("Credible interval")
			enabled: posteriorSummaryTable.checked
		}
	}

	Group
	{
		DoubleField
		{
			name: "expectedPredictors"
			label: qsTr("Expected predictors")
			defaultValue: 1
		}
	}

	Section
	{

		title: qsTr("Model Components")

		columns: 1
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList
			{
				name: "availableTerm"
				title: qsTr("Components")
				width: parent.width / 4
				source: ["covariates", "fixedFactors"]
			}
			ModelTermsList {name: "modelTerms";width: parent.width * 5 / 9}
		}
		CheckBox
		{
			name: "autoregressiveComponent"
			label: qsTr("Add autoregressive component")
			checked: false
			id: checkAr

			columns: 2

			RadioButtonGroup
			{
				columns: 1
				enabled: checkAr.checked
				name: "lagSelectionMethod"
				title: qsTr("Lag selection method")
				radioButtonsOnSameRow: true
				RadioButton
				{
					value: "manual"; label: qsTr("Manually"); checked: true
					//columns: 1
					IntegerField
					{
						name: "lags"
						label: qsTr("No. of lags")
						fieldWidth: 40
						min: 1
					defaultValue: 1
					}
				}
				RadioButton
				{
					value: "auto"; label: qsTr("Automatic")
					columns: 1
					DoubleField { name: "maxLags";	label: qsTr("Maximal lags");	fieldWidth: 40; 	defaultValue: 1; min:1}
				}
			}

			//CheckBox
			//{
			//	name: "arSdPrior"
			//	enabled: checkAr.checked
			//	label: qsTr(" Custom Stand. Dev. Prior") //not sure about the name as it is actually an 	inverse Gamma prior

			//	DoubleField { name:"arSigmaGuess";		label: "σ guess";	fieldWidth: 40;}
			//	DoubleField { name:"arSigmaWeight";		label: "Weight";	fieldWidth: 40;}
			//}
		}

		CheckBox
		{
			name: "localLevelComponent"
			label: qsTr("Add local level component")
			id: checkLocalLevel
			checked: true
			//Layout.columnSpan: 2
			//CheckBox
			//{
			//	name: "localLevelSdPrior"
			//	enabled: checkLocalLevel.checked
			//	label: qsTr(" Custom random walk SD prior") //not sure about the name as it is actually an 	inverse Gamma prior

			//	DoubleField { name:"localLevelSigmaGuess";		label: "σ guess";	fieldWidth: 40;}
			//	DoubleField { name:"localLevelSigmaWeight";		label: "Weight";	fieldWidth: 40;}
			//}
		}
		//Local Linear Trend
		// abbreviated as Llt for priors
		CheckBox
		{
			name: "localLinearTrendComponent"
			label: qsTr("Add local linear trend component")
			id: checkLocalLinearTrend
			checked: false
			//CheckBox
			//{
			//	name: "lltLevelPrior"
			//	enabled: checkLocalLinearTrend.checked
			//	label: qsTr(" Custom level SD prior")

			//	DoubleField { name:"lltLevelSigmaGuess";		label: "σ guess";	fieldWidth: 40;}
			//	DoubleField { name:"lltLevelSigmaWeight";		label: "Weight";	fieldWidth: 40;}
			//}
			//CheckBox
			//{
			//	name: "lltSlopePrior"
			//	enabled: checkLocalLinearTrend.checked
			//	label: qsTr(" Custom slope SD prior")

			//	DoubleField { name:"lltSlopeSigmaGuess";		label: "σ guess";	fieldWidth: 40;}
			//	DoubleField { name:"lltSlopeSigmaWeight";		label: "Weight";	fieldWidth: 40;}
			//}
		}
		//Semi Local Linear Trend
		CheckBox
		{
			name: "semiLocalLinearTrendComponent"
			label : qsTr("Add semi-local linear trend")
			checked: false
			id: checkSemiLocalLinearTrend
		}
		//Dynamic Regression Component
		CheckBox
		{
			name: "dynamicRegregressionComponent"
			label: qsTr("Add dynamic regression component")
			checked: false
			id: checkDynReg
			// The double field should probably be an integerfield, but it's unused in the R code at the moment
			// DoubleField { name:"dynamicRegregressionLags";		label: qsTr("Lag of coefficients");	fieldWidth: 40;}
			enabled: covariates.count > 0 || fixedFactors.count > 0
			info: qsTr("This component allows you to model the effect of covariates or fixed factors on the dependent variable. The lag of coefficients can be specified to account for delayed effects. Only available when covariates or fixed factors are present in the model.")
		}


		ComponentsList
		{
			id: seasonalities
			name: "seasonalities"
			title: qsTr("Seasonalities")
			headerLabels: [qsTr("Name"), qsTr("Number"), qsTr("Duration"), qsTr("Inverse gamma prior"), "", qsTr("Normal prior initial state")]
			rowComponent: RowLayout
			{
				width: seasonalities.width - 35 * preferencesModel.uiScale

				TextField
				{
					name: "name"
					fieldWidth: 80 * preferencesModel.uiScale
					placeholderText: "Yearly"
				}

				IntegerField
				{
					name: "number"
					fieldWidth: 45 * preferencesModel.uiScale
					defaultValue: 2
					min: 2
				}

				DoubleField
				{
					name: "duration"
					fieldWidth: 45 * preferencesModel.uiScale
					defaultValue: 1
				}

				DoubleField
				{
					name: "inverseGammaPriorSd"
					label: "σ"
					fieldWidth: 60 * preferencesModel.uiScale
					defaultValue: 0.01
					min: 0
					inclusive: JASP.MaxOnly
					afterLabel: "× sdy"
				}

				DoubleField
				{
					name: "inverseGammaPriorN"
					label: "n"
					defaultValue: 0.01
					min: 0
					inclusive: JASP.MaxOnly
					fieldWidth: 60 * preferencesModel.uiScale
					Layout.leftMargin: -15
				}
				DoubleField
				{
					name: "normalPriorMean"
					label: "μ"
					defaultValue: 0
					fieldWidth: 40 * preferencesModel.uiScale
				}
				DoubleField
				{
					name: "normalPriorSd"
					label:"σ²"
					fieldWidth: 40 * preferencesModel.uiScale
					defaultValue: 0.01
					min: 0
					inclusive: JASP.MaxOnly
					Layout.leftMargin: -10
				}
			}
		}
	}


	Section
	{

		title: qsTr("Plots")


		Group
		{
			title: qsTr("State Plots")

			CheckBox
			{
				name: "aggregatedStatesPlot"
				label: qsTr("Aggregated state contribution")

				//DropDown
				//{
				//	name: "scaleAggregatedStates"
				//	label: qsTr("Scale")
				//	values: [ "linear", "mean"]
				//}

				CIField
				{
					name: "aggregatedStatesPlotCiLevel"
					label: qsTr("Credible interval")
				}
				CheckBox
				{
					name: "aggregatedStatesPlotObservationsShown"
					label: qsTr("Show observations")
				}


			}

			CheckBox
			{
				name: "componentStatesPlot"
				label: qsTr("Component state contribution")

			}
		}

		//Group
		//{
		//	title: qsTr("Coefficients")

		//	CheckBox
		//	{
		//		name: "checkboxPlotIncProb"
		//		label: qsTr("Inclusion probability plot")
		//	}
		//	CheckBox
		//	{
		//		name: "checkboxPlotDynReg"
		//		label: qsTr("Dynamic regression plot")
		//	}
		//}
		Group
		{
			title: qsTr("Residuals")
			CheckBox {name:"residualPlot"; label: qsTr("Posterior distribution of residuals")}
			//CheckBox {name:"checkBoxForecast"; label: qsTr("Posterior distribution of one-step-ahead prediction")}
			CheckBox {name:"forecastErrorPlot"; label: qsTr("Posterior distribution of one-step-ahead prediction error")}
		}

		Group
		{
			title: qsTr("Prediction")
			DoubleField
			{
				name: "predictionHorizon"
				label: qsTr("Horizon")
				enabled: covariates.count === 0 && fixedFactors.count === 0
				max: 1000000
				inclusive: JASP.MinMax
			}
		}

		Group
		{
			title: qsTr("Control chart")
			CheckBox
			{
				name:"controlChartPlot"; label: qsTr("Show control chart")

				IntegerField {name: "controlPeriod"; label:qsTr("Control period end"); defaultValue: 100; min: 2}
				DoubleField {name: "controlSigma"; label:qsTr("σ threshold"); defaultValue: 2}
				CheckBox{name: "probalisticControlPlot";label: qsTr("Show probabilistic control plot")}

			}

		}

	}

	//Section
	//{
	//	title: qsTr("Priors")

	//	VariablesList
	//	{
	//		name: "ManualPriors"
	//		source: [ { rSource: "myRSource" } ]
	//	}
	//}

	Section

	{

		title: qsTr("Advanced Options")

		//DropDown
		//{
		//	name: "distFam"
		//	indexDefaultValue: 1
		//	label: qsTr("Distribution family")
		//	values: [ "Gaussian", "Logit","Poisson","Student"]
		//}

		DoubleField { name:"samples";		label: qsTr("Desired MCMC draws");	fieldWidth: 60; defaultValue: 2000}

		DoubleField { name:"timeout";		label: qsTr("Timeout in seconds");	fieldWidth: 60; defaultValue: 120}

		RadioButtonGroup
		{
			name: "burninMethod"
			title: qsTr("Burn-in Specification")
			radioButtonsOnSameRow: true
			RadioButton
			{
				value: "auto"; label: qsTr("Automatic suggestion"); checked: true
				DoubleField { name:"automaticBurninProportion"
											label: qsTr("Proportion")
											fieldWidth: 60
											defaultValue: 0.1
											min:0
											max: 0.999
											}
			}
			RadioButton
			{
				value: "manual"; label: qsTr("Manual")
				DoubleField { name:"manualBurninAmount"
											label: qsTr("Number")
											fieldWidth: 60
											defaultValue: 0

											}
			}
		}
		DoubleField { name:"seed";		label: qsTr("Seed");	fieldWidth: 60; defaultValue: 1}

	}
}
