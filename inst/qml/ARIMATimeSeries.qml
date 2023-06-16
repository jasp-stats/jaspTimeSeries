import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
    VariablesForm
    {
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList  
        {
            name: "dependent"
            label: qsTr("Dependent Variable")
            allowedColumns: ["ordinal", "scale"]
            singleVariable: true
        }
        AssignedVariablesList  
        {
            name: "time"
            label: qsTr("Time")
            allowedColumns: ["ordinal", "scale"]
            singleVariable: true
        }
        AssignedVariablesList  
        {
            name: "covariates"
            label: qsTr("Covariates")
            allowedColumns: ["ordinal", "scale"]
            height: 120
        }
    }
    
    Group
    {
        CheckBox
        {
            name: "timeSeriesPlot"
            id:    tsPlot
            label: qsTr("Plot time series")
            checked: true
            RadioButtonGroup
            {
                name:	"timeSeriesPlotType"
                radioButtonsOnSameRow: true
                RadioButton { value: "points";	label: qsTr("Points") }
                RadioButton { value: "line";	label: qsTr("Line") }
                RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            }
            RadioButtonGroup
            {
                name:	"timeSeriesPlotDistribution"
                title: qsTr("Distribution")
                RadioButton { value: "density";	label: qsTr("Density") }
                RadioButton { value: "histogram";	label: qsTr("Histogram") }
                RadioButton { value: "none";	label: qsTr("None");	checked: true }
            }
        }
    }

    Section
    {
        title: qsTr("Model")
        Group
        {
            CheckBox
            {
                name:       "intercept"
                id:         intercept
                label:      qsTr("Include intercept")
                checked:    true
                enabled:    best.checked | (manual.checked & (d.value < 2 | D.value < 2))
            }
            CheckBox
            {
                name:   "seasonal"
                id:     seasonal
                label:  qsTr("Add seasonal components")
                RadioButtonGroup
                {
                    name: "periodSpecification"
                    title: qsTr("Period")
                    radioButtonsOnSameRow: true
                    RadioButton 
                    { 
                        value: "manual"
                        label: qsTr("Specify manually")
                        checked: true
                        childrenOnSameRow: true
                        IntegerField { name: "m";   id: m; defaultValue: 1; min: 1 }
                    }
                    RadioButton { value: "dominant";  label: qsTr("Find dominant period") }
                }
            }
        
            RadioButtonGroup
            {
                name: "modelSpecification"
                title: qsTr("Model Specification")
                RadioButton 
                {
                    value: "auto"
                    id: auto
                    label: qsTr("Best fitting")	
                    checked: true 
                    RadioButtonGroup
                    {
                        name: "autoIc"
                        title: qsTr("Information criterion")
                        radioButtonsOnSameRow: true
                        RadioButton { value: "aicc";    label: qsTr("AICc");	checked: true   }
                        RadioButton { value: "aic";	    label: qsTr("AIC")				        }
                        RadioButton { value: "bic";	    label: qsTr("BIC")				        } 
                    }
                }
                RadioButton 
                { 
                    value: "manual"	
                    id: manual
                    label: qsTr("Manual")
                    Group
                    {
                        columns: 2
                        Group
                        {
                            title: qsTr("Nonseasonal")
                            IntegerField
                            {
                                name:   "p"
                                id:     p
                                label:  qsTr("Autoregressive (AR) order p")
                            }
                            IntegerField
                            {
                                name:   "d"
                                id:     d
                                label:  qsTr("Difference (I) degree d")
                            }
                            IntegerField
                            {
                                name:   "q"
                                id:     q
                                label:  qsTr("Moving average (MA) order q")
                            }
                        }
                        Group
                        {
                            title: qsTr("Seasonal")
                            enabled: seasonal.checked
                            IntegerField
                            {
                                name:   "P"
                                id:     sP
                                label:  qsTr("Autoregressive (AR) order P")
                            }
                            IntegerField
                            {
                                name:   "D"
                                id:     sD
                                label:  qsTr("Difference (I) degree D")
                            }
                            IntegerField
                            {
                                name:   "Q"
                                id:     sQ
                                label:  qsTr("Moving average (MA) order Q")
                            }
                        }
                    }	  
                }
            }
        }
    }

    Section
    {
        title: qsTr("Residual Diagnostics")
        CheckBox
        {
            name:   "residualPlots"
            id:     residualPlots
            label:  qsTr("Residual Plots")
            CheckBox
            {
                name:       "residualTimeSeries"
                id:         residualTimeSeries
                label:      qsTr("Time series")
                checked:    true
                RadioButtonGroup
                {
                    name:	"residualTimeSeriesType"
                    radioButtonsOnSameRow: true
                    RadioButton { value: "points";	label: qsTr("Points") }
                    RadioButton { value: "line";	label: qsTr("Line") }
                    RadioButton { value: "both";	label: qsTr("Both");	checked: true }
                }
                RadioButtonGroup
                {
                    name:	"residualTimeSeriesDistribution"
                    title: qsTr("Distribution")
                    RadioButton { value: "density";	label: qsTr("Density") }
                    RadioButton { value: "histogram";	label: qsTr("Histogram") }
                    RadioButton { value: "none";	label: qsTr("None");	checked: true }
                }
            }
            CheckBox
            {
                name:   "residualQQ"
                id:     residualQQ
                label:  qsTr("Q-Q plot")
            }
            Group
            {
                title: qsTr("Autocorrelation")
                CheckBox
                {
                    name:   "residualAcf"
                    id:     residualACF
                    label:  qsTr("Autocorrelation function")
                    CheckBox
                    {
                        name: "residualAcfCi"
                        label: qsTr("Show confidence interval")
                        checked: true
                        childrenOnSameRow: true
                        CIField { name: "residualAcfCiLevel" }
                    }
                    CheckBox { name: "residualAcfZeroLag"; label: qsTr("Include zero lag") }
                }
                CheckBox
                {
                    name:   "residualLjungBox"
                    id:     residualLjungBox
                    label:  qsTr("Ljung-Box p-values")
                    CIField { name: "ljungBoxSignificanceLevel"; label: qsTr("Significance level"); defaultValue: 5 }
                }
                IntegerField 
                {
                    name: "residualMaxLag"
                    label: qsTr("Maximum lag")
                    min: 1
                    defaultValue: 10
                    enabled: residualACF.checked | residualLjungBox.checked
                }
            }
        }
        CheckBox 
        {
          id:							residualSavedToData
          name:						"residualSavedToData"
          text:						qsTr("Append residuals to spreadsheet")

          ComputedColumnField 
          {
            id:						    residualColumn
            name:					    "residualColumn"
            text:					    qsTr("Column name")
            placeholderText:  qsTr("e.g., residuals")
            fieldWidth:				120
            enabled:				  residualSavedToData.checked
          }
        }
    }

    Section
    {
        title: qsTr("Forecasting")
        CheckBox
        {
          name: "forecast"
          label: qsTr("Forecast")
          IntegerField
          {
            name: "forecastLength"
            label: qsTr("Number of forecasts")
            min: 1
            defaultValue: 10
          }
          CheckBox
            {
                name:     "forecastTimeSeries"
                id:       forecastTimeSeries
                label:    qsTr("Time series plot")
                checked:  true
                RadioButtonGroup
                {
                    name:	"forecastTimeSeriesType"
                    radioButtonsOnSameRow: true
                    RadioButton { value: "points";	label: qsTr("Points") }
                    RadioButton { value: "line";	label: qsTr("Line") }
                    RadioButton { value: "both";	label: qsTr("Both");	checked: true }
                }
                CheckBox
                {
                    name:       "forecastTimeSeriesObserved"
                    id:         forecastTimeSeriesObserved
                    label:      qsTr("Include observed data")
                    checked:    true
                }
            }
            CheckBox
            {
                name:   "forecastTable"
                id:     forecastTable
                label:  qsTr("Forecasts table")
            }
        
            FileSelector
            {
                name:	             "saveforecast"
                label:	            qsTr("Save forecasts as")
                placeholderText:    qsTr("e.g. forecasts.csv")
                filter:	            "*.csv"
                save:	              true
                fieldWidth:         180 * preferencesModel.uiScale
            }
        }
    }
}
// dataSetModel.rowCount()
