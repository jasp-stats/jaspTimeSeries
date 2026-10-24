import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
    info: qsTr("Allows the user to fit a (seasonal) AR/MA/ARMA/ARIMA model to a time series.")
    VariablesForm
    {
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList
        {
            name: "dependent"
            label: qsTr("Dependent Variable")
            allowedColumns: ["scale"]
            singleVariable: true
            info: qsTr("A variable that is measured repeatedly over time.")
        }
        AssignedVariablesList
        {
            name: "time"
            id: time
            label: qsTr("Time")
            allowedColumns: ["scale", "ordinal", "nominal"]
            singleVariable: true
            info: qsTr("Optional. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use 'YYYY-MM-DD' for dates or 'YYYY-MM-DD HH:MM:SS' for dates with times; seconds can be omitted. All variables stay aligned, and spreadsheet row order is unchanged. Tied values retain spreadsheet row order. If any value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time.")
        }
        AssignedVariablesList
        {
            name: "covariates"
            label: qsTr("Covariates")
            allowedColumns: ["scale"]
            height: 120
            info: qsTr("Optional numeric variables to include as external regressors. Forecasting requires covariate values in the next rows of the ordered dataset after the final row used for fitting. Use a filter to end the fitted series before those rows; missing dependent values alone do not exclude a row from fitting.")
        }
    }
    
    Group
    {
        CheckBox
        {
            name: "filter"
            id: filter
            label: qsTr("Filter by")
            info: qsTr("Selects an inclusive range of observations for analysis. Row number refers to original spreadsheet rows, even when Time changes the analysis order. Time index filters numeric Time values, including negative values and decimals. Date filters interpreted dates and times. Date filters use UTC. A date-only End includes the entire day; an End with a time uses that exact cutoff. Time or date filtering requires a usable Time variable; otherwise, select Row number or disable filtering.")
            RadioButtonGroup
            {
                name: "filterBy"
                RadioButton
                {
                    value: "row"
                    id: filterRow
                    label: qsTr("Row number")
                    checked: true
                    Group
                    {
                        columns: 2
                        IntegerField { name: "rowStart"; label: qsTr("Start"); defaultValue: 1 }
                        IntegerField { name: "rowEnd"; label: qsTr("End"); defaultValue: 100 }
                    }
                }
                RadioButton
                {
                    value: "time"
                    id: filterTime
                    label: qsTr("Time index")
                    enabled: time.count != 0
                    Group
                    {
                        columns: 2
                        DoubleField { name: "timeStart"; label: qsTr("Start"); defaultValue: 1; negativeValues: true; decimals: 10 }
                        DoubleField { name: "timeEnd"; label: qsTr("End"); defaultValue: 100; negativeValues: true; decimals: 10 }
                    }
                }
                RadioButton
                {
                    value: "date"
                    id: filterDate
                    label: qsTr("Date")
                    enabled: time.count != 0
                    Group
                    {
                        TextField
                        {
                            name: "dateStart"
                            label: qsTr("Start")
                            placeholderText: "YYYY-MM-DD HH:MM:SS"
                            fieldWidth: 150
                        }
                        TextField
                        {
                            name: "dateEnd"
                            label: qsTr("End")
                            placeholderText: "YYYY-MM-DD HH:MM:SS"
                            fieldWidth: 150
                        }
                    }
                }
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
                label:      qsTr("Intercept")
                checked:    true
                enabled:    best.checked | (manual.checked & (d.value < 2 | D.value < 2))
                info:       qsTr("Include an intercept in the model.")
            }
            CheckBox
            {
                name:   "seasonal"
                id:     seasonal
                label:  qsTr("Seasonal components")
                info:   qsTr("Fit a seasonal ARIMA model. If the frequency is not known, a dominant frequency may be determined from a spectral analysis.")
                RadioButtonGroup
                {
                    name: "periodSpecification"
                    title: qsTr("Period")
                    radioButtonsOnSameRow: true
                    RadioButton
                    {
                        value: "custom"
                        label: qsTr("Custom")
                        checked: true
                        childrenOnSameRow: true
                        IntegerField { name: "m";   id: m; defaultValue: 1; min: 1 }
                    }
                    RadioButton { value: "dominant";  label: qsTr("Dominant") }
                }
            }

            RadioButtonGroup
            {
                name:   "modelSpecification"
                title:  qsTr("Model Specification")
                info:   qsTr("Specifies what ARIMA model to fit. The best fitting model is determined by the Hyndman-Khandakar algorithm.")
                RadioButton
                {
                    value: "auto"
                    id: auto
                    label: qsTr("Best fitting")
                    checked: true
                    RadioButtonGroup
                    {
                        name: "modelSpecificationAutoIc"
                        title: qsTr("Information criterion")
                        radioButtonsOnSameRow: true
                        RadioButton { value: "aicc";    label: qsTr("AICc");	checked: true   }
                        RadioButton { value: "aic";	    label: qsTr("AIC")				        }
                        RadioButton { value: "bic";	    label: qsTr("BIC")				        }
                    }
                }
                RadioButton
                {
                    value: "custom"
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
        title: qsTr("Plots")
        Group
        {
            CheckBox
            {
                name: "timeSeriesPlot"
                id:    tsPlot
                label: qsTr("Observed time series")
                checked: true
                info: qsTr("Plots the dependent variable (y-axis) over time (x-axis).")
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
    }

    Section
    {
        title: qsTr("Residual Diagnostics")
        Group
        {
            title:  qsTr("Plots")
            CheckBox
            {
                name:       "residualTimeSeries"
                id:         residualTimeSeries
                label:      qsTr("Time series plot")
                info:       qsTr("Plots the residuals (y-axis) over time (x-axis).")
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
                info:   qsTr("Plots the quantile-quantile plot of the residuals.")
            }
            Group
            {
                title: qsTr("Autocorrelation")
                CheckBox
                {
                    name:   "residualAcf"
                    id:     residualACF
                    label:  qsTr("Autocorrelation function")
                    info:   qsTr("Plots the autocorrelation for a specified number of lags. The confidence interval may be given assuming a white noise process.")
                    CheckBox { name: "residualAcfZeroLag"; label: qsTr("Zero lag") }
                    CheckBox
                    {
                        name: "residualAcfCi"
                        label: qsTr("Confidence interval")
                        checked: true
                        childrenOnSameRow: true
                        CIField { name: "residualAcfCiLevel" }
                    }
                }
                CheckBox
                {
                    name:   "residualLjungBox"
                    id:     residualLjungBox
                    label:  qsTr("Ljung-Box p-values")
                    info:   qsTr("Plots the p-values of the Ljung-Box test for a number of lags of which the null hypothesis assumes that the data independently distributed and therefore have no autocorrelation.")
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
            id:		residualSavedToData
            name:	"residualSavedToData"
            text:	qsTr("Append residuals to spreadsheet")
            info: qsTr("Appends the residuals to the spreadsheet, so these can be used in further analyses.")
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
        IntegerField
        {
            name: "forecastLength"
            id: forecastLength
            label: qsTr("Number of forecasts")
            min: 0
            max: 1e6
            defaultValue: 0
            info: qsTr("Number of observation steps to forecast after the final row included in the model, including rows with missing dependent values. A value of 0 produces no forecasts. When Time is assigned, the table reports steps ahead; future Time values or dates are not inferred.")
        }
        FileSelector
        {
            name:	             "forecastSave"
            label:	            qsTr("Save forecasts as")
            placeholderText:    qsTr("e.g. forecasts.csv")
            filter:	            "*.csv"
            save:	              true
            enabled:            forecastLength.value > 0
            fieldWidth:         180 * preferencesModel.uiScale
            info:               qsTr("Saves the forecasts in a separate .csv file. When Time is assigned, the first column, stepAhead, counts steps after the final modeled row. When Time is empty, the first column, t, continues the row index.")
        }
        CheckBox
        {
            name:     "forecastTimeSeries"
            id:       forecastTimeSeries
            label:    qsTr("Time series plot")
            info:     qsTr("Plots forecasts and optionally observed values. With Time assigned, the x-axis counts observation steps in model order and continues beyond the final modeled row; future Time values or dates are not inferred. With Time empty, t retains and continues the row index.")
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
                label:      qsTr("Observed data")
                checked:    true
            }
        }
        CheckBox
        {
            name:   "forecastTable"
            id:     forecastTable
            label:  qsTr("Forecasts table")
            info:   qsTr("Reports forecast means and intervals. With Time assigned, Steps ahead counts from 1 after the final modeled row. With Time empty, t continues the row index.")
        }
    }
}
