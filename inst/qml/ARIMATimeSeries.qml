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
            allowedColumns: ["ordinal", "nominal"]
            singleVariable: true
            info: qsTr("Optional. Can either be an ordinal variable indicating the time index/order of the observations, or a text variable indicating the date/time stamp of the observations. Combined date and time values should be in the standard format 'YYYY-MM-DD HH:MM:SS', where seconds (':SS') can also be omitted. Date-only values should be in the format 'YYYY-MM-DD'. If a time variable is not supplied, the row order of the data is used.")
        }
        AssignedVariablesList
        {
            name: "covariates"
            label: qsTr("Covariates")
            allowedColumns: ["scale"]
            height: 120
            info: qsTr("Optional. A numerical variable to include in the model as external regressor.")
        }
    }
    
    Group
    {
        CheckBox
        {
            name: "timeSeriesPlot"
            id:    tsPlot
            label: qsTr("Time series plot")
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
    Group
    {
        CheckBox
        {
            name: "filter"
            id: filter
            label: qsTr("Filter by")
            info: qsTr("Filters the time series so only a specific range will be used for further analyses. Row number refers to the row number in the spreadsheet. If a 'Time' variable is supplied it is also possible to filter by time index or date, depending on the format of the 'Time' variable.")
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
                        IntegerField { name: "timeStart"; label: qsTr("Start"); defaultValue: 1 }
                        IntegerField { name: "timeEnd"; label: qsTr("End"); defaultValue: 100 }
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
            info: qstr("Appends the residuals to the spreadsheet, so these can be used in further analyses.")
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
            info: qsTr("Determines the number forecasts to make.")
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
            info:               qsTr("Saves the forecasts in a seperate .csv file.")
        }
        CheckBox
        {
            name:     "forecastTimeSeries"
            id:       forecastTimeSeries
            label:    qsTr("Time series plot")
            info:     qsTr("Plots the forecasts (and observed values) (y-axis) over time (x-axis)")
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
        }
    }
}
