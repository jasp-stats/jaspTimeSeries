import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
    info: qsTr("Stationarity allows the user to test a univariate time-series for stationarity and to transform the time-series if necessary.")
    VariablesForm
    {
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList
        {
            name: "dependent"
            label: qsTr("Variable")
            allowedColumns: ["ordinal", "scale"]
            singleVariable: true
            allowAnalysisOwnComputedColumns: false
            info: qsTr("A variable that is measured repeatedly over time.")
        }
        AssignedVariablesList
        {
            name: "time"
            label: qsTr("Time")
            allowedColumns: ["ordinal", "nominalText"]
            singleVariable: true
            allowAnalysisOwnComputedColumns: false
            info: qsTr("Optional. Can either be an ordinal variable indicating the order of the observations, or a text variable indicating the date/time stamp of the observations. Combined date and time values should be in the standard format 'YYYY-MM-DD HH:MM:SS', where seconds (':SS') can also be omitted. Date-only values should be in the format 'YYYY-MM-DD'. If a time variable is not supplied, the row order of the data is used.")
        }
    }

    Group
    {
        title: qsTr("Stationarity Tests")
        CheckBox
        {
            name:   "adfTest"
            id:     adfTest
            label:  qsTr("Augmented Dickey-Fuller")
            info:   qsTr("Computes an ADF test where the null assumes that the time-series has a unit root. The p-values are interpolated from a table of critical values in Banerjee et al. (1993), when the statistic lies outside the range of critical values, a note is added to the table.")
        }
        Group
        {
            title: qsTr("Phillips-Perron")
            columns: 2
            info:  qsTr("Computes a PP test where the null assumes that the time-series has a unit root. The p-values are interpolated from a table of critical values in Banerjee et al. (1993), when the statistic lies outside the range of critical values, a note is added to the table.")
            CheckBox { name: "ppTestRegressionCoefficient"; id: ppRegression;	label: qsTr("Regression coefficient test") }
            CheckBox { name: "ppTestStudentized"; id: ppStudentized;	label: qsTr("Studentized test") }

        }
        Group
        {
            title: qsTr("Kwiatkowski-Phillips-Schmidt-Shin")
            columns: 2
            info:   qsTr("Computes a KPSS test where the null assumes that the time-series is level or trend stationary. The p-values are interpolated from a table of critical values in Kwiatkowski et al. (1992), when the statistic lies outside the range of critical values, a note is added to the table.")
            CheckBox { name: "kpssLevel"; id: kpssLevel;	label: qsTr("Level stationary") }
            CheckBox { name: "kpssTrend"; id: kpssTrend;	label: qsTr("Trend stationary") }
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
        title: qsTr("Transformation")
        Group
        {
            CheckBox
            {
                name: "log"
                id: log
                label: qsTr("Log")
                childrenOnSameRow: true
                info: qsTr("Takes the log of the dependent variable.")
                RadioButtonGroup
                {
                    name:	"logBase"
                    radioButtonsOnSameRow: true
                    RadioButton { value: "10";	label: qsTr("Base 10"); checked: true }
                    RadioButton { value: "e";	label: qsTr("Base e") }
                }
            }
            CheckBox
            {
                name: "root"
                id: root
                label: qsTr("Root")
                childrenOnSameRow: true
                info: qsTr("Takes the root of the dependent variable.")
                RadioButtonGroup
                {
                    name:	"rootIndex"
                    radioButtonsOnSameRow: true
                    RadioButton { value: "square";	label: qsTr("Square"); checked: true }
                    RadioButton { value: "cube";	label: qsTr("Cube") }
                }
            }
            CheckBox
            {
                name: "boxCox"
                id: boxCox
                label: qsTr("Box-Cox")
                info: qsTr("Transforms the dependent variable using a Box-Cox transformation. Lambda is the transformation parameter. If lambda is chosen automatically, it minimizes the coefficient of variation for the dependent variable using Guerrero's method.")
                RadioButtonGroup
                {
                    name:	"boxCoxLambdaSpecification"
                    title: qsTr("Lambda")
                    enabled: boxCox.checked
                    radioButtonsOnSameRow: true
                    RadioButton
                    {
                        value: "auto"
                        id: auto
                        label: qsTr("Auto")
                        checked: true
                    }
                    RadioButton
                    {
                        value: "custom"
                        id: manual
                        label: qsTr("Custom")
                        childrenOnSameRow: true
                        DoubleField { name: "boxCoxLambda"; defaultValue: 0; min: -5; max: 5; }
                    }
                }
            }
            CheckBox
            {
                name: "detrend"
                id: detrend
                label: qsTr("Detrend using linear regression")
                info: qsTr("Fits a polynomial regression to the dependent variable with time as predictor, and keeps only the residuals. If best fitting is selected, the polynomial regression is chosen based on the information criterion.")
                RadioButtonGroup
                {
                    name: "polynomialSpecification"
                    title: qsTr("Polynomial")
                    RadioButton
                    {
                        value: "auto"
                        label: qsTr("Best fitting")
                        checked: true
                        IntegerField
                        {
                            name: "polynomialSpecificationAutoMax"
                            label: qsTr("Maximum")
                            defaultValue: 5
                            min: 1
                            max: 10
                        }
                        RadioButtonGroup
                        {
                            name: "polynomialSpecificationAutoIc"
                            title: qsTr("Information criterion")
                            radioButtonsOnSameRow: true
                            RadioButton { value: "aic"; label: qsTr("AIC"); checked: true }
                            RadioButton { value: "bic";	label: qsTr("BIC")				        }
                        }
                    }
                    RadioButton
                    {
                        value: "custom"
                        label: qsTr("Custom")
                        childrenOnSameRow: true
                        IntegerField { name: "detrendPoly"; defaultValue: 1; min: 1; max: 10; }
                    }
                }
            }
            CheckBox
            {
                name: "difference"
                id: difference
                label: qsTr("Difference")
                childrenOnSameRow: true
                info: qsTr("Differences the dependent variable. Note that differencing leads to a shorter time-series (n - lag) as a lag is used.")
                IntegerField { name: "differenceLag"; label: qsTr("Lag"); defaultValue: 1; min: 1; }
                IntegerField { name: "differenceOrder"; label: qsTr("Order"); defaultValue: 1; min: 1; }
            }

        }

        CheckBox
        {
            id:     transformationSavedToData
            name:		"transformationSavedToData"
            text:		qsTr("Append transformation to spreadsheet")
            info:  qsTr("Appends the transformed dependent variable to the spreadsheet, so these can be used in further analyses.")
            ComputedColumnField
            {
                id:						    transformationColumn
                name:					    "transformationColumn"
                text:					    qsTr("Column name")
                placeholderText:  qsTr("e.g., transformed")
                fieldWidth:				120
                enabled:				  transformationSavedToData.checked
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
                label: qsTr("Time series plot")
                checked: true
                info: qsTr("Plots the (transformed) dependent variable (y-axis) over time (x-axis).")
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

            CheckBox
            {
                name: "acf"
                id: acf
                label: qsTr("Autocorrelation function")
                info: qsTr("Plots the autocorrelation for a specified number of lags. The confidence interval may be given assuming either a white noise process, or assuming for a lag q a moving average process of order q - 1")
                IntegerField { name: "acfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
                CheckBox { name: "acfZeroLag"; label: qsTr("Zero lag") }
                CheckBox
                {
                    name: "acfCi"
                    id: acfCi
                    label: qsTr("Confidence interval")
                    checked: true
                    childrenOnSameRow: true
                    CIField { name: "acfCiLevel" }
                }
                RadioButtonGroup
                {
                    name: "acfCiType"
                    enabled: acfCi.checked
                    Layout.leftMargin: 25 * preferencesModel.uiScale
                    RadioButton { value: "whiteNoise";	label: qsTr("Based on white noise");	checked: true }
                    RadioButton { value: "movingAverage";	label: qsTr("Based on moving average")	}
                }
            }
            CheckBox
            {
                name: "pacf"
                id: pacf
                label: qsTr("Partial autocorrelation function")
                info: qsTr("Plots the partial autocorrelation for a specified number of lags.")
                IntegerField { name: "pacfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
                CheckBox
                {
                    name: "pacfCi"
                    label: qsTr("Confidence interval")
                    checked: true
                    childrenOnSameRow: true
                    CIField { name: "pacfCiLevel" }
                }
            }
        }
    }
}
