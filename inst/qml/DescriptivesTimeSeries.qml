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
            label: qsTr("Variable")
            allowedColumns: ["ordinal", "scale"]
            singleVariable: true
        }
        AssignedVariablesList  
        {
            name: "time"
            label: qsTr("Time")
            allowedColumns: ["ordinal", "nominalText"]
            singleVariable: true
        }
    }

    Group
    {
      CheckBox
      {
          name: "descriptivesTableTransposed"
          label: qsTr("Transpose descriptives table")
      }
    }

    Section
    {
        title: qsTr("Plots")
        columns: 2
        CheckBox
        {
            name: "timeSeriesPlot"
            id:    tsPlot
            label: qsTr("Time series plot")
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
        CheckBox
        {
            name: "lagPlot"
            id: sspPlot
            label: qsTr("Lag plot")
            Group
            {
                IntegerField
                {
                    name: "lagPlotLag"
                    label: qsTr("Lag")
                    defaultValue: 1
                    min: 1
                }
                CheckBox
                {
                    name: "lagPlotRegressionLine"
                    id: sspSmooth
                    label: qsTr("Add regression line")
                    checked: true
                    RadioButtonGroup
                    {
                        name:	"lagPlotRegressionType"
                        radioButtonsOnSameRow: true
                        RadioButton { value: "smooth";	label: qsTr("Smooth");	checked: true }
                        RadioButton { value: "linear";	label: qsTr("Linear")	}
                    }

                    CheckBox
                    {
                        name: "lagPlotRegressionCi"
                        label: qsTr("Confidence interval")
                        checked: true
                        childrenOnSameRow: true
                        CIField { name: "lagPlotRegressionCiLevel" }
                    }
                }
            }
        }
        CheckBox
        {
            name: "acf"
            id: acf
            label: qsTr("Autocorrelation function")
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
// dataSetModel.rowCount()
