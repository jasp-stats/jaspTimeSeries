import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
    VariablesForm
    {
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList  { name: "dependentVariable"; label: qsTr("Dependent Variable"); allowedColumns: ["ordinal", "scale"] }
    }

    Group
    {
        title: qsTr("Plots")
        CheckBox
        {
            name: "timeSeriesPlot"
            id:    tsPlot
            label: qsTr("Time series plot")
            checked: true
            RadioButtonGroup
            {
                name:	"tsType"
                visible: tsPlot.checked
                radioButtonsOnSameRow: true
                RadioButton { value: "points";	label: qsTr("Points")}
                RadioButton { value: "line";	label: qsTr("Line")}
                RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            }
        }
        CheckBox
        {
            name: "stateSpacePlot"
            id: sspPlot
            label: qsTr("State space plot")
            Group
            {
                visible: sspPlot.checked
                columns: 2
                IntegerField
                {
                    name: "sspLag"
                    label: qsTr("Lag")
                    defaultValue: 1
                }
                CheckBox
                {
                    name: "addSmooth"
                    id: sspSmooth
                    label: qsTr("Add regression line")
                    checked: true
                    RadioButtonGroup
                    {
                        name:	"regressionType"
                        visible: sspSmooth.checked
                        radioButtonsOnSameRow: true
                        RadioButton { value: "smooth";	label: qsTr("Smooth");	checked: true }
                        RadioButton { value: "linear";	label: qsTr("Linear")				  }
                    }

                    CheckBox
                    {
                        name: "addSmoothCI"
                        visible: sspSmooth.checked
                        label: qsTr("Show confidence interval")
                        checked: true
                        childrenOnSameRow: true
                        CIField { name: "addSmoothCIValue" }
                    }
                }
            }
        }
        CheckBox
        {
            name: "acfPlot"
            id: acf
            label: qsTr("Autocorrelation function")
            CheckBox
            {
                name: "addLinesCI"
                visible: acf.checked
                label: qsTr("Show confidence interval")
                checked: true
                childrenOnSameRow: true
                CIField { name: "addLinesCIValue" }
            }
        }
        CheckBox
        {
            name: "powerSpectralDensity"
            label: qsTr("Power spectral density")
        }
    }
}
// dataSetModel.rowCount()
