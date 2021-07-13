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
            label: qsTr("Time series plot")
        }
        CheckBox
        {
            name: "stateSpacePlot"
            label: qsTr("State space plot")
            CheckBox
            {
                name: "addSmooth"
                label: qsTr("Add regression line")
                checked: true
                RadioButtonGroup
                {
                    name:	"regressionType";
                    RadioButton { value: "smooth";	label: qsTr("Smooth");	checked: true }
                    RadioButton { value: "linear";	label: qsTr("Linear")				  }
                }

                CheckBox
                {
                    name: "addSmoothCI"
                    label: qsTr("Show confidence interval")
                    checked: true
                    childrenOnSameRow: true
                    CIField { name: "addSmoothCIValue" }
                }
            }
        }
        CheckBox
        {
            name: "acf"
            label: qsTr("Autocorrelation function")
            CheckBox
            {
                name: "addLinesCI"
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

