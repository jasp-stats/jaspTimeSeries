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
            label: qsTr("Time Series Plot")
        }
        CheckBox
        {
            name: "stateSpacePlot"
            label: qsTr("State Space Plot")
        }
        CheckBox
        {
            name: "ACF"
            label: qsTr("Autocorrelation Function")
        }
        CheckBox
        {
            name: "powerSpectralDensity"
            label: qsTr("Power Spectral Density")
        }
    }
}

