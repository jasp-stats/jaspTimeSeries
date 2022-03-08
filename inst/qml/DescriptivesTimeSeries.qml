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
            name: "dependentVariable"
            label: qsTr("Dependent Variable")
            allowedColumns: ["ordinal", "scale"]
            singleVariable: true
        }
        AssignedVariablesList  
        {
            name: "timeVariable"
            label: qsTr("Time")
            allowedColumns: ["ordinal", "scale"]
            singleVariable: true
        }
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
                radioButtonsOnSameRow: true
                RadioButton { value: "points";	label: qsTr("Points") }
                RadioButton { value: "line";	label: qsTr("Line") }
                RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            }
            RadioButtonGroup
            {
                name:	"distribution"
                title: qsTr("Distribution")
                // radioButtonsOnSameRow: true
                RadioButton { value: "density";	label: qsTr("Density") }
                RadioButton { value: "histogram";	label: qsTr("Histogram") }
                RadioButton { value: "none";	label: qsTr("None");	checked: true }
            }
        }
        CheckBox
        {
            name: "stateSpacePlot"
            id: sspPlot
            label: qsTr("State space plot")
            Group
            {
                columns: 2
                IntegerField
                {
                    name: "lag"
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
                        radioButtonsOnSameRow: true
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
        }
        CheckBox
        {
            name: "acfPlots"
            id: acf
            label: qsTr("Autocorrelation functions")
            CheckBox
            {
                name: "acfCI"
                label: qsTr("Show confidence interval")
                checked: true
                childrenOnSameRow: true
                CIField { name: "acfCIValue" }
            }
            IntegerField { name: "acfMax"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
        }
        CheckBox
        {
            name: "powerSpectralDensity"
            id: psd
            label: qsTr("Power spectral density")
            Group
            {
                columns: 2
                CheckBox
                {
                    name: "detrend"
                    label: qsTr("Detrend")
                    checked: true
                }
                CheckBox
                {
                    name: "demean"
                    label: qsTr("Demean")
                    checked: false
                }
            }
            CheckBox
            {
                name: "smoothing"
                label: qsTr("Add kernel smoother")
                childrenOnSameRow: true
                DropDown
                {
                    name: "kernel"
                    values:
                    [
                        { label: qsTr("Daniell"), value: "daniell" },
                        { label: qsTr("Modified Daniell"), value: "modified.daniell" }
                    ]
                }
                Group
                {
                    columns: 2
                    IntegerField
                    {
                        name: "m1"
                        label: qsTr("Dimensions")
                    }
                    IntegerField
                    {
                        name: "m2"
                    }
                }
            }
            DoubleField
            {
                name: "taper"
                label: qsTr("Taper")
                min: 0
                max: 0.5
            }
            RadioButtonGroup
            {
                name: "scaling"
                title: qsTr("Scaling")
                radioButtonsOnSameRow: true
                RadioButton
                {
                    name: "noScaling"
                    label: qsTr("None")
                    checked: true
                }
                RadioButton
                {
                    name: "log"
                    label: qsTr("ln")
                }
                RadioButton
                {
                    name: "log10"
                    label: qsTr("log")
                }
            }
        }
    }
}
// dataSetModel.rowCount()
