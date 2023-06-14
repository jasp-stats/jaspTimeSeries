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
        // AssignedVariablesList  
        // {
        //     name: "time"
        //     label: qsTr("Time")
        //     allowedColumns: ["ordinal", "scale"]
        //     singleVariable: true
        // }
    }

    Group
    {
        title: qsTr("Spectral density")
        CheckBox
        {
            name: "powerSpectralDensityDetrend"
            label: qsTr("Detrend")
            checked: true
        }
        CheckBox
        {
            name: "powerSpectralDensityDemean"
            label: qsTr("Demean")
            checked: false
        }
        CheckBox
        {
            name: "powerSpectralDensitySmoother"
            label: qsTr("Add kernel smoother")
            childrenOnSameRow: true
            DropDown
            {
                name: "powerSpectralDensitySmootherKernel"
                values:
                [
                    { label: qsTr("Daniell"), value: "daniell" },
                    { label: qsTr("Modified Daniell"), value: "modified.daniell" }
                ]
            }
            Group
            {
                RowLayout
                {
                  Label
                  {
                    text:						qsTr("Dimension")
                    Layout.leftMargin:			130 * preferencesModel.uiScale
                    Layout.preferredWidth:	70 * preferencesModel.uiScale
                  }
                }

                ComponentsList
                {
                  name:							"term"
                  defaultValues:		[1]
                  minimumItems:			1
                  rowComponent:			RowLayout
                  {
                    Row
                    {
                      spacing:				4 * preferencesModel.uiScale
                      Layout.preferredWidth:	110 * preferencesModel.uiScale

                      Label
                      {
                        text:				qsTr("Term ") + (rowIndex + 1)
                      }
                    }
                    
                    RowLayout
                    {
                      spacing:				4 * preferencesModel.uiScale
                      Layout.preferredWidth:	50 * preferencesModel.uiScale
                      
                      IntegerField
                      {
                        id:					  dimension
                        name:				  "dimension"
                        useExternalBorder:	true
                        min:				    1
                        defaultValue:		1
                      }
                    }
                  }
                } 
            }
        }
        DoubleField
        {
            name: "powerSpectralDensityTaper"
            label: qsTr("Taper")
            min: 0
            max: 0.5
        }
        RadioButtonGroup
        {
            name: "powerSpectralDensityScaling"
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
// dataSetModel.rowCount()
