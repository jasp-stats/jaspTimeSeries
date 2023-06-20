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
            name: "kernel"
            id: kernel
            label: qsTr("Kernel smoother")
            childrenOnSameRow: true
            DropDown
            {
                name: "kernelMethod"
                values:
                [
                    { label: qsTr("Daniell"), value: "daniell" },
                    { label: qsTr("Modified Daniell"), value: "modified.daniell" }
                ]
            }
        }
        Group
        {
            Layout.leftMargin:			25 * preferencesModel.uiScale
            enabled: kernel.checked
            RowLayout
            {
              Label
              {
                text:						        qsTr("Dimension")
                Layout.leftMargin:			130 * preferencesModel.uiScale
                Layout.preferredWidth:	70 * preferencesModel.uiScale
              }
            }

            ComponentsList
            {
              name:							"kernelTerm"
              defaultValues:		[1]
              minimumItems:			1
              rowComponent:			RowLayout
              {
                Row
                {
                  spacing:				          4 * preferencesModel.uiScale
                  Layout.preferredWidth:	110 * preferencesModel.uiScale

                  Label
                  {
                    text:				qsTr("Term ") + (rowIndex + 1)
                  }
                }
                
                RowLayout
                {
                  spacing:				          4 * preferencesModel.uiScale
                  Layout.preferredWidth:	 50 * preferencesModel.uiScale
                  
                  IntegerField
                  {
                    id:					  dimension
                    name:				  "kernelDimension"
                    useExternalBorder:	true
                    min:				    1
                    defaultValue:		1
                  }
                }
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
    }

    Group
    {
      title: qsTr("Transformation")
      CheckBox
      {
          name: "detrend"
          label: qsTr("Detrend")
      }
      CheckBox
      {
          name: "demean"
          label: qsTr("Demean")
      }
      CheckBox
      {
          name: "log"
          label: qsTr("Log")
          checked: true
      }
    }
    Group
      {
          title: qsTr("Noise shape line")
          CheckBox
          {
              name: "whiteNoise"
              label: qsTr("White noise")
          }
          CheckBox
          {
              name: "pinkNoise"
              label: qsTr("Pink noise")
          }
          CheckBox
          {
              name: "brownNoise"
              label: qsTr("Brown noise")
          }
      }
}
