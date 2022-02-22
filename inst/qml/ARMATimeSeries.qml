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
        AssignedVariablesList  
        {
            name: "covariates"
            label: qsTr("Covariates")
            allowedColumns: ["ordinal", "scale"]
        }
    }

    Group
    {
        title: qsTr("Model Specification")
        RadioButtonGroup
        {
            name: "model"
            RadioButton 
            {
                value: "best"
                id: best
                label: qsTr("Best fitting")	
                checked: true 
                RadioButtonGroup
                {
                    name: "ic"
                    title: qsTr("Information criterion")
                    radioButtonsOnSameRow: true
                    enabled: best.checked
                    RadioButton { value: "aicc";    label: qsTr("AICc");	checked: true   }
                    RadioButton { value: "aic";	    label: qsTr("AIC")				        }
                    RadioButton { value: "bic";	    label: qsTr("BIC")				        } 

                }
            }
            RadioButton 
            { 
                value: "manual"	
                id: manual
                label: qsTr("Manual")
                Group
                {
                    title: qsTr("Model terms")
                    enabled: manual.checked
                    CheckBox
                    {
                        name:   "ar"
                        id:     ar
                        label:  qsTr("Autoregressive (AR) order")
                        checked: true
                        childrenOnSameRow: true
                        IntegerField { name: "p" }
                    }
                    CheckBox
                    {
                        name:       "i"
                        id:         i
                        label:      qsTr("Difference (I) degree")
                        childrenOnSameRow: true
                        IntegerField { name: "d" }
                    }
                    CheckBox
                    {
                        name:       "ma"
                        id:         ma
                        label:      qsTr("Moving average (MA) order")
                        childrenOnSameRow: true
                        IntegerField { name: "q" }
                    }
                }				  
            }
        }
    }

    Group
    {
        title: qsTr("Data Transformation")
        CheckBox
        {
            name:   "center"
            id:     center
            label:  qsTr("Center")
        }
        CheckBox
        {
            name:   "detrend"
            id:     detrend
            label:  qsTr("Detrend")
        }
    }
    Group
    {
        title: qsTr("Residual Diagnostics")
        CheckBox
        {
            name:   "residualPlots"
            id:     residualPlots
            label:  qsTr("Diagnostic plots")
        }
        CheckBox
        {
            name:   "residualTable"
            id:     residualTable
            label:  qsTr("Diagnostic table")
        }
    }
}
// dataSetModel.rowCount()
