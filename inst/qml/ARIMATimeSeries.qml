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
        RadioButtonGroup
        {
            name: "transformation"
            title: qsTr("Data Transformation")
            RadioButton  { value: "noTransform";	label: qsTr("None") }
            RadioButton  { value: "center";	        label: qsTr("Center") }
            RadioButton
            {
                value: "detrend"
                label: qsTr("Detrend using linear regression")
                IntegerField { name: "poly"; label: qsTr("Polynomial"); defaultValue: 1; min: 0; max: 10; }
            }
        }
        // title: qsTr("Data Transformation")
        CheckBox
        {
            name: "timeSeriesPlot"
            id:    tsPlot
            label: qsTr("Plot time series")
            checked: true
            RadioButtonGroup
            {
                name:	"tsType"
                radioButtonsOnSameRow: true
                RadioButton { value: "points";	label: qsTr("Points") }
                RadioButton { value: "line";	label: qsTr("Line") }
                RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            }
        }
        // CheckBox
        // {
        //     name:   "center"
        //     id:     center
        //     label:  qsTr("Center")
        // }
        // CheckBox
        // {
        //     name:   "detrend"
        //     id:     detrend
        //     label:  qsTr("Detrend using linear regression")
        //     IntegerField { name: "poly"; label: qsTr("Polynomial"); defaultValue: 1; min: 0; max: 10; }
        // }
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
                    enabled: manual.checked
                    IntegerField
                    {
                        name:   "p"
                        id:     p
                        label:  qsTr("Autoregressive (AR) order p")
                    }
                    IntegerField
                    {
                        name:       "d"
                        id:         d
                        label:      qsTr("Difference (I) degree d")
                    }
                    IntegerField
                    {
                        name:       "q"
                        id:         q
                        label:      qsTr("Moving average (MA) order q")
                    }
                    CheckBox
                    {
                        name:   "addConstant"
                        id:     addConstant
                        label:  qsTr("Include constant")
                    }
                    CheckBox
                    {
                        name:   "addSeasonal"
                        id:     addSeasonal
                        label:  qsTr("Add seasonal components")
                        Group
                        {
                            IntegerField
                            {
                                name:   "m"
                                id:     m
                                label:  qsTr("Period")
                            }
                            IntegerField
                            {
                                name:   "P"
                                id:     sP
                                label:  qsTr("AR order P")
                            }
                            IntegerField
                            {
                                name:       "D"
                                id:         sD
                                label:      qsTr("I degree D")
                            }
                            IntegerField
                            {
                                name:       "Q"
                                id:         sQ
                                label:      qsTr("MA order Q")
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
            title: qsTr("Plots")
            CheckBox
            {
                name:   "residualTimeSeries"
                id:     residualTimeSeries
                label:  qsTr("Time series plot")
            }
            CheckBox
            {
                name:   "residualACF"
                id:     residualACF
                label:  qsTr("Autocorrelation function")
            }
            CheckBox
            {
                name:   "residualHistogram"
                id:     residualHistogram
                label:  qsTr("Histogram")
            }
            CheckBox
            {
                name:   "residualQQ"
                id:     residualQQ
                label:  qsTr("Q-Q plot")
            }
        }
        Group
        {
            title: qsTr("Tests")
            CheckBox
            {
                name:   "ljungBox"
                id:     ljungBox
                label:  qsTr("Ljung-Box test")
            }
        }
    }
}
// dataSetModel.rowCount()
