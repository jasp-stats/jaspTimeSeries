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
            height: 120
        }
    }
    
    Group
    {
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
        // CheckBox
        // {
        //     name:   "detrend"
        //     id:     detrend
        //     label:  qsTr("Detrend using linear regression")
        //     IntegerField { name: "poly"; label: qsTr("Polynomial"); defaultValue: 1; min: 0; max: 10; }
        // }
    }

    // Section
    // {
    //     title: qsTr("Data Preparation")
    //     Group
    //     {
    //         title: qsTr("Stationarity Tests")
    //         CheckBox
    //         {
    //             name:   "adfTest"
    //             id:     adfTest
    //             label:  qsTr("Augmented Dickey-Fuller")
    //         }
    //         CheckBox
    //         {
    //             name:   "ppTest"
    //             id:     ppTest
    //             label:  qsTr("Phillips-Perron")
    //             // RadioButtonGroup
    //             // {
    //             //     name: "ppType"
    //             //     title: qsTr("Type") 
    //             //     radioButtonsOnSameRow: true
    //             //     RadioButton { value: "normalized";	label: qsTr("Normalized biased") }
    //             //     RadioButton { value: "studentzed";	label: qsTr("Studentized") }
    //             // }
    //         }
    //         Group
    //         {
    //             title: qsTr("Kwiatkowski-Phillips-Schmidt-Shin")
    //             columns: 2
    //             CheckBox { name: "kpssLevel"; id: kpssLevel;	label: qsTr("Level stationary") }
    //             CheckBox { name: "kpssTrend"; id: kpssTrend;	label: qsTr("Trend stationary") }

    //         }
    //     }
    //     RadioButtonGroup
    //     {
    //         name: "transformation"
    //         title: qsTr("Transformation")
    //         info: "test"
    //         RadioButton  { value: "noTransform";	label: qsTr("None");    checked: true }
    //         RadioButton  { value: "center";	        label: qsTr("Center") }
    //         RadioButton
    //         {
    //             value: "detrend"
    //             label: qsTr("Detrend using linear regression")
    //             IntegerField { name: "poly"; label: qsTr("Polynomial"); defaultValue: 1; min: 0; max: 10; }
    //         }
    //     }
    //     CheckBox 
    //     {
    //       id:							transformationSavedToData
    //       name:						"transformationSavedToData"
    //       text:						qsTr("Add transformation to data")

    //       ComputedColumnField 
    //       {
    //         id:						    transformationColumn
    //         name:					    "transformationColumn"
    //         text:					    qsTr("Column name")
    //         placeholderText:  qsTr("e.g., transformed")
    //         fieldWidth:				120
    //         enabled:				  transformationSavedToData.checked
    //       }
    //     }
    // }


    Section
    {
        title: qsTr("Model")
        Group
        {
            CheckBox
            {
                name:       "addConstant"
                id:         addConstant
                label:      qsTr("Include constant")
                checked:    true
                enabled:    best.checked | (manual.checked & (d.value < 2 | D.value < 2))
            }
            CheckBox
            {
                name:   "addSeasonal"
                id:     addSeasonal
                label:  qsTr("Add seasonal components")
                RadioButtonGroup
                {
                    name: "period"
                    radioButtonsOnSameRow: true
                    RadioButton 
                    { 
                        value: "specifyPeriod"
                        label: qsTr("Specify period")
                        checked: true
                        childrenOnSameRow: true
                        IntegerField { name: "m";   id: m; defaultValue: 1 }
                    }
                    RadioButton { value: "findPeriod";  label: qsTr("Find dominant period") }
                }
            }
        
            RadioButtonGroup
            {
                name: "model"
                title: qsTr("Model Specification")
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
                        // enabled: best.checked
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
                        columns: 2
                        Group
                        {
                            title: qsTr("Nonseasonal")
                            IntegerField
                            {
                                name:   "p"
                                id:     p
                                label:  qsTr("Autoregressive (AR) order p")
                            }
                            IntegerField
                            {
                                name:   "d"
                                id:     d
                                label:  qsTr("Difference (I) degree d")
                            }
                            IntegerField
                            {
                                name:   "q"
                                id:     q
                                label:  qsTr("Moving average (MA) order q")
                            }
                        }
                        Group
                        {
                            title: qsTr("Seasonal")
                            enabled: addSeasonal.checked
                            IntegerField
                            {
                                name:   "P"
                                id:     sP
                                label:  qsTr("Autoregressive (AR) order P")
                            }
                            IntegerField
                            {
                                name:   "D"
                                id:     sD
                                label:  qsTr("Difference (I) degree D")
                            }
                            IntegerField
                            {
                                name:   "Q"
                                id:     sQ
                                label:  qsTr("Moving average (MA) order Q")
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
        CheckBox
        {
            name:   "residualPlots"
            id:     residualPlots
            label:  qsTr("Residual Plots")
            // CheckBox
            // {
            //     name:   "residualTimeSeries"
            //     id:     residualTimeSeries
            //     label:  qsTr("Time series plot")
            CheckBox
            {
                name:       "residualTs"
                id:         residualTs
                label:      qsTr("Time series")
                checked:    true
                RadioButtonGroup
                {
                    name:	"residualTsType"
                    radioButtonsOnSameRow: true
                    RadioButton { value: "points";	label: qsTr("Points") }
                    RadioButton { value: "line";	label: qsTr("Line") }
                    RadioButton { value: "both";	label: qsTr("Both");	checked: true }
                }
                RadioButtonGroup
                {
                    name:	"residualsDistribution"
                    title: qsTr("Distribution")
                    // radioButtonsOnSameRow: true
                    RadioButton { value: "density";	label: qsTr("Density") }
                    RadioButton { value: "histogram";	label: qsTr("Histogram") }
                    RadioButton { value: "none";	label: qsTr("None");	checked: true }
                }
            }
            CheckBox
            {
                name:   "residualQQ"
                id:     residualQQ
                label:  qsTr("Q-Q plot")
            }
            Group
            {
                title: qsTr("Autocorrelation")
                CheckBox
                {
                    name:   "residualAcf"
                    id:     residualACF
                    label:  qsTr("Autocorrelation function")
                    CheckBox
                    {
                        name: "residualAcfCi"
                        label: qsTr("Show confidence interval")
                        checked: true
                        childrenOnSameRow: true
                        CIField { name: "residualAcfCiLevel" }
                    }
                    CheckBox { name: "residualAcfFirstLag"; label: qsTr("Include first lag") }
                    // IntegerField { name: "residualAcfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
                }
                CheckBox
                {
                    name:   "residualLB"
                    id:     residualLB
                    label:  qsTr("Ljung-Box p-values")
                    CIField { name: "ljungBoxSignificanceLevel"; label: qsTr("Significance level"); defaultValue: 5 }
                    // DoubleField
                    // { 
                    //     name: "ljungBoxSignificanceLevel"
                    //     label: qsTr("Significance level")
                    //     defaultValue: 5
                    //     decimals: 1

                    // }
                }
                IntegerField 
                {
                    name: "residualMaxLag"
                    label: qsTr("Maximum lag")
                    min: 1
                    defaultValue: 10
                    enabled: residualACF.checked | residualLB.checked
                }
            }
        }
        CheckBox 
        {
          id:							residualSavedToData
          name:						"residualSavedToData"
          text:						qsTr("Add residuals to data")

          ComputedColumnField 
          {
            id:						    residualColumn
            name:					    "residualColumn"
            text:					    qsTr("Column name")
            placeholderText:  qsTr("e.g., residuals")
            fieldWidth:				120
            enabled:				  residualSavedToData.checked
          }
        }
        // Group
        // {
        //     title: qsTr("Tests")
        //     CheckBox
        //     {
        //         name:   "ljungBox"
        //         id:     ljungBox
        //         label:  qsTr("Ljung-Box test")
        //     }
        // }
    }

    Section
    {
        title: qsTr("Forecasting")
        Group
        {
            CheckBox
            {
                name:   "forecastTimeSeries"
                id:     forecastTimeSeries
                label:  qsTr("Time series plot")
                RadioButtonGroup
                {
                    name:	"forecastType"
                    radioButtonsOnSameRow: true
                    RadioButton { value: "points";	label: qsTr("Points") }
                    RadioButton { value: "line";	label: qsTr("Line") }
                    RadioButton { value: "both";	label: qsTr("Both");	checked: true }
                }
                IntegerField { name: "nForecasts"; label: qsTr("Number of forecasts"); min: 1; defaultValue: 10 }
                CheckBox
                {
                    name:       "addObserved"
                    id:         addObserved
                    label:      qsTr("Include observed data")
                    checked:    true
                }
            }
        }
        Group
        {
            CheckBox
            {
                name:   "forecastTable"
                id:     forecastTable
                label:  qsTr("Forecasts table")
            }
        
            FileSelector
            {
                name:	             "save"
                label:	            qsTr("Save forecasts as")
                placeholderText:    qsTr("e.g. forecasts.csv")
                filter:	            "*.csv"
                save:	              true
                fieldWidth:         180 * preferencesModel.uiScale
            }
        }
    }
}
// dataSetModel.rowCount()
