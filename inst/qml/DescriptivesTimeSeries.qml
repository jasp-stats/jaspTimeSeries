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
            allowedColumns: ["ordinal", "scale"]
            singleVariable: true
        }
    }

    Group
    {
      // title: qsTr("Table")
      CheckBox
      {
          name: "descriptivesTableTransposed"
          label: qsTr("Transpose descriptives table")
      }
    }

    Section
    {
        title: qsTr("Plots")
        columns: 1
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
                // radioButtonsOnSameRow: true
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
                // columns: 2
                IntegerField
                {
                    name: "lagPlotLag"
                    label: qsTr("Lag")
                    defaultValue: 1
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
                        RadioButton { value: "linear";	label: qsTr("Linear")				  }
                    }

                    CheckBox
                    {
                        name: "lagPlotRegressionCi"
                        label: qsTr("Show confidence interval")
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
            CheckBox
            {
                name: "acfCi"
                label: qsTr("Show confidence interval")
                checked: true
                childrenOnSameRow: true
                CIField { name: "acfCiLevel" }
                RadioButtonGroup
                {
                  name: "acfCiType"
                  title: qsTr("Confidence interval type")
                  RadioButton { value: "whiteNoise";	label: qsTr("White noise");	checked: true }
                  RadioButton { value: "movingAverage";	label: qsTr("Moving average")	}
                  // RadioButton { value: "bartlett";	label: qsTr("Bartlett")	}
                }
            }
            CheckBox { name: "acfFirstLag"; label: qsTr("Include first lag") }
            IntegerField { name: "acfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
        }
        CheckBox
        {
            name: "pacf"
            id: pacf
            label: qsTr("Partial autocorrelation function")
            CheckBox
            {
                name: "pacfCi"
                label: qsTr("Show confidence interval")
                checked: true
                childrenOnSameRow: true
                CIField { name: "pacfCiLevel" }
            }
            IntegerField { name: "pacfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
        }
        // CheckBox
        // {
        //     name: "powerSpectralDensity"
        //     id: psd
        //     label: qsTr("Power spectral density")
        //     Group
        //     {
        //         columns: 2
        //         CheckBox
        //         {
        //             name: "powerSpectralDensityDetrend"
        //             label: qsTr("Detrend")
        //             checked: true
        //         }
        //         CheckBox
        //         {
        //             name: "powerSpectralDensityDemean"
        //             label: qsTr("Demean")
        //             checked: false
        //         }
        //     }
        //     CheckBox
        //     {
        //         name: "powerSpectralDensitySmoother"
        //         label: qsTr("Add kernel smoother")
        //         childrenOnSameRow: true
        //         DropDown
        //         {
        //             name: "powerSpectralDensitySmootherKernel"
        //             values:
        //             [
        //                 { label: qsTr("Daniell"), value: "daniell" },
        //                 { label: qsTr("Modified Daniell"), value: "modified.daniell" }
        //             ]
        //         }
        //         Group
        //         {
        //             RowLayout
        //             {
        //               Label
        //               {
        //                 text:						qsTr("Dimension")
        //                 Layout.leftMargin:			130 * preferencesModel.uiScale
        //                 Layout.preferredWidth:	70 * preferencesModel.uiScale
        //               }
        //             }

        //             ComponentsList
        //             {
        //               name:							"term"
        //               defaultValues:		[1]
        //               minimumItems:			1
        //               rowComponent:			RowLayout
        //               {
        //                 Row
        //                 {
        //                   spacing:				4 * preferencesModel.uiScale
        //                   Layout.preferredWidth:	110 * preferencesModel.uiScale

        //                   Label
        //                   {
        //                     text:				qsTr("Term ") + (rowIndex + 1)
        //                   }
        //                 }
                        
        //                 RowLayout
        //                 {
        //                   spacing:				4 * preferencesModel.uiScale
        //                   Layout.preferredWidth:	50 * preferencesModel.uiScale
                          
        //                   IntegerField
        //                   {
        //                     id:					  dimension
        //                     name:				  "dimension"
        //                     useExternalBorder:	true
        //                     min:				    1
        //                     defaultValue:		1
        //                   }
        //                 }
        //               }
        //             } 
        //         }
        //     }
    //         DoubleField
    //         {
    //             name: "powerSpectralDensityTaper"
    //             label: qsTr("Taper")
    //             min: 0
    //             max: 0.5
    //         }
    //         RadioButtonGroup
    //         {
    //             name: "powerSpectralDensityScaling"
    //             title: qsTr("Scaling")
    //             radioButtonsOnSameRow: true
    //             RadioButton
    //             {
    //                 name: "noScaling"
    //                 label: qsTr("None")
    //                 checked: true
    //             }
    //             RadioButton
    //             {
    //                 name: "log"
    //                 label: qsTr("ln")
    //             }
    //             RadioButton
    //             {
    //                 name: "log10"
    //                 label: qsTr("log")
    //             }
    //         }
    //     }
    }
}
// dataSetModel.rowCount()
