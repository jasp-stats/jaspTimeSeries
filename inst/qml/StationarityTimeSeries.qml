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
            allowAnalysisOwnComputedColumns: false
        }
        AssignedVariablesList  
        {
            name: "time"
            label: qsTr("Time")
            allowedColumns: ["ordinal", "nominalText"]
            singleVariable: true
            allowAnalysisOwnComputedColumns: false
        }
    }

    Group
    {
        title: qsTr("Stationarity Tests")
        CheckBox
        {
            name:   "adfTest"
            id:     adfTest
            label:  qsTr("Augmented Dickey-Fuller")
        }
        Group
        {
            title: qsTr("Phillips-Perron")
            columns: 2
            CheckBox { name: "ppTestRegressionCoefficient"; id: ppRegression;	label: qsTr("Regression coefficient test") }
            CheckBox { name: "ppTestStudentized"; id: ppStudentized;	label: qsTr("Studentized test") }

        }
        Group
        {
            title: qsTr("Kwiatkowski-Phillips-Schmidt-Shin")
            columns: 2
            CheckBox { name: "kpssLevel"; id: kpssLevel;	label: qsTr("Level stationary") }
            CheckBox { name: "kpssTrend"; id: kpssTrend;	label: qsTr("Trend stationary") }
        }
    }

    Section
    {
      title: qsTr("Transformation")
      Group
      {
        CheckBox
        { 
          name: "log"
          id: log
          label: qsTr("Log")
          childrenOnSameRow: true
          RadioButtonGroup
          {
              name:	"logBase"
              radioButtonsOnSameRow: true
              RadioButton { value: "10";	label: qsTr("Base 10"); checked: true }
              RadioButton { value: "e";	label: qsTr("Base e") }
          }
        }
        CheckBox
        { 
          name: "root"
          id: root
          label: qsTr("Root")
          childrenOnSameRow: true
          RadioButtonGroup
          {
              name:	"rootIndex"
              radioButtonsOnSameRow: true
              RadioButton { value: "square";	label: qsTr("Square"); checked: true }
              RadioButton { value: "cube";	label: qsTr("Cube") }
          }
        }
        CheckBox
        { 
          name: "boxCox"
          id: boxCox
          label: qsTr("Box-Cox")
          RadioButtonGroup
          {
              name:	"boxCoxLambdaSpecification"
              title: qsTr("Lambda")
              enabled: boxCox.checked
              radioButtonsOnSameRow: true
              RadioButton
              {
                value: "auto"
                id: auto
                label: qsTr("Auto")
                checked: true
              }
              RadioButton
              {
                value: "manual"
                id: manual
                label: qsTr("Manual")
                childrenOnSameRow: true
                DoubleField { name: "boxCoxLambda"; defaultValue: 0; min: -5; max: 5; }
              }
          }
        }
        CheckBox
        { 
          name: "detrend"
          id: detrend
          label: qsTr("Detrend using linear regression")
          RadioButtonGroup
          {
            name: "polynomialSpecification"
            title: qsTr("Polynomial")
            RadioButton
            {
              value: "auto"
              label: qsTr("Best fitting")
              checked: true
              IntegerField
              {
                name: "polynomialSpecificationAutoMax"
                label: qsTr("Maximum")
                defaultValue: 5
                min: 1
                max: 10
              }
              RadioButtonGroup
              {
                name: "polynomialSpecificationAutoIc"
                title: qsTr("Information criterion")
                radioButtonsOnSameRow: true
                RadioButton { value: "aic"; label: qsTr("AIC"); checked: true }
                RadioButton { value: "bic";	label: qsTr("BIC")				        } 
              }
            }
            RadioButton
            {
              value: "custom"
              label: qsTr("Custom")
              childrenOnSameRow: true
              IntegerField { name: "detrendPoly"; defaultValue: 1; min: 1; max: 10; }
            }
          }
        }
        CheckBox
        { 
          name: "difference"
          id: difference
          label: qsTr("Difference")
          childrenOnSameRow: true
          IntegerField { name: "differenceLag"; label: qsTr("Lag"); defaultValue: 1; min: 1; }
          IntegerField { name: "differenceOrder"; label: qsTr("Order"); defaultValue: 1; min: 1; }
        }

      }

      CheckBox 
      {
        id:							transformationSavedToData
        name:						"transformationSavedToData"
        text:						qsTr("Append transformation to spreadsheet")

        ComputedColumnField 
        {
          id:						    transformationColumn
          name:					    "transformationColumn"
          text:					    qsTr("Column name")
          placeholderText:  qsTr("e.g., transformed")
          fieldWidth:				120
          enabled:				  transformationSavedToData.checked
        }
      }
    }

    Section
    {
        title: qsTr("Plots")
        Group
        {
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
                  RadioButton { value: "density";	label: qsTr("Density") }
                  RadioButton { value: "histogram";	label: qsTr("Histogram") }
                  RadioButton { value: "none";	label: qsTr("None");	checked: true }
              }
          }

          CheckBox
          {
              name: "acf"
              id: acf
              label: qsTr("Autocorrelation function")
              IntegerField { name: "acfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
              CheckBox { name: "acfZeroLag"; label: qsTr("Zero lag") }
              CheckBox
              {
                  name: "acfCi"
                  id: acfCi
                  label: qsTr("Confidence interval")
                  checked: true
                  childrenOnSameRow: true
                  CIField { name: "acfCiLevel" }   
              }
              RadioButtonGroup
              {
                name: "acfCiType"
                enabled: acfCi.checked
                Layout.leftMargin: 25 * preferencesModel.uiScale
                RadioButton { value: "whiteNoise";	label: qsTr("Based on white noise");	checked: true }
                RadioButton { value: "movingAverage";	label: qsTr("Based on moving average")	}
              }
          }
          CheckBox
          {
              name: "pacf"
              id: pacf
              label: qsTr("Partial autocorrelation function")
              IntegerField { name: "pacfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
              CheckBox
              {
                  name: "pacfCi"
                  label: qsTr("Confidence interval")
                  checked: true
                  childrenOnSameRow: true
                  CIField { name: "pacfCiLevel" }
              }
          }
        }
    }
}
// dataSetModel.rowCount()
