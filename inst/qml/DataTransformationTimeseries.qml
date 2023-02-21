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
        title: qsTr("Stationarity Tests")
        CheckBox
        {
            name:   "adfTest"
            id:     adfTest
            label:  qsTr("Augmented Dickey-Fuller")
        }
        CheckBox
        {
            name:   "ppTest"
            id:     ppTest
            label:  qsTr("Phillips-Perron")
            // RadioButtonGroup
            // {
            //     name: "ppType"
            //     title: qsTr("Type") 
            //     radioButtonsOnSameRow: true
            //     RadioButton { value: "normalized";	label: qsTr("Normalized biased") }
            //     RadioButton { value: "studentzed";	label: qsTr("Studentized") }
            // }
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

    RadioButtonGroup
    {
        name: "transformation"
        title: qsTr("Transformation")
        info: "test"
        RadioButton  { value: "noTransform";	label: qsTr("None");    checked: true }
        RadioButton  { value: "center";	        label: qsTr("Center") }
        RadioButton
        {
            value: "detrend"
            label: qsTr("Detrend using linear regression")
            IntegerField { name: "poly"; label: qsTr("Polynomial"); defaultValue: 1; min: 0; max: 10; }
        }
    }
    CheckBox 
    {
      id:							transformationSavedToData
      name:						"transformationSavedToData"
      text:						qsTr("Add transformation to data")

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
                  // radioButtonsOnSameRow: true
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
                    RadioButton { value: "normal";	label: qsTr("Normal");	checked: true }
                    RadioButton { value: "bartlett";	label: qsTr("Bartlett")	}
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
        }
    }
}
// dataSetModel.rowCount()
