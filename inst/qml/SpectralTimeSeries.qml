import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
    info: qsTr("Spectral analysis allows the user to assess the frequency components and power distribution of a time series.")
    VariablesForm
    {
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList
        {
            name: "dependent"
            label: qsTr("Variable")
            allowedColumns: ["scale"]
            singleVariable: true
            info: qsTr("A variable that is measured repeatedly over time.")
        }
        AssignedVariablesList
        {
            name: "time"
            label: qsTr("Time")
            allowedColumns: ["scale", "ordinal", "nominal"]
            singleVariable: true
            info: qsTr("Optional. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use 'YYYY-MM-DD' for dates or 'YYYY-MM-DD HH:MM:SS' for dates with times; seconds can be omitted. All variables stay aligned, and spreadsheet row order is unchanged. Tied values retain spreadsheet row order. If any value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time.")
        }
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
            info: qsTr("Smooths the power spectral density using a kernel. The dimension determines the convolution of kernels. The taper specifies the proportion of data to which a split cosine bell taper is applied at the beginning and end of the series.")
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
		Item
		{
			Layout.leftMargin: 25 * preferencesModel.uiScale
			Layout.preferredHeight: kernelTerm.implicitHeight

			ComponentsList
			{
				id:					kernelTerm
				name:				"kernelTerm"
				enabled:			kernel.checked
				defaultValues:		[1]
				minimumItems:		1
				headerLabels:		[{"kernelDimension": qsTr("Dimension")}]
				rowComponent: RowLayout
				{

					Label
					{
						Layout.preferredWidth:	110 * preferencesModel.uiScale
						text:					qsTr("Term ") + (rowIndex + 1)
					}
					IntegerField
					{
						id:						dimension
						name:					"kernelDimension"
						useExternalBorder:		true
						min:				    1
						defaultValue:			1
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
        CheckBox
        {
            name: "filter"
            id: filter
            label: qsTr("Filter by")
            info: qsTr("Selects an inclusive range of observations for analysis. Row number refers to original spreadsheet rows, even when Time changes the analysis order. Time index filters numeric Time values, including negative values and decimals. Date filters interpreted dates and times. Date filters use UTC. A date-only End includes the entire day; an End with a time uses that exact cutoff. Time or date filtering requires a usable Time variable; otherwise, select Row number or disable filtering.")
            RadioButtonGroup
            {
                name: "filterBy"
                RadioButton
                {
                    value: "row"
                    id: filterRow
                    label: qsTr("Row number")
                    checked: true
                    Group
                    {
                        columns: 2
                        IntegerField { name: "rowStart"; label: qsTr("Start"); defaultValue: 1 }
                        IntegerField { name: "rowEnd"; label: qsTr("End"); defaultValue: 100 }
                    }
                }
                RadioButton
                {
                    value: "time"
                    id: filterTime
                    label: qsTr("Time index")
                    enabled: time.count != 0
                    Group
                    {
                        columns: 2
                        DoubleField { name: "timeStart"; label: qsTr("Start"); defaultValue: 1; negativeValues: true; decimals: 10 }
                        DoubleField { name: "timeEnd"; label: qsTr("End"); defaultValue: 100; negativeValues: true; decimals: 10 }
                    }
                }
                RadioButton
                {
                    value: "date"
                    id: filterDate
                    label: qsTr("Date")
                    enabled: time.count != 0
                    Group
                    {
                        TextField
                        {
                            name: "dateStart"
                            label: qsTr("Start")
                            placeholderText: "YYYY-MM-DD HH:MM:SS"
                            fieldWidth: 150
                        }
                        TextField
                        {
                            name: "dateEnd"
                            label: qsTr("End")
                            placeholderText: "YYYY-MM-DD HH:MM:SS"
                            fieldWidth: 150
                        }
                    }
                }
            }
        }
    }

    Group
    {
        title:  qsTr("Noise shape line")
        info:   qsTr("Add a line to the spectral density indicating the power spectral density of a white, pink or brown noise process.")
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

    Group
    {
        title:  qsTr("Transformation")
        info:   qsTr("Transforms the time series by removing the trend or mean.")
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
    }
}
