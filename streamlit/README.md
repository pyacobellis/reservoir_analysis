# Reservoir Time Series Forecasting App

A Streamlit web application for analyzing reservoir data and forecasting the value time series using multiple algorithms.

## Features

- **Data Upload**: Upload Excel files containing reservoir data
- **Exploratory Data Analysis**:
  - Data overview and statistics
  - Time series visualizations
  - Distribution plots
  - Correlation analysis
  - Missing values analysis
- **Forecasting**: Three algorithms available:
  - **ARIMA**: AutoRegressive Integrated Moving Average
  - **Prophet**: Facebook's forecasting library
  - **LSTM**: Long Short-Term Memory neural network

## Installation

1. Install core dependencies:
```bash
pip install -r requirements.txt
```

2. (Optional) For LSTM forecasting, install TensorFlow:
```bash
pip install tensorflow
```

Or install all dependencies including TensorFlow:
```bash
pip install -r requirements_full.txt
```

Note: TensorFlow may not be available for all Python versions. If you encounter issues, the app will still work with ARIMA and Prophet forecasting methods.

## Usage

1. Run the Streamlit app:
```bash
streamlit run app.py
```

2. Open your browser (should open automatically) and navigate to the local URL (typically `http://localhost:8501`)

3. Upload your Excel file using the sidebar

4. Explore the three tabs:
   - Data Overview
   - EDA
   - Forecasting

## Data Format

Your Excel file should contain at minimum:
- `datetime` column: timestamp data
- `value` column: numeric values to forecast

Additional columns will be included in the EDA analysis.

## Example

Use the `scenario_data.xlsx` file from the parent directory to test the application.
