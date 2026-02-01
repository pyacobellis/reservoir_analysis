import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import warnings
warnings.filterwarnings('ignore')

# Check for optional dependencies
try:
    import tensorflow as tf
    TENSORFLOW_AVAILABLE = True
except ImportError:
    TENSORFLOW_AVAILABLE = False

# Page configuration
st.set_page_config(
    page_title="Reservoir Time Series Forecasting",
    page_icon="💧",
    layout="wide"
)

st.title("💧 Reservoir Time Series Analysis & Forecasting")
st.markdown("Upload your reservoir data to perform exploratory analysis and forecast the 'value' time series")

# Sidebar for file upload
st.sidebar.header("📁 Data Upload")
uploaded_file = st.sidebar.file_uploader("Upload Excel file", type=['xlsx', 'xls'])

if uploaded_file is not None:
    # Load data
    try:
        df = pd.read_excel(uploaded_file)
        st.sidebar.success(f"✅ File loaded: {uploaded_file.name}")
        st.sidebar.info(f"Shape: {df.shape[0]} rows × {df.shape[1]} columns")

        # Main content tabs
        tab1, tab2, tab3 = st.tabs(["📊 Data Overview", "🔍 EDA", "🔮 Forecasting"])

        # ===== TAB 1: DATA OVERVIEW =====
        with tab1:
            st.header("Data Overview")

            col1, col2, col3, col4 = st.columns(4)
            with col1:
                st.metric("Total Records", df.shape[0])
            with col2:
                st.metric("Variables", df.shape[1])
            with col3:
                st.metric("Missing Values", df.isnull().sum().sum())
            with col4:
                if 'datetime' in df.columns:
                    st.metric("Date Range", f"{(pd.to_datetime(df['datetime']).max() - pd.to_datetime(df['datetime']).min()).days} days")

            st.subheader("Data Preview")
            st.dataframe(df.head(20), use_container_width=True)

            st.subheader("Column Information")
            col_info = pd.DataFrame({
                'Column': df.columns,
                'Type': df.dtypes.values,
                'Non-Null Count': df.count().values,
                'Missing': df.isnull().sum().values,
                'Missing %': (df.isnull().sum().values / len(df) * 100).round(2)
            })
            st.dataframe(col_info, use_container_width=True)

            st.subheader("Descriptive Statistics")
            st.dataframe(df.describe(), use_container_width=True)

        # ===== TAB 2: EDA =====
        with tab2:
            st.header("Exploratory Data Analysis")

            # Check if datetime column exists
            if 'datetime' in df.columns:
                df['datetime'] = pd.to_datetime(df['datetime'])
                df_sorted = df.sort_values('datetime')

                # Time series plot for 'value'
                if 'value' in df.columns:
                    st.subheader("Value Time Series")
                    fig_ts = px.line(df_sorted, x='datetime', y='value',
                                     title='Value over Time',
                                     labels={'datetime': 'Date', 'value': 'Value'})
                    fig_ts.update_traces(line_color='#1f77b4')
                    st.plotly_chart(fig_ts, use_container_width=True)

                    # Distribution of values
                    col1, col2 = st.columns(2)
                    with col1:
                        st.subheader("Value Distribution")
                        fig_hist = px.histogram(df, x='value', nbins=50,
                                               title='Distribution of Values')
                        st.plotly_chart(fig_hist, use_container_width=True)

                    with col2:
                        st.subheader("Value Box Plot")
                        fig_box = px.box(df, y='value', title='Value Box Plot')
                        st.plotly_chart(fig_box, use_container_width=True)

                # Variable selection for additional plots
                st.subheader("Variable Time Series")
                numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
                if numeric_cols:
                    selected_var = st.selectbox("Select variable to visualize", numeric_cols)
                    fig_var = px.line(df_sorted, x='datetime', y=selected_var,
                                     title=f'{selected_var} over Time')
                    st.plotly_chart(fig_var, use_container_width=True)

                # Correlation heatmap
                if len(numeric_cols) > 1:
                    st.subheader("Correlation Matrix")
                    corr_matrix = df[numeric_cols].corr()
                    fig_corr = px.imshow(corr_matrix,
                                        text_auto='.2f',
                                        aspect='auto',
                                        title='Correlation Heatmap',
                                        color_continuous_scale='RdBu_r')
                    st.plotly_chart(fig_corr, use_container_width=True)

                # Missing values visualization
                st.subheader("Missing Values Analysis")
                missing_data = df.isnull().sum()
                missing_data = missing_data[missing_data > 0].sort_values(ascending=False)
                if len(missing_data) > 0:
                    fig_missing = px.bar(x=missing_data.index, y=missing_data.values,
                                        labels={'x': 'Column', 'y': 'Missing Count'},
                                        title='Missing Values by Column')
                    st.plotly_chart(fig_missing, use_container_width=True)
                else:
                    st.success("✅ No missing values detected!")

            else:
                st.warning("⚠️ No 'datetime' column found in the dataset")

        # ===== TAB 3: FORECASTING =====
        with tab3:
            st.header("Time Series Forecasting")

            if 'datetime' in df.columns and 'value' in df.columns:
                # Prepare data
                df['datetime'] = pd.to_datetime(df['datetime'])
                df_forecast = df[['datetime', 'value']].dropna().sort_values('datetime')
                df_forecast = df_forecast.set_index('datetime')

                # Forecast parameters
                col1, col2 = st.columns(2)
                with col1:
                    forecast_periods = st.slider("Forecast Periods", 10, 100, 30)
                with col2:
                    algorithms = ["ARIMA", "Prophet"]
                    if TENSORFLOW_AVAILABLE:
                        algorithms.append("LSTM")
                    algorithm = st.selectbox("Select Algorithm", algorithms)
                    if not TENSORFLOW_AVAILABLE and algorithm == "LSTM":
                        st.warning("⚠️ TensorFlow not installed. Install with: pip install tensorflow")

                if st.button("🔮 Generate Forecast", type="primary"):
                    with st.spinner(f"Training {algorithm} model..."):
                        try:
                            if algorithm == "ARIMA":
                                from statsmodels.tsa.arima.model import ARIMA

                                # Fit ARIMA model
                                model = ARIMA(df_forecast['value'], order=(5, 1, 0))
                                model_fit = model.fit()

                                # Forecast
                                forecast = model_fit.forecast(steps=forecast_periods)
                                forecast_index = pd.date_range(
                                    start=df_forecast.index[-1],
                                    periods=forecast_periods + 1,
                                    freq='D'
                                )[1:]

                                # Plot
                                fig = go.Figure()
                                fig.add_trace(go.Scatter(x=df_forecast.index, y=df_forecast['value'],
                                                        mode='lines', name='Historical Data',
                                                        line=dict(color='#1f77b4')))
                                fig.add_trace(go.Scatter(x=forecast_index, y=forecast,
                                                        mode='lines', name='Forecast',
                                                        line=dict(color='#ff7f0e', dash='dash')))
                                fig.update_layout(title='ARIMA Forecast',
                                                xaxis_title='Date',
                                                yaxis_title='Value',
                                                hovermode='x unified')
                                st.plotly_chart(fig, use_container_width=True)

                                # Model summary
                                st.subheader("Model Summary")
                                st.text(model_fit.summary())

                            elif algorithm == "Prophet":
                                from prophet import Prophet

                                # Prepare data for Prophet
                                df_prophet = df_forecast.reset_index()
                                df_prophet.columns = ['ds', 'y']

                                # Fit model
                                model = Prophet(daily_seasonality=True)
                                model.fit(df_prophet)

                                # Create future dataframe
                                future = model.make_future_dataframe(periods=forecast_periods)
                                forecast = model.predict(future)

                                # Plot
                                fig = go.Figure()
                                fig.add_trace(go.Scatter(x=df_prophet['ds'], y=df_prophet['y'],
                                                        mode='lines', name='Historical Data',
                                                        line=dict(color='#1f77b4')))
                                fig.add_trace(go.Scatter(x=forecast['ds'], y=forecast['yhat'],
                                                        mode='lines', name='Forecast',
                                                        line=dict(color='#ff7f0e')))
                                fig.add_trace(go.Scatter(x=forecast['ds'], y=forecast['yhat_upper'],
                                                        mode='lines', name='Upper Bound',
                                                        line=dict(color='rgba(255,127,14,0.3)'),
                                                        showlegend=False))
                                fig.add_trace(go.Scatter(x=forecast['ds'], y=forecast['yhat_lower'],
                                                        mode='lines', name='Lower Bound',
                                                        fill='tonexty',
                                                        line=dict(color='rgba(255,127,14,0.3)'),
                                                        showlegend=False))
                                fig.update_layout(title='Prophet Forecast',
                                                xaxis_title='Date',
                                                yaxis_title='Value',
                                                hovermode='x unified')
                                st.plotly_chart(fig, use_container_width=True)

                                # Components plot
                                st.subheader("Forecast Components")
                                fig_components = model.plot_components(forecast)
                                st.pyplot(fig_components)

                            elif algorithm == "LSTM":
                                if not TENSORFLOW_AVAILABLE:
                                    st.error("❌ TensorFlow is not installed. Please install it with: pip install tensorflow")
                                    st.stop()

                                from tensorflow.keras.models import Sequential
                                from tensorflow.keras.layers import LSTM, Dense, Dropout
                                from sklearn.preprocessing import MinMaxScaler

                                # Prepare data
                                scaler = MinMaxScaler()
                                scaled_data = scaler.fit_transform(df_forecast[['value']])

                                # Create sequences
                                def create_sequences(data, seq_length=10):
                                    X, y = [], []
                                    for i in range(len(data) - seq_length):
                                        X.append(data[i:i+seq_length])
                                        y.append(data[i+seq_length])
                                    return np.array(X), np.array(y)

                                seq_length = min(10, len(scaled_data) // 2)
                                X, y = create_sequences(scaled_data, seq_length)

                                # Build model
                                model = Sequential([
                                    LSTM(50, activation='relu', return_sequences=True, input_shape=(seq_length, 1)),
                                    Dropout(0.2),
                                    LSTM(50, activation='relu'),
                                    Dropout(0.2),
                                    Dense(1)
                                ])
                                model.compile(optimizer='adam', loss='mse')

                                # Train
                                model.fit(X, y, epochs=50, batch_size=32, verbose=0)

                                # Forecast
                                last_sequence = scaled_data[-seq_length:]
                                predictions = []

                                for _ in range(forecast_periods):
                                    pred = model.predict(last_sequence.reshape(1, seq_length, 1), verbose=0)
                                    predictions.append(pred[0, 0])
                                    last_sequence = np.append(last_sequence[1:], pred)

                                predictions = scaler.inverse_transform(np.array(predictions).reshape(-1, 1))
                                forecast_index = pd.date_range(
                                    start=df_forecast.index[-1],
                                    periods=forecast_periods + 1,
                                    freq='D'
                                )[1:]

                                # Plot
                                fig = go.Figure()
                                fig.add_trace(go.Scatter(x=df_forecast.index, y=df_forecast['value'],
                                                        mode='lines', name='Historical Data',
                                                        line=dict(color='#1f77b4')))
                                fig.add_trace(go.Scatter(x=forecast_index, y=predictions.flatten(),
                                                        mode='lines', name='Forecast',
                                                        line=dict(color='#ff7f0e', dash='dash')))
                                fig.update_layout(title='LSTM Forecast',
                                                xaxis_title='Date',
                                                yaxis_title='Value',
                                                hovermode='x unified')
                                st.plotly_chart(fig, use_container_width=True)

                                st.info("📊 LSTM model trained with 50 epochs")

                            st.success(f"✅ {algorithm} forecast completed!")

                        except Exception as e:
                            st.error(f"❌ Error: {str(e)}")
                            st.info("Try adjusting the forecast parameters or check your data format")
            else:
                st.warning("⚠️ Dataset must contain 'datetime' and 'value' columns for forecasting")

    except Exception as e:
        st.error(f"❌ Error loading file: {str(e)}")
        st.info("Please ensure the file is a valid Excel file with the correct format")
else:
    st.info("👈 Please upload an Excel file to begin")
    st.markdown("""
    ### Expected Data Format
    Your Excel file should contain at least:
    - A `datetime` column with timestamp data
    - A `value` column with numeric values to forecast

    ### Available Features
    1. **Data Overview**: View your data structure, statistics, and missing values
    2. **EDA**: Explore time series plots, distributions, correlations, and patterns
    3. **Forecasting**: Choose from three algorithms:
       - **ARIMA**: Classic statistical time series model
       - **Prophet**: Facebook's robust forecasting tool with trend and seasonality
       - **LSTM**: Deep learning neural network for complex patterns
    """)
