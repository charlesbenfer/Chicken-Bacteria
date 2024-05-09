import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense
from sklearn.metrics import mean_squared_error

# Generate some sample data
np.random.seed(0)
num_samples = 1000
time = np.arange(0, num_samples)
response_variable = np.sin(0.02 * time) + 0.5 * np.random.randn(num_samples)

# Create a DataFrame
df = pd.DataFrame({'response': response_variable})

# Plot the data
plt.figure(figsize=(10, 6))
plt.plot(df['response'], label='Original Data')
plt.title('Original Data')
plt.xlabel('Time')
plt.ylabel('Response Variable')
plt.legend()
plt.show()

# Normalize the data
scaler = MinMaxScaler(feature_range=(0, 1))
df_scaled = scaler.fit_transform(df)

# Function to create sequences for LSTM
def create_sequences(data, seq_length):
    X, y = [], []
    for i in range(len(data) - seq_length):
        X.append(data[i:(i + seq_length), 0])
        y.append(data[i + seq_length, 0])
    return np.array(X), np.array(y)

# Define sequence length
sequence_length = 50

# Create sequences
X, y = create_sequences(df_scaled, sequence_length)

# Split data into training and testing sets
train_size = int(len(X) * 0.8)
test_size = len(X) - train_size
X_train, X_test = X[:train_size], X[train_size:]
y_train, y_test = y[:train_size], y[train_size:]

# Reshape data for LSTM input [samples, time steps, features]
X_train = np.reshape(X_train, (X_train.shape[0], X_train.shape[1], 1))
X_test = np.reshape(X_test, (X_test.shape[0], X_test.shape[1], 1))

# Build LSTM model
model = Sequential()
model.add(LSTM(units=50, return_sequences=True, input_shape=(X_train.shape[1], 1)))
model.add(LSTM(units=50, return_sequences=False))
model.add(Dense(units=1))
model.compile(optimizer='adam', loss='mean_squared_error')

# Train the model
model.fit(X_train, y_train, epochs=10, batch_size=32)

# Make predictions
train_predict = model.predict(X_train)
test_predict = model.predict(X_test)

# Invert predictions to original scale
train_predict = scaler.inverse_transform(train_predict)
y_train = scaler.inverse_transform([y_train])
test_predict = scaler.inverse_transform(test_predict)
y_test = scaler.inverse_transform([y_test])

# Calculate RMSE
train_score = np.sqrt(mean_squared_error(y_train[0], train_predict[:,0]))
print(f'Train RMSE: {train_score:.2f}')
test_score = np.sqrt(mean_squared_error(y_test[0], test_predict[:,0]))
print(f'Test RMSE: {test_score:.2f}')

# Plot training predictions
train_plot = np.empty_like(df_scaled)
train_plot[:, :] = np.nan
train_plot[sequence_length:len(train_predict) + sequence_length, :] = train_predict

# Plot test predictions
test_plot = np.empty_like(df_scaled)
test_plot[:, :] = np.nan
test_plot[len(train_predict) + (sequence_length * 2) + 1:len(df_scaled) - 1, :] = test_predict

# Plot all predictions
plt.figure(figsize=(10, 6))
plt.plot(scaler.inverse_transform(df_scaled), label='Original Data')
plt.plot(train_plot, label='Training Predictions')
plt.plot(test_plot, label='Testing Predictions')
plt.title('LSTM Forecast')
plt.xlabel('Time')
plt.ylabel('Response Variable')
plt.legend()
plt.show()
