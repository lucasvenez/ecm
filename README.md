# Modelling Power Consumption based on Resource Utilization

This source code aims to model power consumption based on hardware utilization counters (e.g., memory usage, CPU occupation, number of I/O operations per interval) extracted from Unix-like operating systems.

We applied three different machine learning techniques to model energy consumption of two different hardwares:

- Linear Regression
- Regression Tree
- Artificial Neural Network (Multilayer Perceptron)

Our best model has an accuracy greater than 99.0% with mean squared error of ~6.00 watts. For correlation analysis, we applied a metric called [Maximal Information Coefficient (MIC)](http://exploredata.net) that has the equitability and generality properties.