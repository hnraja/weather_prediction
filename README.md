# weather_prediction
## Data Source
Kaggle > Hourly energy demand generation and weather

https://www.kaggle.com/datasets/nicholasjhana/energy-consumption-generation-prices-and-weather?resource=download

## Data breakdown
Weather features (of interest)
* Temperature
* Pressure
* Humidity

Cities
* Barcelona	
* Bilbao	
* Madrid	
* Seville	
* Valencia

## Project breakdown
Proposed models
* Regularized regression
* Holt Winters
* Box Jenkins

## File Tree
* data.R
  * handles data preprocessing
  * controls the city, size of train-test split
  * loaded into other R files
* data.pdf
  * document exlplaining data preprocessing found in data.R
  * generated using data.Rmd
* weather_features.csv
  * the data file
* temperature/box_jenkins.R
  * implementaion of SARIMA for temperature data (in progress)

