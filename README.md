# Gw2Trading

Github project see [here](https://github.com/lz039/Gw2Trading).

## Goal
- In this project auction data from the online game Guild Wars 2 is analyzed.
- Data exploration is searching for the most profitable items in the auction house.
- There is also a regression model that predicts buy prices basically based on sell prices.

## Project structure

### Data

The folder `data` contains:

- `raw` data that was fetched from API
- `processed` data after data cleaning / filtering and also the model results used in the dashboard.

Because of the size of the data, it will not be part of the final ZIP file, but can be downloaded from Github.

### Notebooks

The folder `notebooks` contains different working sets:

- data-scraping for getting data
- data-exploration were the first contact points with the data or stuff that only bloated the project file.
- project is the step-by-step working notebook where the whole work was done
- report is the relevant, stripped report for data-science presentation

### Libs

The folder `libs` contains custom functions:

- prepare_data will transform raw API data to processed data that can be used for modeling
- train_model is not used but will basically take processed data and train the model again

### Reports

The folder `reports` contains:

- app.R, a shiny app used as a dashboard
- presentation.rmd will generate a power point presentation from R
- presentation_custom_design is the final presentation after some manual intervention

## Deployment

The shiny app (dashboard) can also be built as Docker image using the Dockerfile in the `reports` folder, e.g. with:
```
docker build -t gw2trading .
```

Then push run it locally with:
```
docker run -p 8080:8080 -d gw2trading
```
.. and open a browser at `http://localhost:8080`.
Careful, there is zero security! ;)

It's also possible to deploy the image to a destination of choice.
I deployed it: [here](http://20.23.97.43:8080/) (may take some time to load first time because of container warm-up)