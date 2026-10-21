![Wadden sea](images/intertidalflat.jpeg)

Tidal flats are vital coastal areas regularly shaped by the tides. These ecosystems are important for coastal protection, biodiversity, and local economies, but they are sensitive to extreme weather events, such as heatwaves, which can strongly affect their temperature and ecological function. Recently, the Wadden Sea has experienced mass mortality of bottom-dwelling organisms due to extreme warming. However, we still lack precise data on how sediment temperature changes under different weather conditions, limiting our understanding of how climate change will impact this ecosystem. During this summer school project, you deployed high-frequency temperature loggers along an intertidal gradient to track temperature fluctuations across multiple tidal cycles. You have also taken sediment corers and characterize grain sizes and bulk density properties in the lab. Now it's time to map the spatio-temporal dynamics of intertidal seafloor's thermal variability and develop a simple mechanistic model of the sediment heat budget over the Wadden Sea. Depending on time, you will also investigate how these temperature shifts impact the composition and abundance of local marine fauna.

## How to use this VL

### Locally 
To use this repository on your local PC, simply clone this repository:

```
git clone https://github.com/LTER-LIFE/vl-tempsed-wadden-sea.git
```

Afterward, you will need to install the R packages necessary to use contents in this repo:

```
remotes::install_github("TempSED/TempSED", depend=TRUE)
remotes::install_github("LTER-LIFE/dtR/dtRtools", depend=TRUE)
remotes::install_github("LTER-LIFE/dtR/dtRwad", depend=TRUE)
```

### Cloud-based virtual lab 
To use this virtual lab, login to [NaaVRE](https://beta.naavre.net/vreapp) and scroll down to the different lab available. Find TempSED Wadden Sea Virtual Lab. Click and open the lab via log-in or registrating if for the first time. 

Upon launching this VL, you will be provided with contents present in this repository with all the software components necessary to run the notebook pre-installed. 

Open one of the notebook, and if no computational kernel is pre-selected, select `R-tempsed-wadden-sea`

## Assets
This repo/virtual lab contains the following assets:

- R packages developed in the framework of LTER-LIFE project
- Observed sediment temperature for testing provided by Oscar Franken & Tjitske Kooistra
- Notebooks for demonstration 
