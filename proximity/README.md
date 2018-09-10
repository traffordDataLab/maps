## Proximity 

This [Shiny](https://cran.r-project.org/web/packages/shiny/index.html) app using the [OpenRouteService](https://openrouteservice.org/) API to measure the proximity (distance and travel time) of fast food outlets to schools.

The geospatial data for schools and colleges derive from the the [Department for Education](https://get-information-schools.service.gov.uk/) and fast food outlets from the [Food Standards Agency](http://ratings.food.gov.uk/open-data/en-GB).

<br>

<img src="https://github.com/traffordDataLab/shiny/blob/master/proximity/screenshot.png" width="800">

<br />

To run the app locally execute the following code: 

``` r
shiny::runGitHub("trafforddatalab/maps", subdir = "proximity")
```

