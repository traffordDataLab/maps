## Reachability
[Shiny](https://cran.r-project.org/web/packages/shiny/index.html) app using the [OpenRouteService](https://openrouteservice.org/) API to generate equal distance and travel time polygons.

The geospatial data for General Practices derive from the the [Care Quality Commission](http://www.cqc.org.uk/about-us/transparency/using-cqc-data) and Pharmacies from Trafford Council.

<br>

<img src="https://github.com/traffordDataLab/shiny/blob/master/reachability/screenshot.png" width="800">

<br />

To run the app locally execute the following code: 

``` r
shiny::runGitHub("trafforddatalab/maps", subdir = "reachability")
```

