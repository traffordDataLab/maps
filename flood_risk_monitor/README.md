# Flood Risk Monitor
[Flood Risk Monitor](https://www.trafforddatalab.io/maps/flood_risk_monitor) is an interactive map application built by [Trafford Data Lab](https://www.trafforddatalab.io) combining datasets from the <a href="https://www.gov.uk/government/organisations/environment-agency" target="_blank">Environment Agency</a> and <a href="https://www.ordnancesurvey.co.uk/" target="_blank">Ordnance Survey</a> showing <a href="https://environment.data.gov.uk/dataset/96ab4342-82c1-4095-87f1-0082e8d84ef1" target="_blank">flood risk</a> from <a href="https://www.ordnancesurvey.co.uk/products/os-open-rivers" target="_blank">watercourses</a> in and around Trafford and the latest available monitoring stations' <a href="https://environment.data.gov.uk/dataset/ae80cf81-f3aa-4703-88e7-41fbe80c67b2" target="_blank">water level readings</a>.

The datasets used in the app are stored within our [open_data repo](https://github.com/traffordDataLab/open_data) and can be accessed via the following links:

- [**Flood risk**](https://github.com/traffordDataLab/open_data/tree/master/flood_risk)
- [**Water level monitoring**](https://github.com/traffordDataLab/open_data/tree/master/water_level_monitoring)
- [**Watercourses**](https://github.com/traffordDataLab/open_data/tree/master/watercourses)

Within each folder is the metadata includinging links to the data source and publisher, the R script for obtaining the data and the dataset files.

The map application itself uses the following technologies:

- [**Leaflet JS**](https://leafletjs.com/) JavaScript mapping library
- [**Lab_leaflet**](https://github.com/traffordDataLab/lab_leaflet) our Leaflet JS wrapper library
- [**Leaflet.reachability**](https://github.com/traffordDataLab/leaflet.reachability) our reachability Leaflet plugin which uses the [isochrones API](https://openrouteservice.org/dev/#/api-docs) from [OpenRouteService](https://openrouteservice.org/)
- [**Leaflet.Locate**](https://github.com/domoritz/leaflet-locatecontrol) Leaflet plugin
- [**OpenStreetMap**](https://www.openstreetmap.org/#map=5/54.910/-3.432) raster tiles
- [**CartoDB**](https://github.com/CartoDB/basemap-styles) raster tiles
