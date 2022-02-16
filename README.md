<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.

# 259-integrating-skills-hw
Homework to put together all of the skills we've covered so far in class: importing and transforming data, manipulating strings, factors, and dates, writing functions, using across, map, and for loops to automate operations, and making graphs.

The data are drawn from the fivethirtyeight article [What 12 Months Of Record-Setting Temperatures Looks Like Across The U.S.](http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/). The data are drawn from 10 different weather stations, each in a different US city.

## Stations and city names

STATION | CITY
---|----
KCLT | Charlotte
KCQT | Los Angeles
KHOU | Houston
KIND | Indianapolis
KJAX | Jacksonville
KMDW | Chicago
KNYC | New York City
KPHL | Philadelphia
KPHX | Phoenix
KSEA | Seattle

## Data

Column | Description
---|---------
`date` | The date of the weather record, formatted YYYY-M-D
`actual_mean_temp` | The measured average temperature for that day
`actual_min_temp` | The measured minimum temperature for that day
`actual_max_temp` | The measured maximum temperature for that day
`average_min_temp` | The average minimum temperature on that day since 1880
`average_max_temp` | The average maximum temperature on that day since 1880
`record_min_temp` | The lowest ever temperature on that day since 1880
`record_max_temp` | The highest ever temperature on that day since 1880
`record_min_temp_year` | The year that the lowest ever temperature occurred
`record_max_temp_year` | The year that the highest ever temperature occurred
`actual_precipitation` | The measured amount of rain or snow for that day
`average_precipitation` | The average amount of rain or snow on that day since 1880
`record_precipitation` | The highest amount of rain or snow on that day since 1880

Source: [Weather Underground](http://wunderground.com)
