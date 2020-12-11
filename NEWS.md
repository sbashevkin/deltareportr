# deltareportr 2.0.0

* Altered plotting functions to output 1 plot for each season.
* Added SKT, 20mm, Suisun, USGS, and Baystudy data to water quality graphs
* Committed to full report showing all seasons, removed 1-season version.
* Moved region designation back to this package from [`discretewq`](https://github.com/sbashevkin/discretewq)
* Fixed error where December was being assigned to the prior year instead of the next year (to keep winters in the same year)
* Changed the year filtering so that December is counted in the next year before filtering by years (so now Dec 2001 will be included in the 2002 data used in the report)

# deltareportr 1.0.0

* Added zooplankton EZ stations
* Moved water quality integrated dataset to the [`discretewq`](https://github.com/sbashevkin/discretewq) package
* Fixed error in calculation of zoop BPUE. No change in overall pattern of zoop abundance across years.
* Updated datasets (https://github.com/sbashevkin/deltareportr/issues/3)
* Created 2019 version of report
* Fixed phytoplankton peak label.

# deltareportr 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Version used in climate change modeling
