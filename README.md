# Heatmap Dashboard

##### Description

The `Heatmap Dashboard` is a Tercen Shiny Operator for an interactive heatmap representation of a cross tab view.
Including:
- Palette settings
- Clustering and ordering of rows and columns
- Heatmap labels and annotation by row and columns factors

##### Usage

Input projection|.
---|---
`row`  | Factors(s) defining the rows of the heatmap         
`column` | Factors(s) defining the columns of the heatmap      
`value`  | values to represent in the heatmap    

Output relations|.
---|---
`Operator view` | view of the Shiny application
`corder`        | col order
`rorder`        | row order

##### Details


##### See Also
 The `Heatmap Dashboard`operator implements functionality of the R-package [ComplexHeatmap](https://www.bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html)
