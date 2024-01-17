# kingCoData

This is a data package.  It includes two main datasets:

1) All single family and townhome sales (560,000+) in King County, Washington from 1999 through December 2023.
2) All single family and townhome properties (505,000+) in King County, Washington as of December 31, 2023. 

After installing this package with `devtools::install_github('andykrause/kingCoData')`, you can load the data with: `data(kingco_sales)` and/por `data(kingco_homes)`

Most fields are self-explanatory, those that are note can be further explained with the `explain('x')` function, where 'x' is the field name. 

