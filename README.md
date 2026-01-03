# kingCoData

This is a data package.  It includes two main datasets:

1) All single family and townhome sales (605,000+) in King County, Washington from 1999 through December 2025.
2) All single family and townhome properties (508,000+) in King County, Washington as of December 31, 2025. 

After installing this package with `devtools::install_github('andykrause/kingCoData')`, you can load the data with: `data(kingco_sales)` and/or `data(kingco_homes)`

Most fields are self-explanatory, those that are note can be further explained with the `explain('x')` function, where 'x' is the field name. 
