# finanalysis

This project is a Shiny server project that displays and analyzes financial information by comparing it to a set of peers and then 
allowing the user to explore the data by using the googleVis Shiny package.  By using reactive HTML elements, the user is able to click
on the charts and get additional information either through additional charts specific to the financial category or subsidiary
clicked on by the user.

The project utilizes the RMySQL package to connect to a MySQL database that houses all of the financial and operating data.
