## Reflection

The goal for this last milestone was to finalize a dashboard that incorporated all the feedback from prior iterations. One of the main considerations was developing in R versus Python. The primary advantages of working in Python was that Dash was significantly better documented, meaning that troubleshooting was much more efficient. Furthermore, the deployment time on Heroku was much quicker on the Python version of the app. However, one of the main issues was the performance in terms of load speed. The R version performed significantly better than the Python version. This, coupled with the fact that the R version already had several bugs fixed and additional features implemented, made it the more feasible choice for the final deployment. 

### Changes & Improvements

One of the most significant changes in this iteration is the design changes that were implemented. This new version has stylizing that helps users distinguish between areas they can input information and areas where they can view and interact with the dashboard. There is also additional text and links so users can find more information about who built the app, the last updated time and where they can find the original data and source code. Finally, the aesthetics of the plots have been improved to be more visually appealing. 

This version of the app also included three additional plots to convey “New Confirmed Cases”, “New Death Cases” and “New Recovered Cases” faceted as one object. The purpose of this was to give users a more comprehensive level of  information - similar to what was provided in the original dataset. 

Other changes include: 

- New colour scale for the map to highlight sequential nature of the data
- Map data now displays multiple metrics (previously only showed confirmed cases)

### Unresolved Issues :( 

Although the app has significantly more features and improved user experience since the first version, there are still several known issues that remain.

1. When the “per 1M” functionality is toggled, values on the y-axis appear very small. This was a known issue which is why in Milestone 3 the functionality was converted from “per capita” to “per 1M”. Since we were not able to make the y-axis dynamic, we decided to stay with scientific notation for the absolute numbers. 

2. When users select more than five or so fields in a dropdown, two issues occur. Firstly, the dropdown field does not expand large enough to view all the selected options. Secondly, the expanded text pushes down the remaining selection options beyond the grey box that highlights the selection options. In Milestone 4, we explored limiting the user to five inputs but there didn’t appear to be an easy way to implement this with the default parameters. 

### Final Thoughts

Building a dashboard is a lot harder than it seems! One of the major takeaways was to focus on the edge cases while testing the app. A recurring theme in the feedback was bugs that only occurred when the app was being used in a certain way. An example of this was the dropdowns being unable to show all the fields selected when a user chose more than five or so categories. Ultimately, the tradeoff was understanding whether it made more sense to fix edge case bugs or to focus our efforts on building new features that would be more widely used.  

