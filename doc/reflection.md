## Reflection

The goal for this last milestone was to finalize a dashboard that incorporated all the feedback from prior iterations. One of the main considerations was developing in R versus Python. The primary advantages of working in Python was that Dash was significantly better documented, meaning that troubleshooting was much more efficient. Furthermore, the deployment time on Heroku was much shorter on the Python version of the app. However, one of the main issues was the performance in terms of load speed. The R version performed significantly better than the Python version. This, coupled with the fact that the R version already had several bugs fixed and additional features implemented, made it the more feasible choice for the final deployment. 

### Changes & Improvements

One of the most significant changes in this iteration is the design changes that were implemented. This new version has stylizing that helps users distinguish between areas they can input information and areas where they can view and interact with the dashboard. There is also additional text and links so users can find more information about who built the app, the last updated time and where they can find the original data and source code. Finally, the aesthetics of the plots have been improved to be more visually appealing. 

This version of the app also displays the plots as one vertically stacked object instead of three separate horizontal plots. The main purpose of this was to resolve the issue of the legend cutting into plot space in the previous versions as more countries or regions were added. Secondarily, faceting as one object meant that titles could be displayed more uniformly, instead of as annotated text. Lastly, this setup meant that the three plots could share one x axis, making the dashboard a little less cluttered. 

Other changes include: 

- New colour scale for the map to highlight sequential nature of the data
- Map data now displays multiple metrics (previously only showed confirmed cases)
- Scientific notation for absolute data and two significant digits shown for “Per 1M” data to improve readability
- Clickable legend to show or hide data at the plot level instead of having to go through the selection options again

### Unresolved Issues :( 

Although the app has significantly more features and improved user experience since the first version, there are still several known issues that remain.


1. When users select more than five or so fields in a dropdown, two issues occur. Firstly, the dropdown field does not expand large enough to view all the selected options. Secondly, the expanded text pushes down the remaining selection options beyond the grey box that highlights the selection options. In Milestone 4, we explored limiting the user to five inputs but there didn’t appear to be an easy way to implement this with the default parameters. 

2. Throughout Milestone 2 and 3, one of the documented areas of improvement was adding map interactiveness that would display on the plots. For example, a user could select a country by clicking on the map instead of selecting the dropdown to change the plots below. However, this was not implemented in the final version of the app. This was based on the decision that even though this type of interactivity was neat, it probably would not be a better user experience for selecting countries of a small geographical size compared to the dropdown.

### Final Thoughts

Building a dashboard is a lot harder than it seems! One of the major takeaways was to focus on the edge cases while testing the app. A recurring theme in the feedback was bugs that only occurred when the app was being used in a certain way. An example of this was the dropdowns being unable to show all the fields selected when a user chose more than five or so categories. Ultimately, the tradeoff was understanding whether it made more sense to fix edge case bugs or to focus our efforts on building new features that would be more widely used. 



