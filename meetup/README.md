
# Overview #

This is an analysis of tech meetups in Bangalore. Data was obtained from [Meetup website](http://meetup.com). Category "Tech" was selected within a 2-mile radius in and around Bangalore. By scrolling down, all meetups were listed on the page. The HTML was manually copied. Individual meetup pages were later downloaded and parsed by an R script. Data analysis is done is R.

An analysis done in early April 2017 is [described on IEDF blog with relevant visualizations](https://iedf.in/index.php/blog/item/an-analysis-of-tech-meetup-groups-in-bangalore).

# Meetup API #
Meetup offers an API. Since this requires a valid login, the API can be accessed from the web browser after logging into Meetup site. Response will be in JSON format. The browser will parse and display this response neatly but this is not suitable for copying. Hence we need to use the browser's developer tools to copy the JSON response manually.

It may not be possible to obtain full data of a particular meetup group if you are not a member of that meetup group.

It has been seen that the API offers by default 200 records per page. We can obtain a maximum of 500 records per page. The following APIs are suited for this analysis:
* Get all categories: https://api.meetup.com/find/topic_categories
* Get relevant meetup groups, where category IDs can be obtained from the API call noted above: 
    + "Tech" category page 0: https://api.meetup.com/find/groups?zip=560001&radius=2&category=34&order=members&page=500
    + "Tech" category page 1: https://api.meetup.com/find/groups?zip=560001&radius=2&category=34&order=members&offset=1&page=500
    + "Career & Business" category page 0: https://api.meetup.com/find/groups?zip=560001&radius=2&category=2&order=members&page=500


# Future Improvements #
* Current implementation uses HTML responses. The code can be changed to use the API and parse JSON responses.
* We are now considering only "Tech" category but some relevant meetup groups are in "Career & Business" category. Meetup currently puts a group in one and only category. We can also start looking at the "Career & Business" category and pick only tech-related groups from this category.
