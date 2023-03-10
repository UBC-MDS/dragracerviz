# Motivation and purpose

Our motivation behind designing the dragracer_viz app is to provide a tool that can help fans of RuPaul's Drag Race to easily explore and analyze the performance of each drag queen on the show, across all seasons and episodes. 

For more context: <br>
**What is RuPaul's Drag Race and What is our purpose for designing this app?** <br>
It is a reality TV show that originated in the US, where drag queens compete to become America's Next Drag Superstar. The show is hosted by drag queen RuPaul, who is a cultural icon and is widely regarded as one of the most influential drag queens in the world. The show has gained a huge following since its debut in 2009 and has since spawned numerous spin-off shows and international versions.

On the show, contestants compete in various challenges, such as acting, singing, and fashion design, with the aim of impressing the judges and avoiding elimination. The contestants are judged based on their performances, runway looks, and overall charisma, uniqueness, nerve, and talent (or "C-U-N-T," as it's known on the show). The contestants also participate in lip-sync battles, where the bottom two performers lip-sync for their lives, with one being eliminated from the competition.

The show has gained a reputation for promoting inclusivity, diversity, and acceptance, with a large and loyal fan base across the world. The show has also helped to raise awareness of the drag community and has provided a platform for drag queens to showcase their talent and artistry to a wider audience. 

**Our Purpose:**<br>
With a large number of drag queens and seasons, it can be difficult for fans to keep track of each queen's performance and progress. The purpose of the app is to provide an interactive and user-friendly platform for fans to answer their research questions and make informed decisions on which drag queens to follow or see in live performances.

# Description of the data

Our dashboard will visualize data on about 184 drag queens that have appeared on RuPaul's Drag Race over 14 seasons. We will be using the {dragracer} R package, which provides the following three data sets: `rpdr_contestants`, with contestant-level data, `rpdr_contep`, with contestant and episode level data, and `rpdr_ep` with episode-level data. This package is available on CRAN, and can be found with installation instructions [here](https://cran.r-project.org/web/packages/dragracer/readme/README.html). The latitude and longitude are sourced from [simplemaps.com](https://simplemaps.com/data/us-cities) using their Basic Database. We will be joining these three data sets into one dataframe containing the main variables of interest. 

The final dataframe, called `rpdr`, will include variables that describe general information on the queens (`contestant`, `age`, `city`, `state`, `lng`, `lat`), the season they appeared on (`season`), and  how they fared throughout their competition (`rank`, `outcome`). The `outcome` describes their status at the end of a particular episode, which are WIN, HIGH, SAFE, LOW, and BTM, from highest to lowest score. In the app, these variables will be portrayed as part of the summary results for the chosen queen. The dataframe will also include some special features, such as whether the queen was eliminated (`eliminated`) or awarded the title "Miss Congeniality" (`missc`), which is given to the kindest queen of the season. We will also derive some additional features from the data that will indicate whether the queens where finalists and/or winners in their respective seasons (`finalist`, `winner`). These special features will act as filters and help our app users to search for a queen of interest based on their desired criteria.

# Research questions and usage scenarios

## Research questions

Possible research questions that can be answered by this visualization are as follows:

1.  What was the progression of each drag queen on each season?

2.  Which drag queen has the highest win right across all seasons?

3.  Who was the youngest drag queen on any season?

4.  Which drag queens came from each state?

## Usage scenario

James is a frequent watcher of RuPaul's Drag Race. He has seen every season of the American franchise however, he struggles to remember all the drag queens, and how they did, and each season with the sheer volume of seasons at this point.

With the increasing popularity of the show, more queens have started to go on tour. However, James does not have the money to go see every drag queen that comes to town. He would like a tool to figure out how each queen performs in their season, and overall compared to all the queens in each season. The visualization app dragracer_viz can help James with his problem. This app can get the overall performance of each drag queen on a season or overall on the franchise. He can also figure out how old each queen was when they were on their season and where their hometown is located. Finally, he can get information on special categories like `winners`, `finalists`, `first eliminated` and `miss congeniality`.

Following this, James can make an informed decision on which drag queen he would like to spend his money to go see when they come to town.