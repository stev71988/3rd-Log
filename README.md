# 3rd-Log

3rd and Log was a project that started partially out of boredom but mainly out of stubborness. Back in 2016, I was having an “intense debate” with my boss about who was the best 4th quarter quarterback in the NFL. Obviously, I had to defend Matt Ryan, AKA Matty Ice, quarterback for the Atlanta Falcons. For some delusional reason, my boss kept insisting that Matthew Stafford, quarterback for the Detroit Lions, was the better 4th quarter quarterback.

This debate led us down a path where we started to wonder how outside factors, such as distance traveled for the away team or average size and age of their offensive lines, had any impact on the way teams performed, which then led to wondering if we could use these outside factors along with a team’s historical performance to predict wins and losses for the 2016 NFL season.

I decided to take this on as a side project and collaborated with our designers to make the visuals. I did not have any coding experience at the time, but thankfully we had a Data Strategist Intern who was well-versed in R to get us started. We worked together to build an algorithm that was based on an ELO rating system, which is a method for calculating the relative skill levels of players in zero-sum games such as chess. We then applied additional weights to each team’s ELO rating score based on various outside factors: distance traveled for away teams, average age of team, average weight of offensive/defensive lines, stadium loudness, etc. The algorithm would use these variables + the ELO rating score to calculate a win probability for each team.

Once the 2015 NFL season kicked off, I was responsible for maintaining the algorithm and conducting the analysis to determine which team was likely to win their matchup that week. We would also review our predictions from the previous week and write about what our mistakes were, and how we could improve for the next week.

At the end of the season, our algorithm ended up predicting 65.4% of the matchups correctly, beating the accuracy rating of Nate Silver and his website, FiveThirtyEight.com (64.5%).
