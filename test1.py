import pandas as pd
import matplotlib.pyplot as plt
d1 = pd.read_csv("Data Files/nfl-passing-statistics/NFL_passing_statistics_1970to2023.csv")

# %%
# for all teams
plt.scatter(d1.quarterback_wins, d1.passing_tds)
plt.xlabel('wins')
plt.ylabel("touchdowns")
plt.show()

# %%

# %%
# filter data for patriots
pats_d1 = d1[d1.team == "NE"]
plt.plot(pats_d1.season, pats_d1.quarterback_wins)

# %% 
# %%
import numpy as np
# %%
