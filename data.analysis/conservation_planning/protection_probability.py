"""
Script to plot the probability of protecting a unit that is amongst the most valuable for conservation purposes globally. 
"""

import seaborn as sns
import pandas as pd
from matplotlib import pyplot as plt

# data = pd.read_csv("./protection_probability.csv")

# data["Probability of protection"] = data.probProtection.apply(lambda l: l[1:-1].split(","))
# data = data.explode("Probability of protection").reset_index()
# data["Probability of protection"] = data["Probability of protection"].apply(lambda i: float(i))

# data["Average area of administrative regions \n relative to the landscape area"] = 1/data["administrativeRegions"]

# data["Average size of administrative regions"] = 4921/data["administrativeRegions"]

# sns.relplot(data, y="Probability of protection", x="Average size of administrative regions", linewidth=4.0, alpha=0.6,  kind="line", height=6, aspect=1.2, legend= True, errorbar="sd")
# plt.xscale("log")

# plt.show()


data = pd.read_csv("./realized_conservation_value.csv")

data["Realized conservation value relative to potential"] = data.realizedConservation.apply(lambda l: l[1:-1].split(","))
data = data.explode("Realized conservation value relative to potential").reset_index()
data["Realized conservation value relative to potential"] = data["Realized conservation value relative to potential"].apply(lambda i: float(i))

data["Average area of administrative regions \n relative to the landscape area"] = 1/data["administrativeRegions"]

data["Average size of administrative regions"] = 1261/data["administrativeRegions"]

data["Fraction protected"] = data["fractionProtected"]

sns.relplot(data, y="Realized conservation value relative to potential", x="Average size of administrative regions", linewidth=4.0, alpha=0.6,  kind="line", hue="Fraction protected", height=6, aspect=1.2, legend= True, errorbar="sd")
plt.xscale("log")

plt.show()