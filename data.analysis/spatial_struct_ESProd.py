import numpy as np
import pandas as pd
import seaborn as sns
from matplotlib import pyplot as plt

data = pd.read_csv("./data/spatial_struct/spatial_stats_ES_Prod.csv")

data["Management Strategy"] = np.where( data["fractionOfMngUnitsSparing"] < 0.333, "Most Sharing", "none"  )
data["Management Strategy"] = np.where( data["fractionOfMngUnitsSparing"] > 0.666, "Most Sparing", data["Management Strategy"]  )
data["Management Strategy"] = np.where( data["Management Strategy"] == "none", "Mixed", data["Management Strategy"]  )

sparing = np.unique(data["Management Strategy"]) 
m_areas = np.unique(data["managementArea"])
f_agr = np.unique(data["initFractionAgricultural"])
e_sat = np.unique(data["ecoServicesMaxArea"])

data["moranEs"] = data.moranEs.apply(lambda l: l[1:-1].split(","))
data["moranProd"] = data.moranProd.apply(lambda l: l[1:-1].split(","))
data["cESProd"] = data.cESProd.apply(lambda l: l[1:-1].split(","))

sns.set_style("dark")
palette = sns.color_palette("flare", as_cmap=True)


"""
Correlation heat-map.
"""
# data2 = data.explode("cESProd").reset_index()
# data2["cESProd"] = data2.cESProd.apply(lambda i: float(i))
# palette_map = sns.diverging_palette(230, 21, s=90, as_cmap=True)
# for f in f_agr:
#     print(f)
#     df = data2[ data2.initFractionAgricultural == f ]
#     df = df[ ["Management Strategy", "managementArea", "cESProd"] ]
#     df = pd.pivot_table(df, values='cESProd', index = "managementArea", columns = "Management Strategy", aggfunc=np.mean)

#     df = df[["Most Sharing", "Mixed", "Most Sparing"]]
#     df.columns = pd.CategoricalIndex(df.columns, categories= ["Most Sharing", "Mixed", "Most Sparing"])
#     print(df)

#     sns.heatmap(df, annot=True, vmin=-1, vmax=0.5, cmap = palette_map )
#     plt.show()
#     # break


data2 = data.explode("moranEs")#.reset_index()
data2["moranEs"] = data2.moranEs.apply(lambda i: float(i))

data3 = data.explode("moranProd")#.reset_index()
data3["moranProd"] = data3.moranProd.apply(lambda i: float(i))

data2["moranI"] = data2["moranEs"]
data2["obs"] = "Ecosystem Services"

data3["moranI"] = data3["moranProd"]
data3["obs"] = "Agricultural Production"

cols = ["moranI","r","managementArea","Management Strategy","ecoServicesMaxArea","initFractionAgricultural","obs"]

data2 = data2[cols]
data3 = data3[cols]

df =pd.concat([data2,data3])
print(df)

g = sns.relplot(df, y="moranI", x="r", hue="obs", linewidth=4.0, alpha=0.6,  kind="line", height=4, aspect=.8, legend= True,errorbar="sd",row="managementArea", col = "Management Strategy")
g.set_axis_labels("Distance", "Spatial Autocorrelation (Moran's I)")
for row in g.axes:
    for ax in row:
        ax.axhline(0, ls='--',color="w",linewidth=3.5,zorder=0)
plt.tight_layout()
plt.show()

# for s in sparing:
#     for ma in m_areas:
# df = data2[ (data2["ecoServicesMaxArea"] == e) ]

# df = data2[ ["moranEs","r","managementArea","Management Strategy","ecoServicesMaxArea","initFractionAgricultural"] ]
# # g = sns.relplot(df, y="moranEs", x="r", hue="initFractionAgricultural", linewidth=4.0, alpha=0.7,  kind="line", height=5, aspect=1.,palette = palette, legend= True)
# g = sns.relplot(df, y="moranEs", x="r", linewidth=4.0, alpha=0.7,  kind="line", height=5, aspect=1., legend= True,errorbar="sd",row="managementArea", col = "Management Strategy")
# # g = sns.relplot(df, y="moranEs", x="r", hue="initFractionAgricultural", alpha=0.7,  kind="scatter", height=5, aspect=1.,palette = palette, legend= True)
# # plt.ylabel("Spatial autocorrelation (data's I)")
# # plt.xlabel("Distance")
# # g.ax.axhline(0, ls='--',color="w",linewidth=3.5,zorder=0)
# # g.ax.set_ylim(-0.32,1.0)
# # plt.title( str(e))
# plt.tight_layout()

# # plt.show()
#             # plt.savefig("./figures/moranI_corrected_mixed_strategy_{}_mngarea_{}.svg".format(s,ma), format = "svg")


# data2 = data.explode("moranProd").reset_index()
# data2["moranProd"] = data2.moranProd.apply(lambda i: float(i))
# df = data2[ ["moranProd","r","managementArea","Management Strategy","ecoServicesMaxArea","initFractionAgricultural"] ]
# # g = sns.relplot(df, y="moranEs", x="r", hue="initFractionAgricultural", linewidth=4.0, alpha=0.7,  kind="line", height=5, aspect=1.,palette = palette, legend= True)
# sns.relplot(df, y="moranProd", x="r", linewidth=4.0, alpha=0.7,  kind="line", height=5, aspect=1., legend= True,errorbar="sd",row="managementArea", col = "Management Strategy")
# # g = sns.relplot(df, y="moranEs", x="r", hue="initFractionAgricultural", alpha=0.7,  kind="scatter", height=5, aspect=1.,palette = palette, legend= True)
# # plt.ylabel("Spatial autocorrelation (data's I)")
# # plt.xlabel("Distance")
# # g.ax.axhline(0, ls='--',color="w",linewidth=3.5,zorder=0)
# # g.ax.set_ylim(-0.32,1.0)
# # plt.title( str(e))
# plt.tight_layout()

# plt.show()
           