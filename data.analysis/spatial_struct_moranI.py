import numpy as np
import pandas as pd
import seaborn as sns
from matplotlib import pyplot as plt

moran = pd.read_csv("./data/spatial_struct/moranI.csv")

moran["Management Strategy"] = np.where( moran["fractionOfMngUnitsSparing"] < 0.333, "Most Sharing", "none"  )
moran["Management Strategy"] = np.where( moran["fractionOfMngUnitsSparing"] > 0.666, "Most Sparing", moran["Management Strategy"]  )
moran["Management Strategy"] = np.where( moran["Management Strategy"] == "none", "Mixed", moran["Management Strategy"]  )

sparing = np.unique(moran["Management Strategy"]) 
m_areas = np.unique(moran["managementArea"])

moran["moranI"] = moran.moranI.apply(lambda l: l[1:-1].split(","))
moran = moran.explode("moranI")
moran["moranI"] = moran.moranI.apply(lambda i: float(i))


sns.set_style("dark")
palette = sns.color_palette("flare", as_cmap=True)

for s in sparing:
    for ma in m_areas:
        data = moran[ (moran["Management Strategy"] == s) & (moran["managementArea"] == ma) ]

        g = sns.relplot(data, y="moranI", x="r", hue="initFractionAgricultural", linewidth=4.0, alpha=0.7,  kind="line", height=5, aspect=1.,palette = palette, legend= True)
        plt.ylabel("Spatial autocorrelation (Moran's I)")
        plt.xlabel("Distance")
        g.ax.axhline(0, ls='--',color="w",linewidth=3.5,zorder=0)
        g.ax.set_ylim(-0.32,0.82)
        plt.title(str(s) + "--" + str(ma))
        plt.tight_layout()


        # plt.show()
        plt.savefig("./figures/moranI_corrected_mixed_strategy_{}_mngarea_{}.svg".format(s,ma), format = "svg")