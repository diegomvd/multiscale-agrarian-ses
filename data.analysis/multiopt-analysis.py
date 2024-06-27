import numpy as np
import pandas as pd
import seaborn as sns
from matplotlib import pyplot as plt

files = ["./data/population12500_esarea_0.1.csv","./data/population13500_esarea_0.5.csv","./data/population14000_esarea_1.0.csv"]

esat = [0.1,0.5,1.0]
for i,file in enumerate(files): 
    data = pd.read_csv(file)
    print(data)

    # Management parameters in 2D: strategy vs. area with init agricultural as hue.
    # sns.scatterplot(data,x="fractionOfMngUnitsSparing",y="managementArea",hue="initFractionAgricultural")
    # plt.show()

    # sns.scatterplot(data,x="managementArea",y="fractionOfMngUnitsSparing",hue="initFractionAgricultural")
    # plt.show()


    # sns.scatterplot(data,x="initFractionAgricultural",y="fractionOfMngUnitsSparing",size="managementArea")
    # plt.show()

    data["pop"]=-data["objective$pop"]
    data["average"]=-data["objective$average"]
    data["rob"]=-data["objective$robustness"]
    data["espop"] = data["average"]/data["pop"]
    data["avrob"] = data["rob"]/data["average"]

    data["Management Strategy"] = np.where( data["fractionOfMngUnitsSparing"] < 0.333, "Most Sharing", "none"  )
    data["Management Strategy"] = np.where( data["fractionOfMngUnitsSparing"] > 0.666, "Most Sparing", data["Management Strategy"]  )
    data["Management Strategy"] = np.where( data["Management Strategy"] == "none", "Mixed", data["Management Strategy"]  )

    data["Management Area"] = np.where( data["managementArea"] <= 0.066666, "Small", "none"  )
    data["Management Area"] = np.where( data["managementArea"] > 0.13333, "Large", data["Management Area"]  )
    data["Management Area"] = np.where( data["Management Area"] == "none", "Medium", data["Management Area"]  )

    print(data["Management Area"])

    max_production = 3*25**2+3*25+1
    data["pop"] = data["pop"]/max_production
    

    sns.set_style("dark")
    pal1 = sns.diverging_palette(230, 21, s=90, as_cmap=True)
    pal1bis = sns.diverging_palette(230, 21, s=90, n=3)
    palette = {"Most Sharing" : pal1bis[0], "Mixed": pal1bis[1], "Most Sparing": pal1bis[2]}
    

    # sns.set(rc={'figure.figsize':(6,6), "figure.style":"dark"})
    sizes = {"Small": 30.0, "Medium":80.0 , "Large":180.0 }
    sns.relplot(data, y="pop", x="initFractionAgricultural", hue="Management Strategy",s=100, linewidth=1.0, alpha=0.7, edgecolor="k", size="Management Area", kind="scatter", height=5, aspect=1.2, palette=palette, sizes = sizes, legend= True)
    plt.ylabel("Agricultural production")
    plt.xlabel("Fraction of agricultural land")
    plt.tight_layout()
    # plt.show()
    plt.savefig("production_esat_with_legend_{}.svg".format(esat[i]), format = "svg")

    # pal2 = sns.diverging_palette(145, 320, s=75, as_cmap=True)


    # g = sns.JointGrid(ratio=3,space=0.05, height=6)
    # x, y = data["fractionOfMngUnitsSparing"], data["managementArea"]
    # sns.scatterplot( x = x, y = y,
    #                  alpha=0.7, s = 100, linewidth=1.0, edgecolor="k",
    #                  hue=data["Management Strategy"], 
    #                  palette= palette,
    #                  ax = g.ax_joint, legend=False  )
    # g.ax_joint.set_xlabel("Fraction of land-sparing management units")
    # g.ax_joint.set_ylabel("Average area of management units")
    # sns.histplot(x=x, hue = data["Management Strategy"], palette=palette, alpha=0.7,fill=True, ax = g.ax_marg_x, bins=3, legend=False, edgecolor="k")
    # sns.histplot(y=y, hue = data["Management Strategy"], palette=palette, alpha=0.7,fill=True, bins=5, multiple='stack', ax = g.ax_marg_y, legend=False, edgecolor="k")
    # plt.tight_layout()
    # # plt.show()

    # plt.savefig("strategy_area_esat_{}.svg".format(esat[i]), format = "svg")

    # pal = sns.diverging_palette(145, 320, s=75, as_cmap=True)
    # sns.jointplot(data, x="fractionOfMngUnitsSparing", y="managementArea", alpha=0.6, s = 100, linewidth=1.0, edgecolor="k", ratio = 3, hue = "initFractionAgricultural", palette=pal )
    # plt.show()

    # sns.scatterplot(data,x="initFractionAgricultural",y="managementArea",hue="fractionOfMngUnitsSparing",s=60, linewidth=1.0, alpha=0.8, edgecolor="k", palette=pal)
    # plt.show()

    # pal = sns.diverging_palette(145, 320, s=75, as_cmap=True)
    # sns.jointplot(data, y="fractionOfMngUnitsSparing", x="managementArea",marginal_kws=dict(bins=5, fill=True), alpha=0.6, kind="hist")
    # plt.show()

    # pal = sns.diverging_palette(230, 21, s=90, as_cmap=True)
    # sns.scatterplot(data, y="pop", x="initFractionAgricultural", hue="fractionOfMngUnitsSparing", size="managementArea" ,sizes=(30.0,160.0), linewidth=1.0, alpha=0.7, edgecolor="k",  palette=pal)
    # plt.show()

    # pal = sns.diverging_palette(230, 21, s=90, as_cmap=True)
    # sns.scatterplot(data, y="rob", x="espop", hue="fractionOfMngUnitsSparing", size="managementArea" ,sizes=(30.0,160.0), linewidth=1.0, alpha=0.7, edgecolor="k",  palette=pal)
    # plt.xscale("log")
    # plt.show()

    # pal = sns.diverging_palette(230, 21, s=90, as_cmap=True)
    # sns.scatterplot(data, y="rob", x="average", hue="fractionOfMngUnitsSparing", size="pop" ,sizes=(5.0,180.0), linewidth=1.0, alpha=0.7, edgecolor="k",  palette=pal)
    # plt.show()

    # pal = sns.diverging_palette(230, 21, s=90, as_cmap=True)
    # sns.scatterplot(data, y="pop", x="avrob", hue="fractionOfMngUnitsSparing", size="managementArea" ,sizes=(30.0,160.0), linewidth=1.0, alpha=0.7, edgecolor="k",  palette=pal)
    # plt.xscale("log")
    # # plt.yscale("log")
    # plt.show()