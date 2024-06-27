import pandas as pd
import seaborn as sns
from matplotlib import pyplot as plt
import numpy as np

data = pd.read_csv("./data/dynamics/exploration_large_landscape_20.csv")


data["time"] = data["time"].apply(lambda l: l[1:-1].split(","))
data["pop"] = data["pop"].apply(lambda l: l[1:-1].split(","))
data["deg"] = data["deg"].apply(lambda l: l[1:-1].split(","))
data["nat"] = data["nat"].apply(lambda l: l[1:-1].split(","))
data["lia"] = data["lia"].apply(lambda l: l[1:-1].split(","))
data["hia"] = data["hia"].apply(lambda l: l[1:-1].split(","))

data["time"] = data["time"].apply(lambda l: [float(x) for x in l])
data["pop"] = data["pop"].apply(lambda l: [float(x) for x in l])
data["deg"] = data["deg"].apply(lambda l: [float(x) for x in l])
data["nat"] = data["nat"].apply(lambda l: [float(x) for x in l])
data["lia"] = data["lia"].apply(lambda l: [float(x) for x in l])
data["hia"] = data["hia"].apply(lambda l: [float(x) for x in l])



data = data.explode(["time","deg","nat","pop","lia","hia"]).reset_index(drop=True)
data["time"] = data.time.apply(lambda t: round(t,1)) 
print(data)

# # print(data["pop"][0])


# sns.relplot(x=data["time"][0],y=data["pop"][0], kind="line")
# plt.show()

data["ecoServicesMaxArea"] = pd.cut(
    data['ecoServicesMaxArea'],
    bins=np.linspace(data.ecoServicesMaxArea.min(),data.ecoServicesMaxArea.max(),5), include_lowest=True,
    labels=["Very small", 'Small', 'High', 'Very high']
)
data["initFractionAgricultural"] = pd.cut(
    data['initFractionAgricultural'],
    bins=np.linspace(data.initFractionAgricultural.min(),data.initFractionAgricultural.max(),5), include_lowest=True,
    labels=["Very small", 'Small', 'High', 'Very high']
)
data["fractionOfMngUnitsSparing"] = pd.cut(
    data['fractionOfMngUnitsSparing'],
    bins=np.linspace(data.fractionOfMngUnitsSparing.min(),data.fractionOfMngUnitsSparing.max(),5), include_lowest=True,
    labels=["Very small", 'Small', 'High', 'Very high']
)
data["managementArea"] = pd.cut(
    data['managementArea'],
    bins=np.linspace(data.managementArea.min(),data.managementArea.max(),5), include_lowest=True,
    labels=["Very small", 'Small', 'High', 'Very high']
)
print(data)

sns.relplot(
    data = data,
    x="time",
    y="pop",
    kind="line",
    col = "fractionOfMngUnitsSparing",
    row = "managementArea",
    hue="initFractionAgricultural",
    #style = "ecoServicesMaxArea",
    palette = sns.color_palette("crest",n_colors=4), 
    **dict(estimator="mean")
)
plt.show()

# for emax in data.ecoServicesMaxArea.unique():

#     print(data[data.ecoServicesMaxArea==emax])
#     data2 = data[data.ecoServicesMaxArea==emax]
#     sns.relplot(data = data2 , x="time", y="pop", kind="line", col="fractionOfMngUnitsSparing", row="initFractionAgricultural",**dict(estimator="mean",errorbar="sd",))
#     plt.show()

# sns.relplot(data =data , x="time",y="nat", kind="line", col="fractionOfMngUnitsSparing",hue="managementArea")
# # plt.show()

# sns.relplot(data =data , x="time",y="deg", kind="line", col="fractionOfMngUnitsSparing",hue="managementArea")
# # plt.show()

# sns.relplot(data =data , x="time",y="lia", kind="line", col="fractionOfMngUnitsSparing",hue="managementArea")
# # plt.show()

# sns.relplot(data =data , x="time",y="hia", kind="line", col="fractionOfMngUnitsSparing",hue="managementArea")
# plt.show()
# # sns.relplot(data=data,x="time",y="")
# # sns.relplot(data=data,x="time",y="pop")