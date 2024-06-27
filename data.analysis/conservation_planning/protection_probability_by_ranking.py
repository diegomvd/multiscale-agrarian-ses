"""
Script to plot the probability of protecting a unit that is amongst the most valuable for conservation purposes globally. 
"""

import seaborn as sns
import pandas as pd
import numpy as np
import math
from matplotlib import pyplot as plt

# data = pd.read_csv("./protection_probability_ranking_large_scale.csv")


def str_to_2D_array(str):
    list = [str[2:-2].split("],[")[i].split(",") for i in range(len(str[2:-2].split("],[")))]
    arr = np.array(list,dtype="unicode_")
    return arr 

def count_protected(str_2D_array):
    return np.count_nonzero(str_2D_array == "Protected", axis=0)/str_2D_array.shape[0]

def protection_probability_ranking(str):
    prob = count_protected(str_to_2D_array(str))
    rank_prob = [ (i,p) for i,p in enumerate(prob)  ]
    return rank_prob

def analytical_mean_field_2_admin(n, fp, nt, na):
    m = np.ceil(fp*nt/na)
    
    sum = 0
    if n>m:
        factor = np.power(1/na,n)
        for i in range(int(n-m)):
            sum += factor*math.comb(n,n-i)
        print(sum)           

    prob = 1-sum
        
    return prob 

def analytical_mean_field(n, fp, nt, na):
    m = np.ceil(fp*nt/na)
    
    sum = 0
    if n>m:
        factor = np.power(1/na,n)
        for i in range(int(n-m)):
            comb_factor = factor*math.comb(n,i)
            insum = 0

            k_max = np.min([i,na-1])
            for k in range(k_max+1):
                prefactor = comb_factor*math.comb(na-1,na-1-k)
                # print("prefactor:{}".format(prefactor))
                ininsum=0
                for p in range(k+1):
                    # print("i: {}, k: {}, p: {}, na: {}".format(i,k,p,na))
                    sumando = prefactor * np.power(-1.0,p) * math.comb(k,p) * np.power((k-p),i)
                    ininsum += sumando
                    # print("ininsum:{}".format(ininsum))
                # print("ininsum:{}".format(ininsum))    
                insum += ininsum
            sum += insum    
        # print(sum)           

    prob = 1-sum
    print("prob:{} , rank:{}".format(prob,n))
        
    return prob 


# data["Administrative regions"] = data["administrativeRegions"].apply(lambda l: int(l))

# data["probProtectionRanking"] = data["probProtection"].apply(lambda l: protection_probability_ranking(l))

# data = data.explode("probProtectionRanking").reset_index()

# # print(pd.DataFrame(data["probProtectionRanking"].tolist(), index=data.index)
# # )

# data[['Ranking', 'Protection probability']] = pd.DataFrame(data["probProtectionRanking"].tolist(), index=data.index)

# print(data)
# print(data["Protection probability"])
# print(data["Ranking"])

# sns.relplot(data, y="Protection probability", x="Ranking", linewidth=4.0, alpha=0.8,  kind="line", hue="Administrative regions", height=6, aspect=1.2, legend= True)
# # plt.xscale("log")

# plt.show()

nt = 5*5*3 + 5*3 + 1
nas = [2,4,8]
fp = 0.3
rankings = np.arange(1, 5*5*3 + 5*3 + 1 + 1, 1)
print(rankings)
probabilities = {}
for na in nas:
    prob = [analytical_mean_field(int(n),fp,nt,na) for n in rankings] 
    probabilities[na]=prob
print(probabilities)
