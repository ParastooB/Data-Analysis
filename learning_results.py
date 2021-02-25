import numpy as np
import pandas as pd
import matplotlib.pyplot as plt 
import os
import array
import random
from matplotlib import offsetbox
import json

group_id = 2

df = pd.read_csv("rldata.csv", header='infer')


print(df.dtypes)
df_updated = df[(df['name'] == 'RL')]
df_notupdated = df[(df['name'] == 'RL-no-update')]

if not df_notupdated.empty:
    s = df_notupdated.iloc[0]['data']
    q = json.loads(s)
    print(q)
    state_size = q["state_size"]
    action_size = q["action_size"]
    discount_rate = q["discount_rate"]
    learning_rate = q["learning_rate"]
    exploration_rate = q["exploration_rate"]
    dialog_state_ids_size = q["dialog_state_ids_size"]

else:
    state_size = 4
    action_size = 4
    discount_rate = 0.2
    learning_rate = 0.6
    exploration_rate = 0.5
    dialog_state_ids_size = 1


indxs = df_updated.index.values.tolist()

rewards = []
r1s = []
r2s = []
qtabls = []
actions = []

for i in range(0,len(indxs)):
    lots = df_updated.iloc[i]['data']
    qlots = json.loads(lots)
    r1 = float(qlots["r1"])
    r2 = float(qlots["r2"])
    reward = float(qlots["reward"])
    rewards.append(reward)
    r1s.append(r1)
    r2s.append(r2)
    last_action = qlots["last_action"]
    actions.append(last_action)
    q_table_values = qlots["q_table_values"]
    print(q_table_values)
    # q_table_values.replace("\\n", ",")
    # print(q_table_values)
    qtabls.append(q_table_values)

def plots(arr1,arr2,title, axis1, axis2, legs,image):
    font = {'family' : 'DejaVu Sans','weight' : 'bold','size'   : 10}
    plt.rc('font', **font)

    plt.plot(arr2,ms= 10)
    plt.grid()
    plt.title(title)
    plt.ylabel(axis1)
    plt.xlabel(axis2)
    # plt.legend(legs)
    plt.show()
    # plt.savefig(image)

plots(indxs,r1s,"total overall time", "R1", "index", "r1","r1")
plots(indxs,r2s,"ratio of teammebers", "R2", "index", "r2","r1")
plots(indxs,rewards,"rewards over time", "R", "index", "r","r1")
plots(indxs,actions,"actions over time", "A", "index", "r","r1")
# print(rewards)
# print(qtabls)
