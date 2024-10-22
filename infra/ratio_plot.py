import argparse
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def plot_speed_graph(outcomes, ax):
    baseline_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-baseline") & (outcomes['rival_iter'] > 0)]
    rival_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-rival") & (outcomes['rival_iter'] > 0)]
    sollya_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-sollya") & (outcomes['rival_iter'] > 0)]

    def add_values(row):
        return int(row['rival_iter']), (row['number_of_points'] / row['time']) * 1000

    def tool_cmp2speed(x):
        return x.sort_values(by=['rival_iter']).apply(add_values, axis=1, result_type='expand')

    base = np.array(tool_cmp2speed(sollya_cmp)[1])

    ax.plot(tool_cmp2speed(rival_cmp)[0], np.array(tool_cmp2speed(rival_cmp)[1])/base, '.-', linewidth=2.0, color='r', label='reval')
    ax.plot(tool_cmp2speed(baseline_cmp)[0], np.array(tool_cmp2speed(baseline_cmp)[1])/base, '--', linewidth=2.0, color='g',
            label='baseline')
    ax.plot(tool_cmp2speed(sollya_cmp)[0], np.array(tool_cmp2speed(sollya_cmp)[1])/base, '-', linewidth=2.0, color='b',
            label='sollya')

    print("\\newcommand{\RivalAvgSpeedupOverSollya}{" + str(round(tool_cmp2speed(rival_cmp)[1].sum()/np.array(tool_cmp2speed(sollya_cmp)[1]).sum(), 2)) + "\\xspace}")
    print("\\newcommand{\RivalAvgSpeedupOverBaseline}{" + str(
        round(tool_cmp2speed(rival_cmp)[1].sum() / np.array(tool_cmp2speed(baseline_cmp)[1]).sum(), 2)) + "\\xspace}")

    print("\\newcommand{\RivalMaxSpeedupOverSollya}{" + str(round(np.array(tool_cmp2speed(rival_cmp)[1])[-1]/np.array(tool_cmp2speed(sollya_cmp)[1])[-1], 2)) + "\\xspace}")
    print("\\newcommand{\RivalMaxSpeedupOverBaseline}{" + str(round(np.array(tool_cmp2speed(rival_cmp)[1])[-1]/np.array(tool_cmp2speed(baseline_cmp)[1])[-1], 2)) + "\\xspace}")

    ax.legend()
    ax.set_xlabel("Difficulty")
    ax.set_ylabel("Ratio")
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)

def load_outcomes(path):
    outcomes = json.load(open(path, "r"))["outcomes"]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'rival_iter', 'tool_name', 'number_of_points'])
    return outcomes

parser = argparse.ArgumentParser(prog='histograms.py', description='Script outputs mixed precision histograms for a Herbie run')
parser.add_argument('-t', '--timeline', dest='timeline', default="report/timeline.json")
parser.add_argument('-o', '--output-path', dest='path', default="report")
args = parser.parse_args()

outcomes = load_outcomes(args.timeline)

fig, ax = plt.subplots(figsize=(4, 3.5))
fig.tight_layout(pad=2.0)
plot_speed_graph(outcomes, ax)
plt.savefig(args.path + "/ratio_plot.png", format="png")
plt.savefig(args.path + "/ratio_plot.pdf", format="pdf")
