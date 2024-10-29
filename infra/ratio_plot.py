import argparse
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def plot_speed_graph_rival_iter(outcomes, args):
    # Create figure
    fig, ax = plt.subplots(figsize=(4, 3.5))
    fig.tight_layout(pad=2.0)
    
    # Drop precision column and sum up based on iteration
    outcomes = outcomes.drop(['baseline_precision'], axis=1)
    outcomes = outcomes.groupby(['rival_iter', 'tool_name'], as_index=False).sum()
    
    # Select appropriate tools
    baseline_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-baseline") & (outcomes['rival_iter'] > 0)]
    rival_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-rival") & (outcomes['rival_iter'] > 0)]
    sollya_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-sollya") & (outcomes['rival_iter'] > 0)]

    # Some weird functions that creates speed per millisecond for each tool
    def add_values(row):
        return int(row['rival_iter']), (row['number_of_points'] / row['time']) * 1000
    def tool_cmp2speed(x):
        return x.sort_values(by=['rival_iter']).apply(add_values, axis=1, result_type='expand')

    # Sollya timings considered are as base since we are doing speed ratio comparison
    base = np.array(tool_cmp2speed(sollya_cmp)[1])

    # Plot Rival
    ax.plot(tool_cmp2speed(rival_cmp)[0], np.array(tool_cmp2speed(rival_cmp)[1])/base, '.-', linewidth=2.0, color='r', label='reval')
    # Plot Baseline
    ax.plot(tool_cmp2speed(baseline_cmp)[0], np.array(tool_cmp2speed(baseline_cmp)[1])/base, '--', linewidth=2.0, color='g',
            label='baseline')
    # Plot Sollya
    ax.plot(tool_cmp2speed(sollya_cmp)[0], np.array(tool_cmp2speed(sollya_cmp)[1])/base, '-', linewidth=2.0, color='b',
            label='sollya')

    ax.legend()
    ax.set_xlabel("Difficulty")
    ax.set_ylabel("Ratio")
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)
    plt.savefig(args.path + "/ratio_plot_iter.png", format="png")
    plt.savefig(args.path + "/ratio_plot_iter.pdf", format="pdf")
    
    # Latex stuff
    print("\\newcommand{\RivalAvgSpeedupOverSollya}{" + str(round(sollya_cmp['time'].sum() / rival_cmp['time'].sum(), 2)) + "\\xspace}")
    print("\\newcommand{\RivalAvgSpeedupOverBaseline}{" + str(round(baseline_cmp['time'].sum() / rival_cmp['time'].sum(), 2)) + "\\xspace}")
    print("\\newcommand{\RivalMaxSpeedupOverSollya}{" + str(round(np.array(tool_cmp2speed(rival_cmp)[1])[-1]/np.array(tool_cmp2speed(sollya_cmp)[1])[-1], 2)) + "\\xspace}")
    print("\\newcommand{\RivalMaxSpeedupOverBaseline}{" + str(round(np.array(tool_cmp2speed(rival_cmp)[1])[-1]/np.array(tool_cmp2speed(baseline_cmp)[1])[-1], 2)) + "\\xspace}")

def plot_speed_graph_baseline_precision(outcomes, args):
    # Create figure
    fig, ax = plt.subplots(figsize=(4, 3.5))
    fig.tight_layout(pad=2.0)
    
    # Drop precision column and sum up based on iteration
    outcomes = outcomes.drop(['rival_iter'], axis=1)
    outcomes = outcomes.groupby(['baseline_precision', 'tool_name'], as_index=False).sum()
    
    # Select appropriate tools
    baseline_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-baseline") & (outcomes['baseline_precision'] > 73)]
    rival_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-rival") & (outcomes['baseline_precision'] > 73)]
    sollya_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-sollya") & (outcomes['baseline_precision'] > 73)]

    # Some weird functions that creates speed per millisecond for each tool
    def add_values(row):
        return int(row['baseline_precision']), (row['number_of_points'] / row['time']) * 1000
    def tool_cmp2speed(x):
        return x.sort_values(by=['baseline_precision']).apply(add_values, axis=1, result_type='expand')

    # Sollya timings considered are as base since we are doing speed ratio comparison
    base = np.array(tool_cmp2speed(sollya_cmp)[1])

    # Plot Rival
    ax.plot(tool_cmp2speed(rival_cmp)[0], np.array(tool_cmp2speed(rival_cmp)[1])/base, '.-', linewidth=2.0, color='r', label='reval')
    # Plot Baseline
    ax.plot(tool_cmp2speed(baseline_cmp)[0], np.array(tool_cmp2speed(baseline_cmp)[1])/base, '--', linewidth=2.0, color='g',
            label='baseline')
    # Plot Sollya
    ax.plot(tool_cmp2speed(sollya_cmp)[0], np.array(tool_cmp2speed(sollya_cmp)[1])/base, '-', linewidth=2.0, color='b',
            label='sollya')

    ax.legend()
    ax.set_xlabel("True uniform precision")
    ax.set_ylabel("Ratio")
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)
    plt.savefig(args.path + "/ratio_plot_precision.png", format="png")
    plt.savefig(args.path + "/ratio_plot_precision.pdf", format="pdf")
    
    # Latex stuff
    print("\\newcommand{\RivalAvgSpeedupOverSollya}{" + str(round(sollya_cmp['time'].sum() / rival_cmp['time'].sum(), 2)) + "\\xspace}")
    print("\\newcommand{\RivalAvgSpeedupOverBaseline}{" + str(round(baseline_cmp['time'].sum() / rival_cmp['time'].sum(), 2)) + "\\xspace}")
    print("\\newcommand{\RivalMaxSpeedupOverSollya}{" + str(round(np.array(tool_cmp2speed(rival_cmp)[1])[-1]/np.array(tool_cmp2speed(sollya_cmp)[1])[-1], 2)) + "\\xspace}")
    print("\\newcommand{\RivalMaxSpeedupOverBaseline}{" + str(round(np.array(tool_cmp2speed(rival_cmp)[1])[-1]/np.array(tool_cmp2speed(baseline_cmp)[1])[-1], 2)) + "\\xspace}")
    
def load_outcomes(path):
    outcomes = json.load(open(path, "r"))["outcomes"]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'rival_iter', 'baseline_precision', 'tool_name', 'number_of_points'])
    return outcomes

parser = argparse.ArgumentParser(prog='ratio_plot.py', description='Script outputs ratio plots')
parser.add_argument('-t', '--timeline', dest='timeline', default="report/timeline.json")
parser.add_argument('-o', '--output-path', dest='path', default="report")
args = parser.parse_args()

outcomes = load_outcomes(args.timeline)
plot_speed_graph_rival_iter(outcomes, args)
plot_speed_graph_baseline_precision(outcomes, args)

