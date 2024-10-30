import argparse
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def plot_repeats_plot(outcomes, args):
    # Create figure
    fig, ax = plt.subplots(figsize=(4, 3.5))
    fig.tight_layout(pad=2.0)
    
    # Drop precision column and sum up based on iteration
    rival = (outcomes.loc[(outcomes['tool'] == "rival") & (outcomes['iter'] > 0)]).sort_values(by=['iter'])
    baseline = (outcomes.loc[(outcomes['tool'] == "baseline") & (outcomes['iter'] > 0)]).sort_values(by=['iter'])

    ax.bar(np.arange(len(baseline)) + 0.9, baseline['number_of_instr_executions'], color="green", alpha=1, width=0.4, label='baseline', hatch='/')
    ax.bar(np.arange(len(rival)) + 1.1, rival['number_of_instr_executions'], color="red", alpha=0.7, width=0.4, label='reval')
    
    ax.legend()
    ax.set_xlabel("Iteration")
    ax.set_ylabel("Number of instruction executions")
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)
    plt.tight_layout()
    plt.savefig(args.path + "/repeats_plot.png", format="png")
    plt.savefig(args.path + "/repeats_plot.pdf", format="pdf")
    
def load_outcomes(path):
    outcomes = json.load(open(path, "r"))["instr-executed-cnt"]
    outcomes = pd.DataFrame(outcomes, columns=['tool', 'iter', 'number_of_instr_executions'])
    return outcomes

parser = argparse.ArgumentParser(prog='repeats_plot.py', description='Script outputs repeats plots')
parser.add_argument('-t', '--timeline', dest='timeline', default="report/timeline.json")
parser.add_argument('-o', '--output-path', dest='path', default="report")
args = parser.parse_args()

outcomes = load_outcomes(args.timeline)
plot_repeats_plot(outcomes, args)

