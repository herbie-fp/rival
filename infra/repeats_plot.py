import argparse
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
def plot_repeats_plot(outcomes, args):
    # Create figure
    fig, ax = plt.subplots(figsize=(4, 2.5))
    fig.tight_layout(pad=2.0)
    
    # Drop precision column and sum up based on iteration
    rival = (outcomes.loc[(outcomes['tool'] == "rival") & (outcomes['iter'] > 0)]).sort_values(by=['iter'])
    rival_no_repeats = (outcomes.loc[(outcomes['tool'] == "rival-no-repeats") & (outcomes['iter'] > 0)]).sort_values(by=['iter'])
    baseline = (outcomes.loc[(outcomes['tool'] == "baseline") & (outcomes['iter'] > 0)]).sort_values(by=['iter'])
    
    average = round((1.0 - (rival['number_of_instr_executions'].sum() / rival_no_repeats['number_of_instr_executions'].sum())) * 100, 2)
    print("\\newcommand{\\AveragePercentageOfSkippedInstr}{" + str(average) + "}")
    maximum = round((1.0 - (np.array(rival['number_of_instr_executions'])[-1] / np.array(rival_no_repeats['number_of_instr_executions'])[-1])) * 100, 2)
    print("\\newcommand{\\MaximumPercentageOfSkippedInstr}{" + str(maximum) + "}")

    ax.bar(np.arange(len(baseline)) + 0.925, 100, color="green", alpha=1, width=0.5, label='baseline', hatch='/')
    percentages = np.array(rival['number_of_instr_executions']) / np.array(rival_no_repeats['number_of_instr_executions']) * 100
    ax.bar(np.arange(len(rival)) + 1.075, percentages, color="red", alpha=0.7, width=0.5, label='reval')

    ax.legend()
    ax.set_xlabel("Iteration")
    ax.set_ylabel("Instructions executed %")
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
matplotlib.rcParams.update({'font.size': 12})
plot_repeats_plot(outcomes, args)
