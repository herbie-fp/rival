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
    rival = (outcomes.loc[(outcomes['tool'] == "rival")]).sort_values(by=['iter'])
    rival_no_repeats = (outcomes.loc[(outcomes['tool'] == "rival-no-repeats")]).sort_values(by=['iter'])
    baseline = (outcomes.loc[(outcomes['tool'] == "baseline")]).sort_values(by=['iter'])

    # ax.bar(np.arange(len(baseline)) - 0.075, 100, color="green", alpha=1, width=0.5, label='baseline', hatch='/')
    percentages = (1.0 - np.array(rival['number_of_instr_executions']) / np.array(rival_no_repeats['number_of_instr_executions'])) * 100
    ax.bar(np.arange(len(rival)), percentages, color="red", alpha=0.7, label='reval')

    average = round((1.0 - (rival['number_of_instr_executions'].sum() / rival_no_repeats['number_of_instr_executions'].sum())) * 100, 2)
    print("\\newcommand{\AveragePercentageOfSkippedInstr}{" + str(average) + "\\xspace}")
    maximum = round((1.0 - (np.array(rival['number_of_instr_executions'])[-1] / np.array(rival_no_repeats['number_of_instr_executions'])[-1])) * 100, 2)
    print("\\newcommand{\MaximumPercentageOfSkippedInstr}{" + str(maximum) + "\\xspace}")

    # Print percentages
    for bar in ax.patches:
        if bar.get_height() == 0:
            ax.text(
                bar.get_x() + bar.get_width() / 2,
                bar.get_height() + bar.get_y() - bar.get_height()/2 + 0.5,
                str(round(bar.get_height(), 2)) + "%",
                ha='center',
                color='black')
        else:
            ax.text(
                bar.get_x() + bar.get_width() / 2,
                bar.get_height() + bar.get_y() - bar.get_height()/2 + 0.5,
                str(round(bar.get_height(), 2)) + "%",
                ha='center',
                color='black')
    
    ax.legend()
    ax.set_xlabel("Iteration")
    ax.set_ylabel("Percentage of instructions skipped")
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
