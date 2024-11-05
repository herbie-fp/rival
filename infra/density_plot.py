import numpy as np
import requests
from matplotlib import pyplot as plt, ticker
import pandas as pd
import json
import argparse

def load_outcomes(path):
    outcomes = json.load(open(path, "r"))["density"]
    outcomes = pd.DataFrame(outcomes, columns=['tool', 'precision', 'count'])
    return outcomes

def plot_density(args):
    outcomes = load_outcomes(args.timeline)
    baseline = outcomes[outcomes['tool'] == "baseline"].drop('tool', axis=1)
    rival = outcomes[outcomes['tool'] == "rival"].drop('tool', axis=1)
    
    max_precision = max(baseline['precision'].max(), rival['precision'].max())
    min_precision = min(baseline['precision'].min(), rival['precision'].min())
    
    fig, ax = plt.subplots(figsize=(4, 3.5))
    fig.tight_layout(pad=2.0)
    
    # Normalizing precisions
    rival['precision'] = (np.array(rival['precision']) - min_precision) / (max_precision - min_precision)
    rival['precision'] = rival['precision'] - rival['precision'] % 0.1
    rival = rival.groupby(by=['precision'], as_index=False).sum()
    
    baseline['precision'] = (np.array(baseline['precision']) - min_precision) / (max_precision - min_precision)
    baseline['precision'] = baseline['precision'] - baseline['precision'] % 0.1
    baseline = baseline.groupby(by=['precision'], as_index=False).sum()

    ax.bar(baseline['precision']+0.035, baseline['count'], color="green", alpha=1, width=0.07, label='baseline', hatch='/')
    ax.bar(rival['precision']+0.055, rival["count"], color="red", alpha=0.7, width=0.07, label='reval')
    
    ax.set_ylabel("Number of operations")
    ax.set_xlabel("Precision (normalized)")
    
    plt.legend()
    plt.tight_layout()
    plt.savefig(args.path + "/density_plot.pdf", format="pdf")
    plt.savefig(args.path + "/density_plot.png", format="png")

parser = argparse.ArgumentParser(prog='histograms.py', description='Script outputs mixed precision histograms for a Herbie run')
parser.add_argument('-t', '--timeline', dest='timeline', default="report/timeline.json")
parser.add_argument('-o', '--output-path', dest='path', default="report")

args = parser.parse_args()
plot_density(args)
