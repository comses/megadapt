#!/usr/bin/env python
import numpy as np
import argparse

parser = argparse.ArgumentParser(description='Create a condor submit file for batch run')
parser.add_argument('--experiment_name', required=True)
parser.add_argument('--mental_models', default="mental_model_constant")
parser.add_argument('--ponding_model', default="delta")
parser.add_argument('--flooding_model', default="delta")
parser.add_argument('--budget_model', default="split")
args = parser.parse_args()





submitfile_template = """

executable = run_cli.R

Error = log/row_$(Row).err
Log = log/row_$(Row).log

queue arguments from (
%s)

"""


steps = 40
repetitions = 5
experiment = args.experiment_name
climate_scenario = 1

arguments_template = "\t--experiment %s --effectiveness_new_infra %0.2f --effectiveness_maintenance %0.2f --steps %s --infrastructure_decay %0.2f --budget %s --half_sensitivity_d %s --half_sensitivity_ab %s --climate_scenario %s --rep %s --key %s --mental_models %s --ponding_model %s --flooding_model %s --budget_model %s\n"

key = 0
args_table = ""
for rep in range(repetitions):
    for effectiveness_new_infra in np.linspace(0.05, 0.1, 2):
        for effectiveness_maintenance in np.linspace(0.05, 0.1, 2):
            for infrastructure_decay in np.linspace(0.05, 0.1, 2):
                for budget in np.linspace(120, 1200, 5):
                    for half_sensitivity_d in np.linspace(5, 10, 2):
                        for half_sensitivity_ab in np.linspace(5, 10, 2):
                            args_table += arguments_template % (experiment,
                                                                effectiveness_new_infra,
                                                                effectiveness_maintenance,
                                                                steps,
                                                                infrastructure_decay,
                                                                budget,
                                                                half_sensitivity_d,
                                                                half_sensitivity_ab,
                                                                climate_scenario,
                                                                rep,
                                                                key,
                                                                args.mental_models,
                                                                args.ponding_model,
                                                                args.flooding_model,
                                                                args.budget_model)
                            key += 1


with open("%s.sub" % args.experiment_name, 'w') as f:
    f.write(submitfile_template % args_table)
