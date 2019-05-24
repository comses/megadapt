import numpy as np


submitfile_template = """

executable = run_cli.R

Error = log/row_$(Row).err
Log = log/row_$(Row).log

queue arguments from (
%s)

"""


steps = 40
repetitions = 10
prefix = 'trial'

arguments_template = "\t--effectiveness_new_infra %0.2f --effectiveness_maintenance %0.2f --steps %s --infrastructure_decay %0.2f --budget %s --half_sensitivity_d %s --half_sensitivity_ab %s --rep %s --prefix %s\n"


args_table = ""
for rep in range(repetitions):
    for effectiveness_new_infra in np.linspace(0.05, 0.1, 2):
        for effectiveness_maintenance in np.linspace(0.05, 0.1, 2):
            for infrastructure_decay in np.linspace(0.05, 0.1, 2):
                for budget in np.linspace(120, 1200, 5):
                    for half_sensitivity_d in np.linspace(5, 10, 2):
                        for half_sensitivity_ab in np.linspace(5, 10, 2):
                            args_table += arguments_template % (effectiveness_new_infra,
                                                                effectiveness_maintenance,
                                                                steps,
                                                                infrastructure_decay,
                                                                budget,
                                                                half_sensitivity_d,
                                                                half_sensitivity_ab,
                                                                rep, prefix)


with open("input_space.sub", 'w') as f:
    f.write(submitfile_template % args_table)
