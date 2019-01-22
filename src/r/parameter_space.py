import numpy as np

time_simulation = 60

for effectivity_newInfra in np.linspace(0, 1, 5):
    for effectivity_mantenimiento in np.linspace(0, 1, 5):
        for decay_infra in np.linspace(0, 1, 5):
            for budget in np.linspace(24, 2429, 5):
                print "%.2f %.2f %.2f %s %.0f" % (effectivity_newInfra,
                                                  effectivity_mantenimiento,
                                                  decay_infra,
                                                  time_simulation,
                                                  budget)
