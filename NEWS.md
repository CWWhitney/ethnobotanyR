# ethnobotanyR News

ethnobotanyR 0.1.6 is a patch:

Updated UVs() following Tardio and Pardo-de-Santayana (2008)

simple_UVs() now calculates a simple UVs cf. Albuquerque et al. (2006).

Fidelity level per species FLs() from Friedman et al. (1986) is now included for all uses (without the  max Ip "dplyr::slice(which.max(Ip))").

Now includes the CVe() from Reyes-Garcia et al. (2006).

The ethnobotanyChord() function is now replaced with ethnoChord(), which includes a 'by' parameter to allow for plotting simple circos plots of uses by species and informants.

The CIs() function is updated to reflect the simpler URs/N calculation following Tardio and Pardo-de-Santayana (2008).

The ethno_bayes_consensus() function to offer a Bayesian consensus (soft evidence) of the confidence we can have in the answers in the ethnobotany data.
