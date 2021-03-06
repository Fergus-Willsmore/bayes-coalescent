# bayes-coalescent
Make inference on population dynamics in the past based on current genetic diversity.

## Software
- Arlequin
- fastsimcoal2
- BEAUti
- BEAST
- Tracer
- R
- Python

## Approach
Use fastsimcoal2 to simulate DNA sequences of different population models (constant, exponential and bottleneck). 
Analyse .arp files in arlsumstat to produce summary statistics of the sample in a .txt file. 
Formulate data frame in R that consists of summary statistics of each of the population dynamics.
Use linear classifiers MLR, LDA and SVM for classification and analyse accuracy.

## Problems
Classifiers are bad at distinguishing between constant and bottleneck. Cause of error could be originating from .tpl file for the bottleneck model. Try to achieve severe bottlenecks in skyline plots in Tracer.
