# cloud-sim
Cloud Autoscaling Simulation based on Queueing Network Model

This is a script that simulates a cloud autoscaler using a loop over the PDQ library. Overload detection and handling, user satisfaction computation, result evaluation and graphing, and other functions as well as general philosophy and some mathematics can be found in the accompanying article of the same name. Please cite ... (not published yet) when using it in academic research.

How to use:

Install R. Tested with R version 2.15.1 (2012-06-22) under Rstudio Version 0.98.1091

Required packages graphics, grDevices, methods, stats, utils from R core;

queueing 0.2.4 by Pedro Canadilla from CRAN archive

PDQ 6.1-1 by Neil Gunther from http://www.perfdynamics.com/Tools/PDQ.html


Optionally put the supplied .RData file in your working directory. It contains the data used for evaluation in the article.

It also will help in case I've forgotten to do some initializations. This is a prototype, after all.


Read through the script, change the parameters as required in the top part, choose an autoscaling function and a plotting function (the blocks prefixed with "#Call one:" comments).


Source in R.

Examine results. Change parameters. Repeat.

When you understand the behavior of the autoscalers on your data, enable parameter sweep mode and find the best parameters.

If there are no best parameters, write your own autoscaling function.
