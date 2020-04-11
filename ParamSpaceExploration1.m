(* ::Package:: *)

(* This script searches for singular strategies throughout parameter space and analyzes them *)


(* Values of the parameters to explore -- change here *)
avalues = {400};
bvalues = {100};
hvalues = Range[0, 1, 0.5];
svalues = Range[0.01, 2.5, 0.5];
dvalues = {0.2};
mvalues = {0.001, 0.01, 0.1}; (* we ran the three values separately because the output was taking up too much memory *)
\[Alpha]values = {0, 1, 10, 100};

(* Header of the table -- change here *)
header = {{"a", "b", "h", "s", "d", "m", "alpha", "x", "conv", "inv", "n1", "n2"}};


(* Function to map variables to values into rules *)
MapRules[variables_, values_] := MapThread[#1->#2&, {variables, values}]


(* Define combinations of parameters *)
combinations = Tuples[{avalues, bvalues, hvalues, svalues, dvalues, mvalues, \[Alpha]values}];
combiRules = Map[MapRules[{a, b, h, s, d, m, \[Alpha]}, #]&, combinations];


(* Loop over combinations and search for singular strategies *)
results = Reap[Map[Sow[EvaluateSingularStrategies[gradient[x], curvature[x], dmg, init, #, {-1, 1, 0.1}]]&, combiRules][[[2]][[1]];

(* Assemble parameter values and results into a table *)
nstrategies = Map[Length, results]; (* How many singular strategies found for each parameter combination? *)
combinations = MapThread[ConstantArray[#1, #2]&, {combinations, nstrategies}]; (* repeat each combination as many times as there are singular strategies *)
data = MapThread[Map[Flatten, {#1, #2} // Transpose]&, {combinations, results}] // Catenate;
data = Join[header, data];
(* Export["adaptive_dynamics.csv", data, "CSV"] *) (* uncomment to save the data to file *)
