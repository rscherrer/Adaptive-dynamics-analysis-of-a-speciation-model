(* ::Package:: *)

(* This script searches for singular strategies throughout parameter space and analyzes them, within each patch assuming no dispersal *)


(* Values of the parameters to explore -- change here*)
avalues = {400};
bvalues = {100};
hvalues = Range[0, 1, 0.5];
svalues = Range[0.01, 2.5, 0.5];
dvalues = {0.2};
\[Alpha]values = {0, 1, 10, 100};

(* Header of the table -- change here *)
header = {{"a", "b", "h", "s", "d", "alpha", "x", "conv", "inv", "n"}};


(* Function to map variables to values into rules *)
MapRules[variables_, values_] := MapThread[#1->#2&, {variables, values}]


(* Define combinations of parameters *)
combinations = Tuples[{avalues, bvalues, hvalues, svalues, dvalues, \[Alpha]values}];
combiRules = Map[MapRules[{a, b, h, s, d, \[Alpha]}, #]&, combinations];


(* Patch 1 *)


(* Loop over combinations and search for singular strategies *)
results = Reap[Map[Sow[EvaluateSingularStrategies[Subscript[dW, 1][x], Subscript[ddW, 1][x] - \[Alpha] Subscript[W, 1][x, x], Subscript[N, 1] == Subscript[r, 1][x, x] Subscript[N, 1], {Subscript[N, 1], 1000}, #, {-1, 1, 0.1}]]&, combiRules][[[2]][[1]];

(* Assemble parameter values and results into a table *)
nstrategies = Map[Length, results]; (* How many singular strategies found for each parameter combination? *)
combinations1 = MapThread[ConstantArray[#1, #2]&, {combinations, nstrategies}]; (* repeat each combination as many times as there are singular strategies *)
data = MapThread[Map[Flatten, {#1, #2} // Transpose]&, {combinations1, results}] // Catenate;
data = Join[header, data];
(* Export["adaptive_dynamics_patch1.csv", data, "CSV"] *) (* uncomment to save the data to file *)


(* Patch 2 *)


(* Loop over combinations and search for singular strategies *)
results = Reap[Map[Sow[EvaluateSingularStrategies[Subscript[dW, 2][x], Subscript[ddW, 2][x] - \[Alpha] Subscript[W, 2][x, x], Subscript[N, 2] == Subscript[r, 2][x, x] Subscript[N, 2], {Subscript[N, 2], 1000}, #, {-1, 1, 0.1}]]&, combiRules][[[2]][[1]];

(* Assemble parameter values and results into a table *)
nstrategies = Map[Length, results]; (* How many singular strategies found for each parameter combination? *)
combinations2 = MapThread[ConstantArray[#1, #2]&, {combinations, nstrategies}]; (* repeat each combination as many times as there are singular strategies *)
data = MapThread[Map[Flatten, {#1, #2} // Transpose]&, {combinations2, results}] // Catenate;
data = Join[header, data];
(* Export["adaptive_dynamics_patch2.csv", data, "CSV"] *) (* uncomment to save the data to file *)
