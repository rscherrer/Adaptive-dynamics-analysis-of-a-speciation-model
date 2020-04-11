(* ::Package:: *)

(* Export data to generate pairwise invasibility plots across values of s *)


Map[{
	data = PairwiseInvasibility[lambda[y,x], dmg, init, {a->400, b->100, h->1, s->1.5, m->0.01, d->0.2, \[Alpha]->#}, {-1.1, 1.1, 0.01}] // Quiet;
	data = Join[{{"x", "y", "lambda"}}, data];
	filename = StringJoin["pairwise_invasibility_plot_alpha_", ToString[#], ".csv"];
	Export[filename, data, "CSV"];
}&, {0, 1, 10, 100}];
