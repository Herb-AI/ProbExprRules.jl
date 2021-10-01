module ProbExprRules

using ExprRules
using LinearAlgebra
using StatsBase

include("grammar.jl")
include("propagators.jl")
include("sampling.jl")

export	RuleNode,
	@cfgrammar,
	@csgrammar,

	Constraint,
        ValidatorConstraint,
        PropagatorConstraint,
        ComesAfter,
        Ordered,
        Forbidden,

	ProbabilisticGrammar,
	probabilities,
	getprob,

	nonterminals,
	return_type,
	iseval,
	max_arity,
	child_types,
	isterminal,
	nchildren,
	contains_returntype,
	mindepth_map,

	uniform!


end # module
