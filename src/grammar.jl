
const ProbType = Dict{Symbol,Vector{Float64}}

struct ProbabilisticGrammar <: Grammar
    grammar::Grammar
    probs::ProbType
end

"""
	Creates a probabilistic grammar from the given grammar, with probabilities being uniform
"""
function ProbabilisticGrammar(grammar::Grammar) 
	probs = ProbType() 
	for nt in ExprRules.nonterminals(grammar)
	    n = length(grammar[nt])
	    probs[nt] = ones(Float64, n) / n 
	end
	ProbabilisticGrammar(grammar, probs)
end

probabilities(pcfg::ProbabilisticGrammar, typ::Symbol) = pcfg.probs[typ]

"""
	Basic functions for getting infomation about the rules
"""

Base.getindex(g::ProbabilisticGrammar, s::Symbol) = g.grammar.bytype[s]

Base.getindex(g::ProbabilisticGrammar, index::Int) = g.grammar.rules[index]


for func in [:nonterminals, :iseval, :max_arity, :mindepth_map]
	eval(:($func(pg::ProbabilisticGrammar) = ExprRules.$func(pg.grammar)))
end

for func in [:return_type, :child_types, :isterminal, :iseval, :nchildren]
	eval(:($func(pg::ProbabilisticGrammar, rule_index::Int) = ExprRules.$func(pg.grammar, rule_index)))
end

for func in [:return_type, :child_types, :isterminal, :nchildren]
	eval(:($func(pg::ProbabilisticGrammar, node::RuleNode) = ExprRules.$func(pg.grammar, node)))
end

for func in [:contains_returntype]
	eval(:($func(node::RuleNode, pg::ProbabilisticGrammar, sym::Symbol, maxdepth::Int=typemax(Int)) = ExprRules.$func(node, pg.grammar, sym, maxdepth)))
end


"""
	Returns the probability of the rule specified by the index
"""
function getprob(pg::ProbabilisticGrammar, rule_index::Int)
	rule_type = return_type(pg, rule_index)
	probs_of_type = probabilities(pg, rule_type)
	index = indexin(rule_index, pg.grammar[rule_type])
	return probs_of_type[index]
end


"""
    uniform!(pcfg::ProbabilisticGrammar)
Set all probability vectors to uniform distribution
"""
function uniform!(pcfg::ProbabilisticGrammar)
    	fill!(pcfg, 0.0)
    	normalize!(pcfg)
end


"""
	Adds constraint to the probabilistic grammar, if defined over ContextSensitiveGrammar as a base
"""
function addconstraint!(g::ProbabilisticGrammar, c::Constraint)
	if isa(g.grammar, ContextSensitiveGrammar)
		addconstraint!(g.grammar, c)
		g
	else
		error("Constraints can be addeed only to ContextSensitiveGrammar (not $(typeof(g.grammar)))")
end


function Base.show(io::IO, pg::ProbabilisticGrammar)
	for i in eachindex(pg.grammar.rules)
		println(io, i, " ", getprob(pg, i), " : ", pg.grammar.types[i], " = ", pg.grammar.rules[i])
	end
end








