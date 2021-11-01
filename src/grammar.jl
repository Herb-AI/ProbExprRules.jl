
const ProbType = Dict{Symbol,Vector{Float64}}

struct ProbabilisticGrammar <: ExprRules.Grammar
    grammar::ExprRules.Grammar
    probs::ProbType
    rule_to_prob::Dict{Int,Float64}
end

"""
	Creates a probabilistic grammar from the given grammar, with probabilities being uniform
"""
function ProbabilisticGrammar(grammar::Grammar) 
	probs = ProbType() 
	prob_dict = Dict{Int, Float64}()
	for nt in ExprRules.nonterminals(grammar)
	    n = length(grammar[nt])
	    probs[nt] = ones(Float64, n) / n 

	    for rule in grammar[nt]
		prob_dict[rule] = 1/n
	    end
	end

	ProbabilisticGrammar(grammar, probs, prob_dict)
end

function ProbabilisticGrammar(grammar::Grammar, prob_vector::Vector{Float64})
	probs = ProbType()
	for nt in ExprRules.nonterminals(grammar)
		probs[nt] = prob_vector[grammar[nt]]
	end

	prob_dict = Dict{Int, Float64}()
	for (rule, prob) in enumerate(prob_vector)
		prob_dict[rule] = prob
	end
	ProbabilisticGrammar(grammar, probs, prob_dict)
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
	return pg.rule_to_prob[rule_index]
end

function getprob(pg::ProbabilisticGrammar, rule::RuleNode)
	if has_children(rule)
		return getprob(pg, rule.ind) * reduce((acc, x) -> acc * x, [getprob(pg, c) for c in rule.children]; init=1.0)
	else
		return getprob(pg, rule.ind)
	end
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
end


"""
	Returns true if the probabilistic grammar wraps a context sensitive expr grammar
"""
is_contextsensitive(pg::ProbabilisticGrammar) = isa(pg.grammar, ContextSensitiveGrammar)


"""
	Returns true if the probabilistic gramar wraps a context free expr grammar
"""
is_contextfree(pg::ProbabilisticGrammar) = isa(pg.grammar, ContextFreeGrammar)


function Base.show(io::IO, pg::ProbabilisticGrammar)
	for i in eachindex(pg.grammar.rules)
		println(io, i, " ", getprob(pg, i), " : ", pg.grammar.types[i], " = ", pg.grammar.rules[i])
	end
end








