

function propagate_constraints_index(grammar::ContextSensitiveGrammar, context::GrammarContext, rules::Vector{Int}, probs::Vector{Float64})
	domain_internal = deepcopy(rules)
	probs_internal = deepcopy(rules)

	for propagator in grammar.constraints 
		indices = ExprRules.propagate_index(propagator, context, domain_internal)
		domain_internal = domain_internal[indices]
		probs_internal = probs_internal[indices]
	end

	return domain_internal, probs_internal
end