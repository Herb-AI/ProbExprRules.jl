"""
    rand(::Type{RuleNode}, grammar::ProbabilisticGrammar, typ::Symbol, dmap::AbstractVector{Int}, 
        max_depth::Int=10)
Generates a random RuleNode of return type typ and maximum depth max_depth.
"""
function Base.rand(::Type{RuleNode}, pcfg::ProbabilisticGrammar, typ::Symbol,   max_depth::Int=10, 
    context::Union{GrammarContext, Nothing}=nothing, bin::Union{NodeRecycler,Nothing}=nothing)

    if isa(pcfg.grammar, ContextSensitiveGrammar) && context === nothing
        # first call, context not set yet
		init_node = RuleNode(0)
		context = GrammarContext(init_node)
    end

    grammar = pcfg.grammar
    rules = pcfg[typ]
    probs = probabilities(pcfg, typ)

    if isa(pcfg.grammar, ContextSensitiveGrammar)
        rules, probs = propagate_constraints_index(pcfg.grammar, context, rules, probs)
    end

    dmap = mindepth_map(pcfg)
    inds = findall(r->dmap[r] ≤ max_depth, rules)  
    rules, probs = rules[inds], probs[inds]
    rule_index = StatsBase.sample(rules, weights(probs))

    rulenode = iseval(pcfg, rule_index) ?
        RuleNode(bin, rule_index, Core.eval(Main, pcfg[rule_index].args[2])) :
        RuleNode(bin, rule_index)

    if !isterminal(pcfg, rule_index)
        for ch in child_types(pcfg, rule_index)
            if isa(pcfg.grammar, ContextSensitiveGrammar)
                #create context for the child expansion
			    new_context = GrammarContext(rulenode, deepcopy(context.nodeLocation))
			    push!(new_context.nodeLocation, length(rulenode.children) + 1)
            end
            #push!(rulenode.children, rand(RuleNode, pcfg, ch, dmap, max_depth-1, new_context))
            push!(rulenode.children, rand(RuleNode, pcfg, ch, max_depth-1, new_context, bin))
        end
    end
    return rulenode
end