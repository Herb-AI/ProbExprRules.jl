
"""
	Prepares the new context for the grammar
		-> if context sensitive, adds child node index to context
		-> if context free, returns `nothing`
"""
function _prepare_context(grammar::Grammar, context::Union{Nothing,GrammarContext}, child_index::Int=0)
	if is_contextsensitive(grammar)
		# prepare new context
		new_context = GrammarContext(deepcopy(context.originalExpr), deepcopy(context.nodeLocation))
		
		# add step to the node location if needed (> 0)
		if child_index > 0
			push!(new_context.nodeLocation, child_index)
		end

		return new_context
	else
		return nothing
	end
end


"""
	Returns suitable rules of type `type`
		-> if context free grammar, returns all rules of type `type`
		-> if context sensitive, filters the rules to satisfy constraints and sorts them according to their probability (decreasing)
"""
function _prepare_child_rules(grammar::Grammar, rule_type::Symbol, context::Union{Nothing,GrammarContext})
	child_rules = [x for x in grammar[rule_type]]  # select all applicable rules; needs to be copied so that it can be filtered later and not modify the grammar
	probs = probabilities(grammar, rule_type)

	if is_contextsensitive(grammar)
		# filter child rules and probabilities
		child_rules, probs = propagate_constraints_index(grammar, context, child_rules, probs)  # filter out those that violate constraints
	end

	# sort the rules according to their probability
	child_rules = sort(collect(zip(child_rules, probs)), by = x -> x[2], rev=true)
	child_rules = map(x -> x[1], child_rules)

	return (child_rules, sort(probs, rev=true))	
end


"""
	Expend a partial expression by expanding every non-terminal for exactly one derivation
		 -> only returns the probability of the added parts; the correct probability of the entire expression should be handled externally

"""
function _expand_partial(node::RuleNode, grammar::ProbabilisticGrammar, max_depth::Int, context::Union{Nothing, GrammarContext} )
	expansion_pool = PriorityQueue{RuleNode, Float64}(Base.Order.Reverse)
	expansion_pool[node] = 1.0

	if max_depth <= 1
		# if we are out of depth, return empty
		return PriorityQueue{RuleNode,Float64}()
	elseif isterminal(grammar, node)
		# if the node is terminal, just return the node
		pq = PriorityQueue{RuleNode,Float64}(Base.Order.Reverse)
		# pq[node] = 1.0
		return pq
	elseif isempty(node.children)
		# if the node is non-terminal and has no children 
		# 	-> construct a RuleNode for each possible extension
		for (child_index, c) in enumerate(child_types(grammar, node))
			# new dict to hold further expanded candidates
			new_pool = PriorityQueue{RuleNode, Float64}(Base.Order.Reverse)
			
			# go over every partial expansion (containing expansions to the children on the left) in the expansion_pool
			for (pe, prob) in expansion_pool
				# prepare context
				#	-> replace the node indicated by the location with pe
				#	-> do that only if it is necessary, that is, if `new_context` is not `nothing`
				new_context = _prepare_context(grammar, context)

				if new_context !== nothing
					# do this only if using context-sensitive grammar
					swap_node(new_context.originalExpr, pe, new_context.nodeLocation)

					# add node location to the `nodeLocation`; needed to propagate constraints
					push!(new_context.nodeLocation, child_index)
				end
				
				child_rules, probs = _prepare_child_rules(grammar, c, new_context)

				# construct all possible expansions of the pe (adding a child)
				for (ch, pr) in zip(child_rules, probs)
					new_node = deepcopy(pe)
					push!(new_node.children, RuleNode(ch))

					# add new node to the pool and adjust the probability (prob of new child)
					new_pool[new_node] = prob * pr  
				end
			end

			# newly constructed candidates become new expansion pool
			expansion_pool = new_pool
		end
	else
		# if the node is non-terminal and has children
		# 	-> recursively get all extensions of each child and combine them
		for (child_index, c) in enumerate(child_types(grammar, node))
			# new dict to hold further expansions
			new_pool = PriorityQueue{RuleNode, Float64}()

			# go over all existing expansions in expansion_pool (containing expansions of the childred to the left)
			for (elem, prob) in expansion_pool
				# prepare context
				#   -> replace the node indicated by the location with pe 
				#   -> do this only if necessary, that is, if `new_context` is not `nothing`
				new_context = _prepare_context(grammar, context)

				if new_context !== nothing
					swap_node(new_context.originalExpr, pe, new_context.nodeLocation)

					# add node location to the `node location`; needed to propagate constraints 
					push!(new_context.nodeLocation, child_index)
				end

				#build up child expansions
				child_expansions = _expand_partial(node.children[child_index], grammar, max_depth-1, new_context)

				# replace with new child
				for (ch, ch_prob) in child_expansions
					new_node = deepcopy(elem)
					new_node.children[child_index] = ch

					# add new node to the pool and adjust the probability 
					new_pool[new_node] = prob * ch_prob
				end
			end

			expansion_pool = new_pool
		end
	end

	return expansion_pool

end

function _complete_partial_expr!(node::RuleNode, grammar::ProbabilisticGrammar, max_depth::Int, init_prob::Float64, context::Union{Nothing, GrammarContext})
	prob_to_return = init_prob
	subexpr_prob = 0.0
	if max_depth ≤ 1
		return (node, init_prob, false) # cannot expand
	end

	child_index = 1  # keep track of which child we are processing now (needed for context)

	# build out the node
	for c in child_types(grammar, node)
		worked = false
		i = 0
		child = RuleNode(0)

		child_rules = [x for x in grammar[c]]  # select all applicable rules
		probs = probabilities(grammar, c)

		if is_contextsensitive(grammar)
			# prepare new context
			new_context = GrammarContext(context.originalExpr, deepcopy(context.nodeLocation))
			push!(new_context.nodeLocation, child_index)

			child_rules, probs = propagate_constraints_index(grammar, new_context, child_rules, probs)  # filter out those that violate constraints
			
			# sort the rules according to their probability
			child_rules = sort(zip(child_rules, probs), by = x -> x[2], rev=true)
			child_rules = map(x -> x[1], child_rules)
		else
			new_context = nothing
		end
		

		while !worked && i < length(child_rules)
			i += 1
			child = RuleNode(child_rules[i])

			# probability of the child 
			subexpr_prob = getprob(grammar, child)

			if iseval(grammar, child.ind) # if rule needs to be evaluated (_())
				child._val = eval(grammar.rules[child.ind].args[2])
			end

			worked = true
			if !isterminal(grammar, child)
				child, subexpr_prob, worked = _next_state!(child, grammar, max_depth-1, subexpr_prob, new_context)
			end
		end
		if !worked
			return (node, prob_to_return, false) # did not work
		end
		push!(node.children, child)
		prob_to_return *= subexpr_prob

		child_index += 1
	end

	return (node, prob_to_return, true)
end


function _modify_full_expr!(node::RuleNode, grammar::ProbabilisticGrammar, max_depth::Int, init_prob::Float64, context::Union{Nothing, GrammarContext})
	prob_to_return = init_prob
	# make one change, starting with rightmost child
	worked = false
	child_index = length(node.children) + 1

	while !worked && child_index > 1
		child_index -= 1
		child = node.children[child_index]	

		if is_contextsensitive(grammar)
			new_context = GrammarContext(context.originalExpr, deepcopy(context.nodeLocation))
			push!(new_context.nodeLocation, child_index)
		else
			new_context = nothing
		end
		

		# this modifies the node if successfull
		child, child_prob, child_worked = _next_state!(child, grammar, max_depth-1, init_prob, new_context)
		if child_worked
			# update the probability of the entire expression
			# if the child worked, the returned probability is the probability of the entire new expression
			prob_to_return = child_prob
		end

		while !child_worked
			child_type = return_type(grammar, child)

			child_rules = [x for x in grammar[child_type]]  # get all applicable rules
			probs = probabilities(grammar, child_type)
			
			if is_contextsensitive(grammar)
				child_rules, probs = propagate_constraints_index(grammar, new_context, child_rules, probs)  # filter ones that violate constraints
				# sort the rules according to their probability
				child_rules = sort(zip(child_rules, probs), by = x -> x[2], rev=true)
				child_rules = map(x -> x[1], child_rules)
			end
			
			i = something(findfirst(isequal(child.ind), child_rules), 0)
			if i < length(child_rules)
				child_worked = true
				child = RuleNode(child_rules[i+1])

				# probability of the child
				child_prob = getprob(grammar, child.ind)

				# node needs to be evaluated
				if iseval(grammar, child.ind)
					child._val = eval(grammar.rules[child.ind].args[2])
				end

				if !isterminal(grammar, child)
					child, child_prob, child_worked = _next_state!(child, grammar, max_depth-1, child_prob, new_context)
				end

				# correct probability
				curr_child_prob = getprob(grammar, node.children[child_index])
				prob_to_return *= 1.0/curr_child_prob

				# update the child and the probability
				node.children[child_index] = child
				prob_to_return *= child_prob
			else
				break
			end
		end

		if child_worked
			worked = true

			# reset remaining children
			for child_index2 in child_index+1 : length(node.children)
				c = child_types(grammar, node)[child_index2]
				worked = false
				i = 0
				child = RuleNode(0)

				child_rules = [x for x in grammar[c]]  # take all applicable rules
				probs = probabilities(grammar, c)

				if is_contextsensitive(grammar)
					# prepare new context 
					new_context = GrammarContext(context.originalExpr, deepcopy(context.nodeLocation))
					push!(new_context.nodeLocation, child_index2)
					
					# filter violating children
					child_rules, probs = propagate_constraints_index(grammar, new_context, child_rules, probs)  # reomove ones that violate constraints
					
					# sort the rules 
					child_rules = sort(zip(child_rules, probs), by = x -> x[2], rev=true)
					child_rules = map(x -> x[1], child_rules)
				else
					new_context = nothing
				end
				

				while !worked && i < length(child_rules)
					i += 1
					child = RuleNode(child_rules[i])

					# probability of this child
					inner_child_prob = getprob(grammar, child)

					if iseval(grammar, child.ind)
						child._val = eval(grammar.rules[child.ind].args[2])
					end

					worked = true
					if !isterminal(grammar, child)
						# we can just pass the prob 1.0 and later multiply it because the the child is non-terminal 
						child, inner_child_prob, worked = _next_state!(child, grammar, max_depth-1, 1.0, new_context)
					end
				end

				if !worked
					break
				end

				#discount the original child 
				prob_to_return *= 1.0/getprob(grammar, node.childred[child_index2])

				# update the child and the overall prob
				node.children[child_index2] = child
				prob_to_return *= inner_child_prob
			end
		end
	end

	return (node, prob_to_return, worked)
end

"""
reimplementation of the ExprRules._next_state!
Change: child expressions are filtered so that the constraints are not violated
"""
function _next_state!(node::RuleNode, grammar::ProbabilisticGrammar, max_depth::Int, init_prob::Float64, context::Union{Nothing,GrammarContext})
	#prob_to_return = 1.0

	if max_depth < 1
		return (node, init_prob, false) # did not work
	elseif isterminal(grammar, node)
		# do nothing
		if iseval(grammar, node.ind) && (node._val === nothing)  # evaluate the rule
			node._val = eval(grammar.rules[node.ind].args[2])
		end
		return (node, init_prob, false) # cannot change leaves
	else # !isterminal
		# if node is not terminal and doesn't have children, expand every child
		if isempty(node.children)  
			_complete_partial_expr!(node, grammar, max_depth, init_prob, context)
			
		else # not empty
			_modify_full_expr!(node, grammar, max_depth, init_prob, context)
		end
	end
end


mutable struct ProbabilisticIterator <: ExpressionIterator
	grammar::ProbabilisticGrammar
	max_depth::Int
	sym::Symbol
end


Base.IteratorSize(::ProbabilisticIterator) = Base.SizeUnknown()

Base.eltype(::ProbabilisticIterator) = (RuleNode, Float64)


function Base.iterate(iter::ProbabilisticIterator)
	grammar, sym, max_depth = iter.grammar, iter.sym, iter.max_depth
    
	# propagate constraints on the root node 
	sym_rules = [x for x in grammar[sym]]
	probs = probabilities(grammar, sym)

	if isa(iter.grammar.grammar, ContextSensitiveGrammar)
		init_node = RuleNode(0)  # needed for propagating constraints on the root node 
		init_context = GrammarContext(init_node)

		sym_rules, probs = propagate_constraints_index(grammar, init_context, sym_rules, probs)

		# sort 
		sym_rules = sort(zip(sym_rules, probs), by = x -> x[2], rev=true)
		sym_rules = map(x -> x[1], sym_rules)
	else
		init_context = nothing
	end

	# form the queue of candidates
	pq = PriorityQueue{RuleNode, Float64}(Base.Order.Reverse)
	for r in sym_rules
		pq[RuleNode(r)] = getprob(grammar, r)
	end

	return iterate(iter, pq)

end



# function Base.iterate(iter::ProbabilisticIterator)
    
# 	grammar, sym, max_depth = iter.grammar, iter.sym, iter.max_depth
    
# 	# propagate constraints on the root node 
# 	sym_rules = [x for x in grammar[sym]]
# 	probs = probabilities(grammar, sym)

# 	if isa(iter.grammar.grammar, ContextSensitiveGrammar)
# 		init_node = RuleNode(0)  # needed for propagating constraints on the root node 
# 		init_context = GrammarContext(init_node)

# 		sym_rules, probs = propagate_constraints_index(grammar, init_context, sym_rules, probs)

# 		# sort 
# 		sym_rules = sort(zip(sym_rules, probs), by = x -> x[2], rev=true)
# 		sym_rules = map(x -> x[1], sym_rules)
# 	else
# 		init_context = nothing
# 	end
# 	#node = RuleNode(grammar[sym][1])
# 	node = RuleNode(sym_rules[1])
# 	prob = getprob(grammar, sym_rules[1])
     
# 	if isterminal(grammar, node)
# 	    return ((deepcopy(node), prob), (node, prob))
# 	else
	
# 	    if is_contextsensitive(grammar)
# 	    	context = GrammarContext(node)
# 	    else
# 		context = nothing
# 	    end

# 	    node, prob, worked =  ProbExprRules._next_state!(node, grammar, max_depth, prob, context)

# 	    while !worked
# 		# increment root's rule
# 		rules = [x for x in grammar[sym]]
# 		probs = probabilities(grammar, sym)

# 		if is_contextsensitive(grammar)
# 			rules, probs = propagate_constraints_index(grammar, init_context, rules, probs) # propagate constraints on the root node
			
# 			# sort 
# 			rules = sort(zip(rules, probs), by = x -> x[2], rev=true)
# 			rules = map(x -> x[1], rules)
# 		end

# 		i = something(findfirst(isequal(node.ind), rules), 0)
# 		if i < length(rules)
# 		    node, worked = RuleNode(rules[i+1]), true
# 		    prob = getprob(grammar, rules[i+1])
# 		    if !isterminal(grammar, node)
# 			node, prob, worked = _next_state!(node, grammar, max_depth, prob, context)
# 		    end
# 		else
# 		    break
# 		end
# 	    end
# 	    return worked ? ((deepcopy(node), prob), (node, prob)) : nothing
# 	end
# end

function Base.iterate(iter::ProbabilisticIterator, state::PriorityQueue{RuleNode,Float64})
	grammar, max_depth = iter.grammar, iter.max_depth
	if isempty(state)
		return nothing
	else
		expr, prob = dequeue_pair!(state)

		# keep dequeueing until the returned expr is terminal
		while !iscomplete(grammar, expr) && !isempty(state)
			context = GrammarContext(expr)
			expansions = _expand_partial(expr, grammar, max_depth, context)

			# add expansions to state 
			for (node, pr) in expansions
				state[node] = pr * prob
			end

			# # check if the state is empty
			# if isempty(state)
			# 	return nothing
			# end

			# pop the priority node again
			expr, prob = dequeue_pair!(state)
		end

		return ((expr, prob), state)
	end
end

# function Base.iterate(iter::ProbabilisticIterator, state_tuple::Tuple{RuleNode,Float64})
# 	grammar, max_depth = iter.grammar, iter.max_depth
# 	state, init_prob = state_tuple

# 	if isa(iter.grammar.grammar, ContextSensitiveGrammar)
# 		context = GrammarContext(state)
# 	else
# 		context = nothing
# 	end

# 	node, prob, worked = _next_state!(state, grammar, max_depth, init_prob, context)
    
# 	while !worked
# 	    # increment root's rule
# 	    rules = [x for x in grammar[iter.sym]]
# 	    probs = probabilities(grammar, iter.sym)

# 	    if isa(iter.grammar.grammar, ContextSensitiveGrammar)
# 	    	init_node = RuleNode(0)  # needed for propagating constraints on the root node 
#     	    	init_context = GrammarContext(init_node)

# 	    	rules, probs = propagate_contraints_index(grammar, init_context, rules, probs)
		    
# 		# sort 
# 		rules = sort(zip(rules, probs), by = x -> x[2], rev=true)
# 		rules = map(x -> x[1], rules)
# 	    end

# 	    i = something(findfirst(isequal(node.ind), rules), 0)
# 	    if i < length(rules)
# 		node, worked = RuleNode(rules[i+1]), true
# 		prob = getprob(grammar, rules[i+1])
# 		if !isterminal(grammar, node)
# 		    context = GrammarContext(node)
# 		    node, prob, worked = _next_state!(node, grammar, max_depth, prob, context)
# 		end
# 	    else
# 		break
# 	    end
# 	end
# 	return worked ? ((deepcopy(node), prob), (node, prob)) : nothing
# end