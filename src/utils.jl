
has_children(rule::RuleNode) = !isempty(rule.children)

"""
	Checks if the expression is complete (= all leafs are terminals)
"""
function is_complete(grammar::Grammar, node::RuleNode) 
	if isterminal(grammar, node)
		return true
	elseif isempty(node.children)
		# if not terminal but has children
		return false
	else
		return all([is_complete(grammar, c) for c in node.children])
	end
end