"""
    iscomplete(grammar::Grammar, node::RuleNode)
    Returns true if the expression given by the node is complete expression, i.e., all leaves are terminal symbols
"""
function iscomplete(grammar::Grammar, node::RuleNode) 
	if isterminal(grammar, node)
		return true
	elseif isempty(node.children)
		# if not terminal but has children
		return false
	else
		return all([is_complete(grammar, c) for c in node.children])
	end
end