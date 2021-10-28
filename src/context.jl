
mutable struct ProbabilisticContext
	expr::RuleNode
	node_loc::Vector{Int}
	prob::Float64
end

ProbabilisticContext(node::RuleNode) = ProbabilisticContext(node, Vector{Int}(), 1.0)

ProbabilisticContext(node::RuleNode, prob::Float64) = ProbabilisticContext(node, Vector{Int}(), prob)