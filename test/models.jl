@testset "Chain grammar" begin
	grammar = @cfgrammar begin
		S = a
		S = b
		S = a + T
		T = a
		T = b 
		T = a + P 
		P = a 
		P = b  
	end
	
	probs = Vector{Float64}([0.8, 0.1, 0.1, 0.8, 0.15, 0.15, 0.5, 0.5])
	
	pg = ProbabilisticGrammar(grammar, probs)
	
	iter = ProbExprRules.ProbabilisticIterator(pg, 4, :S)

	correct_probs_in_order = [0.8, 0.1, 0.08, 0.015, 0.0075, 0.0075]
	
	for (ind, item) in enumerate(iter)
		@test round(item[2], digits=5) == correct_probs_in_order[ind] 
	end
end

@testset "Chain grammar with level skipping" begin
	grammar = @cfgrammar begin
		S = a
		S = b
		S = a + T
		T = a
		T = b 
		T = a + P 
		P = a 
		P = b  
	end
	
	probs = Vector{Float64}([0.2, 0.2, 0.6, 0.2, 0.2, 0.6, 0.5, 0.5])
	
	pg = ProbabilisticGrammar(grammar, probs)
	
	iter = ProbExprRules.ProbabilisticIterator(pg, 4, :S)

	correct_probs_in_order = [0.2, 0.2, 0.18, 0.18, 0.12, 0.12]
	
	for (ind, item) in enumerate(iter)
		@test round(item[2], digits=5) == correct_probs_in_order[ind] 
	end
end