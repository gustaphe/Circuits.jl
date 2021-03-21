using Circuits
using JuliaFormatter
using Test

@testset "Circuits.jl" begin
    # No tests yet... TODO
end

@testset "Formatting" begin
    @test format(Circuits; overwrite=false)
end
