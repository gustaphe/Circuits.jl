using Circuits
using Unitful
using JuliaFormatter
using Test

@testset "Circuits.jl" begin
    r = Resistor(5.0u"Ω")
    c = Capacitor(2.0u"μF")
    l = Inductor(1.0u"mH")
    s = Short()
    o = Open()
    lcr = Series(l, c, r)

    @test Circuits.impedance(lcr, 5u"kHz") ≈ 110.0u"Ω"

    c = @circuit begin
        b:(0, 0) -->
        VSource(3) --> (1, 0) --> Resistor(4) --> Inductor(2) --> (1, 1) --> a:(0, 1)
        :a --> Capacitor(1)//Inductor(4) --> :b
    end
    @test c.coordinates == Dict(:a => (0, 1), :b => (0, 0), 2 => (1, 1), 1 => (1, 0))
end

@testset "Formatting" begin
    @test format(Circuits; overwrite=false)
end
