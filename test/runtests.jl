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
end

@testset "Formatting" begin
    @test format(Circuits; overwrite=false)
end
