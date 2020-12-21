# TODO
# Improve typing (parametrize components' numeric types?)

module Circuits

using SymEngine, LightGraphs
#using Symata # For the future. It's currently broken?
import Base: +,//,div,IteratorEltype,IteratorSize,length,EltypeUnknown,HasLength,iterate
import Base: show
export ArbitraryComponent,Resistor,Capacitor,Inductor,Series,Parallel
export impedance,invadd, voltageDivision

abstract type AbstractComponent end
AbstractComponent(Z) = ArbitraryComponent(Z)

"""
Arbitrary component
"""
struct ArbitraryComponent <: AbstractComponent
    Z # Impedance (Laplace space)
end
impedance(x::ArbitraryComponent) = x.Z
function show(io::IO,mime::MIME"text/plain",x::ArbitraryComponent)
    print(io,"?(")
    show(io,mime,x.Z)
    print(io,")")
end

struct Resistor <: AbstractComponent
    R # Resistance
end
impedance(x::Resistor) = x.R
function show(io::IO,mime::MIME"text/plain",x::Resistor)
    print(io,"R(")
    show(io,mime,x.R)
    print(io," Ω)")
end

struct Capacitor <: AbstractComponent
    C # Capacitance
end
impedance(x::Capacitor) = inv(symbols(:s)*x.C)
function show(io::IO,mime::MIME"text/plain",x::Capacitor)
    print(io,"C(")
    show(io,mime,x.C)
    print(io," F)")
end

struct Inductor <: AbstractComponent
    L # Inductance
end
impedance(x::Inductor) = symbols(:s)*x.L
function show(io::IO,mime::MIME"text/plain",x::Inductor)
    print(io,"L(")
    show(io,mime,x.L)
    print(io," H)")
end

struct Series <: AbstractComponent
    l::Tuple#{<:AbstractComponent}
end
Series(x::Vararg{<:AbstractComponent}) = Series(x)
impedance(x::Series) = impedance(+(x...))
iterate(x::Series,state...) = iterate(x.l,state...)
IteratorEltype(::Series) = EltypeUnknown()
IteratorSize(::Series) = HasLength()
length(x::Series) = length(x.l)

function show(io::IO, mime::MIME"text/plain", x::Series)
    print(io,"(")
    (val,state) = iterate(x)
    show(io,mime,val)
    for val in Iterators.rest(x,state)
        print(io," ── ")
        show(io,mime,val)
    end
    print(io,")")
end


struct Parallel <: AbstractComponent
    l::Tuple#{<:AbstractComponent}
end
Parallel(x::Vararg{<:AbstractComponent}) = Parallel(x)
impedance(x::Parallel) = impedance(//(x...))
iterate(x::Parallel,state...) = iterate(x.l,state...)
IteratorEltype(::Parallel) = EltypeUnknown()
IteratorSize(::Parallel) = HasLength()
length(x::Parallel) = length(x.l)

function show(io::IO, mime::MIME"text/plain", x::Parallel)
    print(io,"(")
    (val,state) = iterate(x)
    show(io,mime,val)
    for val in Iterators.rest(x,state)
        print(io," ││ ")
        show(io,mime,val)
    end
    print(io,")")
end


abstract type AbstractSource <: AbstractComponent end
abstract type VoltageSource <: AbstractSource end
voltageDivision(::VoltageSource) = 0 # Voltage sources are replaced by short circuits
abstract type CurrentSource <: AbstractSource end

struct HeavisideVoltageSource
    V
end
voltage(x::HeavisideVoltagesource) = x.V
function show(io::IO, mime::MIME"text/plain", x::HeavisideVoltageSource)
    print(io,"(- ")
    show(io,mime,x.V)
    print(io," +)")
end





"""
```julia
+(a::AbstractComponent, b::AbstractComponent)
```
An equivalent component to the components in series. Use the `Series` type to
maintain the representation of each component without simplifying.
"""
+(x::Vararg{<:AbstractComponent}) = ArbitraryComponent(+(impedance.(x)...))
+(x::Vararg{Resistor}) = Resistor(+(getproperty.(x,:R)...))
+(x::Vararg{Capacitor}) = Capacitor(invadd(getproperty.(x,:C)...))
+(x::Vararg{Inductor}) = Inductor(+(getproperty.(x,:L)...))

"""
```julia
//(a::AbstractComponent, b::AbstractComponent)
```
An equivalent component to the components in parallel. Use the `Parallel` type
to maintain the representation of each component without simplifying.
"""
(//)(x::Vararg{AbstractComponent}) = ArbitraryComponent(invadd(impedance.(x)...))
(//)(x::Vararg{Resistor}) = Resistor(invadd(getproperty.(x,:R)...))
(//)(x::Vararg{Capacitor}) = Capacitor(+(getproperty.(x,:C)...))
(//)(x::Vararg{Inductor}) = Inductor(invadd(getproperty.(x,:L)...))

"""
```julia
invadd(a, b, ...)
```
the reciprocal sum of reciprocals (for parallel components):
```doctest
julia> invadd(1//1, 3//1, 5//1) # == 1//(1//1 + 1//2 + 1//3) == 6//11
6//11
```
"""
invadd(varargs...) = inv(+(inv.(varargs)...))

voltageDivision(::AbstractComponent) = 1
voltageDivision(p::Parallel) = Tuple( isa(x,Parallel)||isa(x,Series) ?
                                  (voltageDivision(x)) : 1 for x in p )
voltageDivision(s::Series) = Tuple( isa(x,Parallel)||isa(x,Series) ?
                                   (voltageDivision(x)).*impedance(x)./impedance(+(s...)) :
                              impedance(x)/impedance(+(s...)) for x in s )



end
