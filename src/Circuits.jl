module Circuits
using Latexify

export Circuit, @circuit
export Resistor, Inductor, Capacitor, Short, Open
export Series, Parallel
export Battery, VSource, SinusSource

export voltageDivision, currentDivision

import Base.show, Base.iterate, Base.length

CircuitIndex = Union{Int64,Symbol}

abstract type AbstractComponent end
abstract type Impedor <: AbstractComponent end
abstract type Source <: AbstractComponent end
abstract type VoltageSource <: Source end
abstract type CurrentSource <: Source end

# Circuit {{{
struct Circuit # Could be considered a graph?
    coordinates::Dict#{CircuitIndex,Tuple{Float64,Float64}}
    connections::Dict#{Tuple{CircuitIndex,CircuitIndex},<:AbstractComponent}
end


function show(io::IO, m::MIME"text/plain", x::Circuit)
    print(io,"Circuit with ",
          length(x.coordinates)," nodes, ",
          count(isa.(values(x.connections), Impedor))," impedors and ",
          count(isa.(values(x.connections), Source))," sources.",
         )
end

function show(io::IO, m::MIME"text/circuitikz", x::Circuit; shownodes=false)
    print(io,"\\draw %\n")
    for (k,v) in x.connections
        print(io,"\t(",join(x.coordinates[k[1]],','),") to[")
        show(io, m, v)
        print(io,"] (",join(x.coordinates[k[2]],','),") %\n")
    end
    print(io,"\t;")
    if shownodes
        print(io, "\\path[red, every node/.style={circle,draw=red,fill=white}]")
        for (i,c) in x.coordinates
            print(io, " (",join(c,','),") node {",i,"}")
        end
        print(io, ";\n")
    end
end
# }}}

# Circuit macro {{{
"""
```
<coordinate> <component> <coordinate> <component>...
```

Label coordinates by `name:(x,y)` to reuse later with `:name`.

```julia
@circuit begin
a:(0,0) --> R(10) --> b:(1,0) --> C(4) --> (2,0)
:b --> R(10) --> c:(1,1)
end
```
"""
macro circuit(args...)
    esc(generate_circuit(args[1]))
end


function generate_circuit(arg::Expr)
    if arg.head !== :block
        expressions = (arg,)
    else
        expressions = arg.args
    end
    coordinates = Dict{CircuitIndex,Union{Expr,Symbol}}()
    components = Expr[]

    for e in expressions
        push_stuff!(e, coordinates, components)
    end
    return Expr(:call,
                :Circuit,
                Expr(:call, :Dict,
                     Expr.(:call, :(=>),
                           QuoteNode.(keys(coordinates)),values(coordinates)
                          )...
                    ),
                Expr(:call, :Dict,
                     components...
                    ),
               )
end

"""
`push_stuff!(e, coord, comp)`

recursively add the contents of `e` to the coordinate list `coord` and/or the
component list `comp`. Returns the index if `e` represents a coordinate, a
`(component, index)` tuple if `e` represents a component, connected to
coordinate `index` to the right, or the index of the leftmost coordinate of a
chain starting and ending with a coordinate.
"""
function push_stuff! end

# This recursion is disgusting in every way. Luckily they will never grow very
# large (knock on wood)
function push_stuff!(e::Expr, coord, comp)
    (head, args) = (e.head, e.args)
    if head === :-->
        left = push_stuff!(args[1], coord, comp)
        right = push_stuff!(args[2], coord, comp)
        if isa(left,CircuitIndex) && isa(right,CircuitIndex)
            # coordinate --> coordinate
            push!(comp,:(($(QuoteNode(left)), $(QuoteNode(right))) => Short()))
            return left
        end
        if left isa CircuitIndex
            # coordinate --> component
            push!(comp,:(($(QuoteNode(left)), $(QuoteNode(right.index))) => $(right.component)))
            return left
        end
        if right isa CircuitIndex
            # component --> coordinate
            # should be a (component, index) tuple...
            return (component=left,index=right)
        end
        # component --> component
        return (component=:(Series($left,$(right.component))), index=right.index)
    end
    if head === :||
        # component || component
        left = push_stuff!(args[1], coord, comp)
        right = push_stuff!(args[2], coord, comp)
        return (component=:(Parallel($left,$right)), index=right.index)
    end
    if head === :call
        if args[1] === :(:)
            # Label coordinate arg[2]:arg[3] ( a:(1,2) )
            push!(coord, args[2] => args[3])
            return args[2]
        end
        # Function call
        return e
    end
    if head === :tuple
        # Naked coordinate
        i = length(coord)
        push!(coord, i => e)
        return i
    end

    error("I don't know what to do with argument $e")
end

function push_stuff!(e::QuoteNode, coord, comp)
    if !haskey(coord,e.value)
        error("Coordinate $(e.value) doesn't exist")
    end
    return e.value
end

function push_stuff!(e::Symbol, coord, comp)
    if !haskey(coord,e)
        error("Coordinate $e doesn't exist")
    end
    return e
end

push_stuff!(::LineNumberNode, args...) = nothing

# Circuit macro }}}

# Component macro {{{
macro component(args...)
    esc(component_helper(args...))
end

function component_helper(type,name,circuitikzname,parameters...)

    #  Struct generation {{{
    structexpr = Expr(
                      :struct, false, Expr(
                                           :<:, isempty(parameters) ?
                                           name : Expr(
                                                       :curly,
                                                       name,
                                                       :(T<:Number)
                                                      ),
                                           type
                                          ),
                      Expr(:block, Expr.(:(::),parameters,:T)...)
                     )
    #  }}}

    #  Show generation {{{
    showexpr = Expr(
                    :function,
                    Expr(:call, :show,
                         :(io::IO),
                         Expr(:(::),:x,name),
                        ),
                    Expr(:block,
                         Expr(:call,:print,:io,string(name)),
                         isempty(parameters) ? nothing : Expr(:call,:print,
                                                              :io, '(',
                                                                     Expr.(:.,:x,QuoteNode.(parameters))...,
                                                                     ')',
                                                             )
                        )
                   )
    #  }}}

    #  Circuitikz generation {{{
    circuitikzexpr = Expr(
                          :function,
                          Expr(:call, :show,
                               :(io::IO), :(::MIME"text/circuitikz"),
                               Expr(:(::),:x,name),
                              ),
                          Expr(:block,
                               Expr(:call,:print,:io,string(circuitikzname)),
                               isempty(parameters) ? nothing : Expr(:block,
                                                                    :(print(io,'=')),
                                                                    Expr(:call,:print,:io,
                                                                         Expr(:call,
                                                                              :latexify,
                                                                              Expr(:.,:x,QuoteNode(first(parameters)))
                                                                             )
                                                                        )
                                                                   )
                              )
                         )
    #  }}}

    Expr(:block,
         structexpr,
         showexpr,
         circuitikzexpr,
        )
end
# Component macro }}}

##= Generate components {{{
#           type                name        circuitikzname  parameters
@component  Impedor             Resistor    R               R
@component  Impedor             Capacitor   C               C
@component  Impedor             Inductor    L               L
@component  Impedor             Short       short
@component  Impedor             Open        open

@component  VoltageSource      Battery     battery         U
@component  VoltageSource      VSource     battery1        U
@component  VoltageSource      SinusSource vsourcesin      U ω
# }}}=#

# Series {{{
struct Series <: Impedor
    l::Vector{<:Impedor}

    function Series(l::Vector{<:Impedor})
        types = filter(x -> !<:(x, Union{Short,Parallel}), unique(typeof.(l)))
        if any(types .<: Open)
            return Open()
        end
        l2 = Impedor[]
        append!(l2,filter(x->isa(x,Parallel),l))
        for T in types
            push!(l2,Series(T[filter(x->isa(x,T),l)...]))
        end
        new(l2)
    end
end

Series(x::Vararg{<:Impedor}) = Series(collect(x))

# This is not great, would probably be better to extract l before y is
# constructed
Series(x,y::Series) = Series([x,y.l...])
Series(x::Series,y...) = Series([x...,y...])

Series(x::Vector{Resistor{T}}) where {T<:Number} = Resistor(sum(getproperty.(x,:R)))
Series(x::Vector{Capacitor{T}}) where {T<:Number} = Capacitor(invsum(getproperty.(x,:C)))
Series(x::Vector{Inductor{T}}) where {T<:Number} = Inductor(sum(getproperty.(x,:L)))
Series(x::Vector{Short}) = Short()
Series(x::Vector{Open}) = Open()

show(io::IO, x::Series) = join(io,x," --> ")
show(io::IO, ::MIME"text/circuitikz", x::Series) = print(io,"generic={--}")

iterate(x::Series,state...) = iterate(x.l,state...)
Base.IteratorEltype(::Series) = Base.HasEltype()
eltype(x::Series) = eltype(x.l)
Base.IteratorSize(::Series) = Base.HasLength()
length(x::Series) = length(x.l)

# Series }}}

# Parallel {{{
struct Parallel <: Impedor
    l::Vector{Impedor}

    function Parallel(l::Vector{Impedor})
        types = filter(x -> !<:(x, Union{Open,Series}),unique(typeof.(l)))
        if any(types .<: Short)
            return Short()
        end
        l2 = Impedor[]
        append!(l2,filter(x->isa(x,Series),l))
        for T in types
            push!(l2,Parallel(T[filter(x->isa(x,T),l)...]))
        end
        new(l2)
    end
    function Parallel(l::Vector{Series})
        new(l)
    end
end

Parallel(x::Vararg{<:Impedor}) = Parallel(collect(x))
Parallel(x::Parallel,y...) = Parallel([x...,y...])

Parallel(x::Vector{Resistor{T}}) where {T<:Number} = Resistor(invsum(getproperty.(x,:R)))
Parallel(x::Vector{Capacitor{T}}) where {T<:Number} = Capacitor(sum(getproperty.(x,:C)))
Parallel(x::Vector{Inductor{T}}) where {T<:Number} = Inductor(invsum(getproperty.(x,:L)))
Parallel(x::Vector{Short}) = Short()
Parallel(x::Vector{Open}) = Open()

function show(io::IO, x::Parallel)
    print(io,'(')
    join(io,x,") || (")
    print(io,')')
end
show(io::IO, ::MIME"text/circuitikz", x::Parallel) = print(io,"generic={||}")

iterate(x::Parallel,state...) = iterate(x.l,state...)
Base.IteratorEltype(::Parallel) = Base.HasEltype()
eltype(x::Parallel) = eltype(x.l)
Base.IteratorSize(::Parallel) = Base.HasLength()
length(x::Parallel) = length(x.l)

# Parallel }}}

# Parameters of components {{{
impedance(x::Resistor, s=0) = x.R
impedance(x::Capacitor, s=0) = 1/(s*x.C)
impedance(x::Inductor, s=0) = s*x.L
impedance(x::Short, s=0) = 0
impedance(x::Open, s=0) = Inf
impedance(x::Series, s=0) = sum(impedance.(x, s))
impedance(x::Parallel, s=0) = invsum(impedance.(x, s))

voltage(x::Battery, s=0) = x.U
voltage(x::VSource, s=0) = x.U
voltage(x::SinusSource, s=0) = x.U*x.ω/(s^2+ω^2)
# Parameters of components }}}

# Voltage and current division {{{
"""
```julia
voltageDivision(component, s=0)
```
If a total voltage ``U`` is applied over `component`, the voltage over
respective sub-component is ``k·U``, where ``k`` is the coefficient returned at
its index from this function.

`s` can be given and is the laplace variable of investigation (``≈ iω``).
"""
function voltageDivision end
voltageDivision(::Impedor, s=0) = 1
voltageDivision(::Short, s=0) = 0
voltageDivision(::Open, s=0) = 1
voltageDivision(c::Parallel, s=0) = map(x->voltageDivision(x,s),c)
voltageDivision(c::Series, s=0) = voltageDivision.(c,s) .* impedance.(c,s) ./ sum(x->impedance(x,s),c)
voltageDivision(::VoltageSource, s=0) = 0 # Or the other way around
voltageDivision(::CurrentSource, s=0) = Inf # Or the other way around

"""
```julia
currentDivision(component, s=0)
```
If a total current ``I`` is applied through `component`, the current through
respective sub-component is ``k·I``, where ``k`` is the coefficient return at
its index from this function.

`s` can be given and is the laplace variable of investigation (``≈ iω``).
"""
function currentDivision end
currentDivision(::Impedor, s=0) = 1
currentDivision(::Short, s=0) = 1
currentDivision(::Open, s=0) = 0
currentDivision(c::Parallel, s=0) = ( currentDivision.(c,s) ./  impedance.(c,s) ) .* invsum(x->impedance(x,s),c)
currentDivision(c::Series, s=0) = map(x->currentDivision(x,s),c)
currentDivision(c::VoltageSource, s=0) = Inf # Or the other way around
currentDivision(c::CurrentSource, s=0) = 0 # Or the other way around
# Voltage and current division }}}


# Auxiliary functions {{{
invsum(x) = inv(sum(inv,x))
invsum(f,x) = inv(sum(t->inv(f(t)), x))
# Auxiliary functions }}}

end
