module Circuits
using Latexify

export Circuit, Resistor, Inductor, Capacitor, Short, Battery, VSource, SinusSource

import Base.show

abstract type AbstractComponent end
abstract type Impedor <: AbstractComponent end
abstract type Source <: AbstractComponent end

# Circuit {{{
struct Circuit # Could be considered a graph?
    coordinates::Vector{Tuple{Float64,Float64}}
    connections::Dict{Tuple{Int64,Int64},<:AbstractComponent}
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
        for (i,c) = enumerate(x.coordinates)
            print(io, " (",join(c,','),") node {",i,"}")
        end
        print(io, ";\n")
    end
end
# }}}

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
                         :(io::IO), :(m::MIME"text/plain"),
                         Expr(:(::),:x,name),
                        ),
                    Expr(:block,
                         Expr(:call,:print,:io,string(name)),
                         isempty(parameters) ? nothing : Expr(:block,
                                                              :(print(io,'(')),
                                                                Expr.(:call,:show,:io,
                                                                      :m,Expr.(:.,:x,QuoteNode.(parameters))
                                                                     )...,
                                                                :(print(io,')')),
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
#           type        name        circuitikzname  parameters
@component  Impedor     Resistor    R               R
@component  Impedor     Capacitor   C               C
@component  Impedor     Inductor    L               L
@component  Impedor     Short       short

@component  Source      Battery     battery         U
@component  Source      VSource     battery1        U
@component  Source      SinusSource vsourcesin      U ω


impedance(x::Resistor, s=Inf) = x.R
impedance(x::Capacitor, s=Inf) = s*x.C
impedance(x::Inductor, s=Inf) = 1/(s*x.L)
impedance(x::Short, s=Inf) = 0

voltage(x::Battery, s=Inf) = x.U
voltage(x::VSource, s=Inf) = x.U
voltage(x::SinusSource, s=Inf) = x.U*x.ω/(s^2+ω^2)
# }}}=#
end
