using Circuits, Unitful, UnitfulLatexify, Latexify
set_default(unitformat=:siunitx, fmt="%.3g")
#= Boring syntax {{{
c = Circuit(
[# Coordinates
(0, 0), (3, 0), (3, 3), (0, 3)
],
Dict(# Connections
(1, 2)=>VSource(3u"V"),
(2, 3)=>Resistor(2.4u"Ω"),
(3, 4)=>Inductor(2u"mH"),
(4, 1)=>Short(),
(2, 4)=>Capacitor(5u"μF"),
)
)
# }}} =#
##= Fun syntax {{{
c = @circuit begin
    a:(0, 0) --> VSource(2u"V") --> b:(1, 0) --> Resistor(3u"Ω") --> c:(1, 1) -->
    Capacitor(1u"μF") --> Capacitor(2u"μF") --> d:(0, 1) --> :a
    :b --> Inductor(2u"mH") || Resistor(2u"Ω") --> :d
end
# }}} =#

open("doc/example.tikz", "w") do f
    show(f, MIME"text/circuitikz"(), c; shownodes=true, expandnetworks=true)
end
