using Circuits, Unitful, UnitfulLatexify, Latexify
set_default(unitformat=:siunitx)
c = Circuit(
            [# Coordinates
             (0,0),(3,0),(3,3),(0,3)
            ],
            Dict(# Connections
                 (1,2)=>VSource(3u"V"),
                 (2,3)=>Resistor(2.4u"Ω"),
                 (3,4)=>Inductor(2u"mH"),
                 (4,1)=>Short(),
                 (2,4)=>Capacitor(5u"μF"),
                )
           )

open("doc/example.tikz", "w") do f
    show(f,MIME"text/circuitikz"(),c;shownodes=true)
end
