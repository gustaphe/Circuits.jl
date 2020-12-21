using Circuits
using Documenter

makedocs(;
    modules=[Circuits],
    authors="David Gustavsson <david.e.gustavsson@gmail.com> and contributors",
    repo="https://github.com/gustaphe/Circuits.jl/blob/{commit}{path}#L{line}",
    sitename="Circuits.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://gustaphe.github.io/Circuits.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/gustaphe/Circuits.jl",
)
