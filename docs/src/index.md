```@meta
CurrentModule = Circuits
```
```@setup index
using Circuits
using Plots; gr()
default(;legend=false, fontfamily="Computer Modern")
```

# Circuits

Generate, visualize and analyze electrical networks.


## Creating a circuit

The simplest way to create a circuit is through the `@circuit` macro.  As an
example, here a square network is generated with two named (`a` and  `b`) and
two unnamed (or rather not explicitly named) nodes, one voltage source, a
resistor and a capacitor in series and a capacitor and an inductor in parallel:

```julia
c = @circuit begin
    b:(0,0) --> VSource(3) --> (1,0) --> Resistor(4) --> Inductor(2) --> (1,1) --> a:(0,1)
    :a --> Capacitor(1) // Inductor(4) --> :b
end
```

## Visualizing a circuit

The normal `show` method for `Circuit`s just prints a brief summary (number of
nodes, number of sources, number of impedors (which is what I call RCL
components... if you're an electrical engineer and know a better word lmk)).

To see the entire circuit, a method
`show(io::IO,::MIME"text/circuitikz",c::Circuit)` is implemented. If you use
this method to print to a file, and then `\input` that file inside a
`tikzpicture` in a LaTeX document with `circuitikz` loaded, it will draw the
circuit.

`show(io,mime,c,shownodes=true)` will also show the node labels, which is
useful while modifying or analyzing the circuit.

## Simplifying a network

A `Parallel` will happily contain any collection of `Impedor`s,  except
`Parallel`s which will be expanded during construction
(`Parallel(a,Parallel(b,c)) = Parallel(a,b,c)`). And vice versa for `Series`.
But for calculations, a series or parallel coupling of the same type of
component can be simplified by either addition or reciprocal addition of their
characteristic parameters. `simplify(n::Network)` will walk through the tree and
combine the components like this, until you are left with a single component,
a `Series` of RCL components and `Parallel`s, a `Parallel` of RCL components and
`Series`, a `Short` or an `Open`.


## Analyzing a network

For an `Impedor` (`Series`, `Parallel`, RCL component, `Short` or `Open`) `i`, 
`voltageDivision(i,s)` returns  ``א`` such that component with index ``k`` has a
voltage drop ``א[k]·V₀`` if `i` has a total voltage drop ``V₀``. `s` is the
complex frequency. By default this is `0` (DC voltage).

For the network above, opening the voltage source and going from the node at
`(1,1)` to `b`, (i.e. `n =
Series(Resistor(4),Inductor(2),Parallel(Capacitor(1),Inductor(4)))`),
`voltageDivision(n) == [1.0, 0.0, [0.0, 0.0]]` by which we can read that the
resistor experiences a voltage drop of ``V₀``, and the rest of the components
none (as expected for DC voltage). `voltageDivision(n,1im) ==  [0.97 - 0.16im,
0.081 + 0.48im, [-0.054 - 0.32im, -0.054 - 0.32im]]` (so at ``1`` frequency
unit, those are the respective responses in each component).

Current division works the same: `currentDivision(n) == [1, 1, [0.0, NaN]]`
(the NaN is from an `Inf/Inf`, a good symbolic package will probably fix this,
but for now I mostly read those as `1`s. `currentDivision(n,1im) == [1, 1,
[1.33 + 0.0im, -0.33 - 0.0im]]`. That's probably right? Idk.

```@example index
c = @circuit a:(0,0) --> Inductor(9e-3) --> b:(0,1) --> Resistor(11) --> c:(1,1) --> Capacitor(1.455e-6) --> d:(1,0)
latexstandalone(c, "example1.png") # hide
```
![](example1.png)

```@example index
# One day this will be `p = Network(c, :a, :d)`, but that's not implemented yet.
# `A --> B --> C` currently doesn't return a single `Series`. That's a bug.
p = Series(Inductor(9e-3), Resistor(11), Capacitor(1.455e-6)) # H, Ω, F
s = range(0, 20000, length=1000) # Hz
plot(s, map(ω->abs(5*voltageDivision(p, im*ω)[3]), s);
        xlabel="\$\\nu / \\mathrm{Hz}\$", ylabel="\$U / \\mathrm{V}\$",
    )
vline!([1/sqrt(9e-3*1.455e-6)]) # theoretical resonance frequency of LC circuit
savefig("example2.png") # hide
```
![](example2.png)

## To do

* Connect to LightGraphs.jl?
* Function to extract `Network` from `Circuit` (at specific nodes)
* More recursion in `simplify`. MOAR
* `Circuit` to matrix for computer circuit analysis
* Bode diagrams
* Power calculations
* Get rid of some NaNs in current and voltage division
* ...

```@index
```

```@autodocs
Modules = [Circuits]
```
