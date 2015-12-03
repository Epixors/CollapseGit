

namespace Collapse
open System


type Material = 
    struct
        val staticFriction:float
        val kineticFriction:float
        val density:float
        val restitution:float

        new(sF, kF, d, r) = {staticFriction = sF; kineticFriction = kF; density = d; restitution = r;}

    end