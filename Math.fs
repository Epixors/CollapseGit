
namespace Collapse
open System

//As heavy assumptions are made during the physics processing, it is useful to restrict functions like equal() to a certain amount of floating points
//Math() provides an instance for all these things and certain constants

type Math() = 
    let epsilon = 0.0001f
    let epsilonSquared = epsilon * epsilon

    //Compare to a precision of 4 decimal points
    let equal a b =
        a - b <= epsilon

    let round f =
        (int)(f + 0.5f)

