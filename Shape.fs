

namespace Collapse
open System

//Shape class allows for abstraction so several shapes can be used but can all be primitived to Shape and derived from Shape
[<AbstractClass>]
type Shape() =
    abstract member ShapeToString : unit -> string
    override x.ToString() = x.ShapeToString()

//Describes an AABB
type Rectangle(mini:Vector2D, maxi:Vector2D) = 
    inherit Shape()

    member x.min = mini
    member x.max = maxi

    override x.ShapeToString() = "rectangle"


     
