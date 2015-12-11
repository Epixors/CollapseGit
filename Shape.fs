

namespace Collapse
open System

//Shape class allows for abstraction so several shapes can be used but can all be primitived to Shape and derived from Shape
[<AbstractClass>]
type Shape() =
    abstract member ShapeToString : unit -> string
    override x.ToString() = x.ShapeToString()

//Describes an AABB
type Rectangle(bL:Vector2D, bR:Vector2D, tR:Vector2D, tL:Vector2D) = 
    inherit Shape()

    member x.bottomLeft = bL
    member x.bottomRight = bR
    member x.topLeft = tL
    member x.topRight = tR

    member x.rotate(angle) = 
        let sine = sin(angle)
        let cosine = cos(angle)

        //x = x * cs - y * sn;
        //y = x * sn + y * cs;

        let bLR = new Vector2D(x.bottomLeft.x * cosine - x.bottomLeft.y * sine, x.bottomLeft.x * sine + x.bottomLeft.y * cosine)
        let bRR = new Vector2D(x.bottomRight.x * cosine - x.bottomRight.y * sine, x.bottomRight.x * sine + x.bottomRight.y * cosine)
        let tLR = new Vector2D(x.topLeft.x * cosine - x.topLeft.y * sine, x.topLeft.x * sine + x.topLeft.y * cosine)
        let tRR = new Vector2D(x.topRight.x * cosine - x.topRight.y * sine, x.topRight.x * sine + x.topRight.y * cosine)

        new Rectangle(bLR, bRR, tRR, tLR)

    override x.ShapeToString() = "rectangle"



     
