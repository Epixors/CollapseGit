

namespace Collapse
open System

//Shape class allows for abstraction so several shapes can be used but can all be primitived to Shape and derived from Shape
[<AbstractClass>]
type Shape() =
    abstract member ShapeToString : unit -> string
    override x.ToString() = x.ShapeToString()

//Describes a (bounding) box
type Rectangle(bL:Vector2D, bR:Vector2D, tR:Vector2D, tL:Vector2D) = 
    inherit Shape()

    //Vectors defining the 4 corners
    member x.bottomLeft = bL
    member x.bottomRight = bR
    member x.topLeft = tL
    member x.topRight = tR

    //Rotates all vectors by the given angle and returns a new rectangle
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

    //Returns the vertices as a list
    member x.vertices = 
        [ x.bottomLeft; x.bottomRight; x.topLeft; x.topRight ]

    //Calculates the x- and y-axis in model space and returns them as a list
    member x.axes =
        [(x.bottomRight - x.bottomLeft)/(x.bottomRight - x.bottomLeft).lengthSq(); (x.topLeft - x.bottomLeft)/(x.topLeft - x.bottomLeft).lengthSq() ]


    override x.ShapeToString() =
        let message = List.fold( fun acc x -> acc + " , " + x.ToString()) "rect: " x.vertices
        message

     
