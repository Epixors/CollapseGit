

namespace Collapse
open System

type Vector2D = 
    struct
        val X:float //x-coordinate
        val Y:float //y-coordinate
        new(x:float, y:float) = { X = x; Y = y;} //constructor


        //These member functions allow direct use of mathematical operator on a Vector2D
        static member (+) (a:Vector2D, b:Vector2D) = new Vector2D(a.X + b.X, a.Y + b.Y)
        static member (-) (a:Vector2D, b:Vector2D) = new Vector2D(a.X - b.X, a.Y - b.Y)
        static member ( * ) (a:Vector2D, s:float) = new Vector2D(a.X * s, a.Y * s)
        static member ( * )(a:Vector2D, b:Vector2D) = new Vector2D(a.X * b.X, a.Y * b.Y)
        static member (/) (a:Vector2D, s:float) = a * 1.0/s
        static member get_Zero() = new Vector2D(0.0, 0.0)
        static member DotProduct(a:Vector2D, b:Vector2D) = a.X * b.X + a.Y * b.Y

        //These member functions give some tools to modify/gain information about the vector
        member this.lengthSq() = this.x * this.x + this.y * this.y //Gives the length squared (sqrt not always needed, improves performance)
        member this.length() = this.lengthSq() |> sqrt //Gives the length of the vector
        member this.inv() = this * -1.0 //Returns the inverted vector
        member this.unit() = //Calculates the unit vector
            let length = this.length()
            new Vector2D(this.x/length, this.y/length)

        override v.ToString() = sprintf "[%f, %f]" v.x v.y //Allows the vector to be expressed as a string for debugging purposes

        member this.x = //Returns the x-coordinate through the dot operator such as v.x
            this.X
        
        member this.y = //Returns the y-coordinate through the dot operator such as y.x
            this.Y
    end

