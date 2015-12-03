

namespace Collapse
open System
open System.Drawing

//Stores information about a body and allows manipulation of the body
type Body(sh:Shape, ma:float, mat:Material, ?pos:Vector2D, ?vel:Vector2D, ?immo:bool, ?col:Color, ?nm:string) = 
    let rnd = new Random()
    let s = sh :?> Collapse.Rectangle //Shape
    let clr = defaultArg col Color.White //Color
    let bClr = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0,255))
    let m = ma //Mass
    let e = mat.restitution //Restitution
    let mate = mat
    let i = defaultArg immo false //Immovable, won't change position
    let n = defaultArg nm "None"
    let im = 
        if ma.Equals(0.0) then 
            0.0 //Infinite mass is represented by 0.0, this prevents a division by zero error
         else
            1.0/m //Inverse mass is calculated as it's often used in physics calculations

    let mutable p = defaultArg pos (new Vector2D((s.max.x - s.min.x)/2.0, (s.max.y - s.min.y)/2.0)) //Stores the position in the scene
    let mutable v = defaultArg vel (new Vector2D(0.0, 0.0)) //Stores the body's velocity
    let mutable f = [] //List containing all forces acting on the body


    (* These members return information about the object using the dot operator *)

    member this.shape = s

    member this.name = n

    member this.color = clr

    member this.borderColor = bClr

    member this.position = p

    member this.restitution = e

    member this.material = mate

    member this.mass = m

    member this.inverseMass = im

    member this.forces = f

    member this.velocity = v

    member this.immovable = i 

    member this.totalForce = f |> List.sum


    (* These members provide functions to change internal values *)

    member this.changeVelocity v2 =
        v <- v + v2

    member this.changePosition p2 =
        p <- p + p2

    member this.addForce(f2:Vector2D) =
        f <- f2::f

    member this.clearForces() =
        f <- []

    
