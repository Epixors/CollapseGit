

namespace Collapse
open System
open System.Drawing

//Stores information about a body and allows manipulation of the body
type Body(sh:Shape, ma:float, mat:Material, rot:float, rotSpeed:float, pos:Vector2D, ?vel:Vector2D, ?immo:bool, ?col:Color, ?nm:string) = 
    let rnd = new Random()
    let mutable s = sh :?> Collapse.Rectangle //Shape
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

    let mutable p = pos //Stores the position in the scene
    let mutable v = defaultArg vel (new Vector2D(0.0, 0.0)) //Stores the body's velocity
    let mutable f = [] //List containing all forces acting on the body
    let mutable pF = [] //List containg forces from last step for debugging

    let mutable r = (rot * Math.PI)/180.0
    let mutable t = 0.0
    let mutable rS = rotSpeed

    (* These members return information about the object using the dot operator *)

    member this.shape =
            s

    member this.rotation = r

    member this.rotationSpeed = rS

    member this.torque = t

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

    member this.previousForces = pF


    (* These members provide functions to change internal values *)

    member this.changeVelocity v2 =
        v <- v + v2

    member this.changePosition p2 =
        p <- p + p2

    member this.addForce(f2:Vector2D) =
        f <- f2::f

    member this.clearForces() =
        pF <- f
        f <- []

    member this.rotate(d:float) =
        r <- r + ((d * Math.PI)/180.0)
        s <- s.rotate(r)

    
