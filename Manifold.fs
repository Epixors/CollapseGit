namespace Collapse
open System


type Manifold =
    struct
        //Given in constructor
        val A:Body
        val B:Body
        val AtoB:Vector2D //Position from A to B

        //Generated during collision detection
        val mutable normal:Vector2D //Calculated if overlap exists, gives the direction in which the collision is happening
        val mutable penetration:float //Calculated if overlap exists, gives the penetration depth of the exist with least penetration
        val mutable overlap:bool

        new(a:Body, b:Body) = { A = a; B = b; AtoB = b.position - a.position; normal = new Vector2D(0.0, 0.0); penetration = 0.0; overlap = false }

        //Check if two AABB's are colliding and if so generate the manifold
        member this.AABBvsAABB() =
            //Get the two shapes
            let a = this.A.shape :?> Collapse.Rectangle
            let b = this.B.shape :?> Collapse.Rectangle

            //Determine if overlap is happening using SAT, see documentation for more details
            let aExtentX = (a.bottomRight.x - a.bottomLeft.x) / 2.0
            let bExtentX = (b.bottomRight.x - b.bottomLeft.x) / 2.0

            let xOverlap = aExtentX + bExtentX - abs(this.AtoB.x)

            if xOverlap > 0.0 then
                let aExtentY = (a.topLeft.y - a.bottomLeft.y) / 2.0
                let bExtentY = (b.topLeft.y - b.bottomLeft.y) / 2.0

                let yOverlap = aExtentY + bExtentY - abs(this.AtoB.y)

                if yOverlap > 0.0 then
                    //The bodies have overlap, determine the normal and penetration depth
                    this.overlap <- true
                    if xOverlap < yOverlap then
                        match this.AtoB.x with
                        | x when x < 0.0 -> this.normal <- new Vector2D(-1.0, 0.0)
                        | _ -> this.normal <- new Vector2D(1.0, 0.0)

                        
                        this.penetration <- xOverlap
                    else
                        match this.AtoB.y with
                        | y when y < 0.0 -> this.normal <- new Vector2D(0.0, -1.0)
                        | _ -> this.normal <- new Vector2D(0.0, 1.0)

                        this.penetration <- yOverlap


    end
