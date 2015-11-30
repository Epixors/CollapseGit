﻿namespace Collapse
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
            let aExtentX = (a.max.x - a.min.x) / 2.0
            let bExtentX = (b.max.x - b.min.x) / 2.0

            let xOverlap = aExtentX + bExtentX - abs(this.AtoB.x)

            if xOverlap > 0.0 then
                let aExtentY = (a.max.y - a.min.y) / 2.0
                let bExtentY = (b.max.y - b.min.y) / 2.0

                let yOverlap = aExtentY + bExtentY - abs(this.AtoB.y)

                if yOverlap > 0.0 then
                    //The bodies have overlap, determine the normal and penetration depth
                    this.overlap <- true
                    if xOverlap < yOverlap then
                        if this.AtoB.x < 0.0 then
                            this.normal <- new Vector2D(-1.0, 0.0)
                        else
                            this.normal <- new Vector2D(1.0, 0.0)
                        
                        this.penetration <- xOverlap
                    else
                        if this.AtoB.y < 0.0 then
                            this.normal <- new Vector2D(0.0 , -1.0)
                        else
                            this.normal <- new Vector2D(0.0, 1.0)

                        this.penetration <- yOverlap


    end