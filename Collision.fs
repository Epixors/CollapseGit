

namespace Collapse
open System

type Collision() = 
    //To prevent bodies from sinking into eachother due to floating point errors, positional correction has to be applied
    member this.PositionCorrection(a:Body, b:Body, m:Manifold) =
        let degreeOfCorrection = 0.015 //How much the position is affected by positional correction
        let threshold = 0.01 //Determines if correction should happen

        //Only let the correction happen when the penetration is over the threshold
        let correction =
                match m.penetration > threshold with
                | true -> (m.AtoB * ((m.penetration - threshold) / (a.inverseMass + b.inverseMass) * degreeOfCorrection))
                | false -> new Vector2D(0.0, 0.0)

        //Change the position as long as the body isn't immovable
        let aModifier = if a.immovable then 0.0 else 1.0
        let bModifier = if b.immovable then 0.0 else 1.0

        if a.immovable <> true then a.changePosition((correction * new Vector2D(bModifier * a.velocity.x, 1.0)) * a.inverseMass * -1.0)
        if b.immovable <> true then b.changePosition((correction * new Vector2D(aModifier * b.velocity.x, 1.0)) * b.inverseMass)

    //Check if two bodies are colliding and if so make sure they are separating
    member this.ResolveCollision(a:Body, b:Body) =
        let mutable manifold = new Manifold(a, b)
        manifold.AABBvsAABB()

        if manifold.overlap then
            let relativeVelocity = b.velocity - a.velocity

            //Velocity along the collision normal, if it's > 0 the bodies are separating
            let velAlongNormal = Vector2D.DotProduct(relativeVelocity, manifold.normal)

            //If the bodies aren't separating apply an impulse
            if velAlongNormal <= 0.0 then
                let e =
                    match a.restitution > b.restitution with
                    | true -> b.restitution
                    | false -> a.restitution

                //See documentation for more details on impulse calculation
                let j = (-(1.0 + e) * velAlongNormal) / ((a.inverseMass) + (b.inverseMass))
              
                let impulse = manifold.normal * j


                a.changeVelocity(impulse * a.inverseMass * -1.0)
                b.changeVelocity(impulse * b.inverseMass) 

                this.PositionCorrection(a, b, manifold)

                manifold.AABBvsAABB()

                //Now adjust for friction
                let relativeVelocityFriction = b.velocity - a.velocity
                let tangentVectorPre = (relativeVelocityFriction - (manifold.normal * Vector2D.DotProduct( relativeVelocityFriction, manifold.normal)))
                let tangentVector = match tangentVectorPre.length() with
                    | 0.0 -> tangentVectorPre
                    | _ -> tangentVectorPre.unit()

                if tangentVector <> Vector2D.get_Zero() then 
                    let jt = Vector2D.DotProduct(relativeVelocityFriction, tangentVector) * -1.0

                    let u = (a.material.staticFriction + b.material.staticFriction) / 2.0

                    if abs(jt) < j * u then
                        let frictionImpulse = tangentVector * jt
                        a.changeVelocity(frictionImpulse * a.inverseMass * -1.0)
                        b.changeVelocity(frictionImpulse * b.inverseMass)
                    else
                        let kineticFriction = (a.material.kineticFriction + b.material.kineticFriction) / 2.0
                        let frictionImpulse = tangentVector * -j * kineticFriction
                        a.changeVelocity(frictionImpulse * a.inverseMass * -1.0)
                        b.changeVelocity(frictionImpulse * b.inverseMass)
