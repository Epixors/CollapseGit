

namespace Collapse
open System

type Physics(deltatime:float) =
    let dt = deltatime
     
    //Apply gravity to an object
    member this.applyGravity(b:Body) =
        b.addForce(new Vector2D(0.0, b.mass * -9.81))

    //Move the body according to the forces acting on it
    member this.translateBody(b:Body) =
        if b.immovable <> true then
            //Integrate according to semi-implicit Euler
            let xAcceleration = b.totalForce.x * b.inverseMass
            let yAcceleration = b.totalForce.y * b.inverseMass

            b.changeVelocity(new Vector2D(xAcceleration * dt, yAcceleration * dt))
            b.changePosition(new Vector2D(b.velocity.x * dt, b.velocity.y * dt))

            b.clearForces()
        