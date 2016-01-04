

namespace Collapse
open System
open Gtk
open FSharp.Charting
open System.Collections.Generic

type Pair =
    struct
        val A:Body
        val B:Body
        new(a:Body, b:Body) = { A = a; B = b; }
    end


//The engine stores the bodies and governs all steps taken to modify them
type Engine(bs:Body array, stopAt:float, ts:float, txtBox:TextView) = 
    let mutable bods = bs //List containing the bodies to be manipulated
    let timestep = 1.0/60.0 //dt
    let mutable currentTime = 0.0 //Starting point in time
    let physics = new Physics(timestep)
    let collision = new Collision()
    let timeStart = System.DateTime.Now

    let mutable pairs = [||]
    
    do bods |> Array.iter(fun b -> (for b2 in bods do if b2 <> b then pairs <- Array.append pairs [|(new Pair(b, b2))|]))

    let mutable sortedPairs =  (pairs |> Seq.ofArray |> Seq.distinctBy (fun elem -> new Pair(elem.B, elem.A)) |> Array.ofSeq)


    //Write to the TextView in the Control Panel
    let Log s =
        let iter = txtBox.Buffer.StartIter
        txtBox.Buffer.Insert((ref iter), "[" + System.DateTime.UtcNow.ToString("HH : mm : ss : ffff") + "] " + s + "\n")

    //Data collection tools
    let stopTime = stopAt
    let mutable drewCharts = false
    let velocityData = new Dictionary<string, (float * float) list>()

    do bods |> Array.iter(fun b -> if b.name <> "None" then velocityData.Add(b.name, [(currentTime, b.velocity.length());])) 

    //Move the simulation forward timestep seconds
    member this.step() =

        if drewCharts <> true then
            //Add velocity data to the bodies that are being logged
            bods |> Array.iter(fun b -> if b.name <> "None" then velocityData.Item(b.name) <- velocityData.Item(b.name) @ [(currentTime, b.velocity.length());] )    

            if currentTime > stopTime then
                Log("It took " + (System.DateTime.Now - timeStart).ToString() + " to simulate " + stopTime.ToString() + " seconds at dt = " + timestep.ToString())
                drewCharts <- true
                for kvp in velocityData do
                    let name = "Velocity Data - " + kvp.Key //Chart name
                    let dataToPrint = kvp.Value //Data to construct chart from
                    //Display the chart in a new window
                    (Chart.Line(dataToPrint, Title = name, XTitle = "Time (s)", YTitle = "Speed (m/s)")).ShowChart()

        bods |> Array.iter(fun b -> physics.applyGravity(b); physics.applyAirFriction(b))
        sortedPairs |> Array.iter(fun p -> collision.ResolveCollision(p.A, p.B))
        bods |> Array.iter(fun b -> physics.translateBody(b))
        currentTime <- currentTime + timestep

    //Returns the bodies the engine is processing
    member this.bodies = bods

    //Add another body
    member this.addBody(b:Body) =
        if b.name <> "None" then velocityData.Add(b.name, [(currentTime, b.velocity.length());]) 
        bods <- Array.append bods [| b |]
        bods |> Array.iter(fun b2 -> (sortedPairs <- Array.append sortedPairs [|(new Pair(b, b2))|]))
