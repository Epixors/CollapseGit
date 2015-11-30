

namespace Collapse
open System
open Gtk
open FSharp.Charting
open System.Collections.Generic

//The engine stores the bodies and governs all steps taken to modify them
type Engine(bs:Body list, stopAt:float, txtBox:TextView) = 
    let mutable bods = bs //List containing the bodies to be manipulated
    let timestep = 0.02 //dt
    let mutable currentTime = 0.0 //Starting point in time
    let physics = new Physics(timestep)
    let collision = new Collision()
    let timeStart = System.DateTime.Now

    //Write to the TextView in the Control Panel
    let Log s =
        let iter = txtBox.Buffer.StartIter
        txtBox.Buffer.Insert((ref iter), "[" + System.DateTime.UtcNow.ToString("HH : mm : ss : ffff") + "] " + s + "\n")

    //Data collection tools
    let stopTime = stopAt
    let mutable drewCharts = false
    let velocityData = new Dictionary<string, (float * float) list>()

    do bods |> List.iter(fun b -> if b.name <> "None" then velocityData.Add(b.name, [(currentTime, b.velocity.length());])) 

    //Move the simulation forward timestep seconds
    member this.step() =

        if drewCharts <> true then
            //Add velocity data to the bodies that are being logged
            bods |> List.iter(fun b -> if b.name <> "None" then velocityData.Item(b.name) <- velocityData.Item(b.name) @ [(currentTime, b.velocity.length());] )    

            if currentTime > stopTime then
                Log("It took " + (System.DateTime.Now - timeStart).ToString() + " to simulate " + stopTime.ToString() + " seconds at dt = " + timestep.ToString())
                drewCharts <- true
                for kvp in velocityData do
                    let name = "Velocity Data - " + kvp.Key //Chart name
                    let dataToPrint = kvp.Value //Data to construct chart from
                    //Display the chart in a new window
                    (Chart.Line(dataToPrint, Title = name, XTitle = "Time (s)", YTitle = "Speed (m/s)")).ShowChart()

        bods |> List.iter(fun b -> physics.applyGravity(b))
        bods |> List.iter(fun b -> (for b2 in bods do collision.ResolveCollision(b2,b)))
        bods |> List.iter(fun b -> physics.translateBody(b))
        currentTime <- currentTime + timestep

    //Returns the bodies the engine is processing
    member this.bodies = bods

    //Add another body
    member this.addBody(b:Body) =
        if b.name <> "None" then velocityData.Add(b.name, [(currentTime, b.velocity.length());]) 
        bods <- b::bods