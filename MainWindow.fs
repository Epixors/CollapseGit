namespace Collapse

open System
open Gtk
open System.Drawing
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open Collapse.ParseIni
open System.IO

    type Materials = Metal=0 | Wood=1 | Bouncy=2

    //Window where simulation is rendered
    type Renderer(width, height, mode, title, options, txtBox:TextView, scene) = 
        inherit GameWindow(width, height, mode, title, options)

        let materialList = [| new Material(0.2, 0.2, 0.3, 0.2) ; new Material(0.75, 0.5, 1.2, 0.05) ; new Material(0.01, 0.01, 0.25, 0.99); |]

        let engine = new Engine(scene, 30.0, txtBox)


        //Write to the TextView in the Control Panel
        let Log s =
            let iter = txtBox.Buffer.StartIter
            txtBox.Buffer.Insert((ref iter), "[" + System.DateTime.UtcNow.ToString("HH : mm : ss : ffff") + "] " + s + "\n")

        let drawRectangle(b:Body) =
            let rectShape = b.shape :?> Collapse.Rectangle

            let p = b.position
            let min = rectShape.min + p
            let max = rectShape.max + p

            GL.Begin(BeginMode.Quads)

            GL.Color3(b.color)

            GL.Vertex2(min.x, min.y)
            GL.Vertex2(min.x, max.y)
            GL.Vertex2(max.x, max.y)
            GL.Vertex2(max.x, min.y)

            GL.End()



        //Enable VSync to prevent screen tearing
        do base.VSync <- VSyncMode.On
        do base.WindowBorder <- WindowBorder.Fixed

        //Called when GameWindow is instantiated
        override this.OnLoad e =
                base.OnLoad e
                GL.ClearColor(Color.Black)

        //Called when frame is resized, TO-DO: VIEWPORT
        override this.OnResize e = 
                base.OnResize e
                GL.MatrixMode(MatrixMode.Projection)
                GL.LoadIdentity()
                GL.Ortho((float) base.ClientRectangle.Left, (float) base.ClientRectangle.Right, (float) base.ClientRectangle.Top, (float) base.ClientRectangle.Bottom, -1.0, 1.0)
                GL.Viewport(base.ClientRectangle.Size)

        override this.OnUpdateFrame e =
            base.OnUpdateFrame e
            engine.step()
                
        //Called when a frame is ready to be drawn
        override this.OnRenderFrame e =
            GL.Clear(ClearBufferMask.ColorBufferBit)

            for b in engine.bodies do drawRectangle(b)

            this.Context.SwapBuffers()

            base.OnRenderFrame e

    //Control window and governing process
    module MainWindow =

        let box = new VBox()
        let btnStart = new Button("Start")

        let sceneFiles = Directory.GetFiles("Scenes\\")
        let sceneBox = new ComboBox(sceneFiles)
        sceneBox.Active <- 0

        let txtBox = new TextView()
        txtBox.Editable <- false

        let Log s =
            let iter = txtBox.Buffer.StartIter
            txtBox.Buffer.Insert((ref iter), "[" + System.DateTime.UtcNow.ToString("HH : mm : ss : ffff") + "] " + s + "\n")
                

        let loadConfig() =
            let materialList = [| new Material(0.2, 0.2, 0.3, 0.2) ; new Material(0.75, 0.5, 1.2, 0.05) ; new Material(0.01, 0.01, 0.25, 0.99); |]
            let mutable bodies = []

            let data = (ParseIni.loadConfig(sceneBox.ActiveText))

            for KeyValue(k, v) in data do
                Log("Parsing data for body '" + k + "'")
                let name = ref "None"
                let mass = ref 0.0
                let immovable = ref false
                let posX = ref 0.0
                let posY = ref 0.0
                let velX = ref 0.0
                let velY = ref 0.0
                let width = ref 0.0
                let height = ref 0.0
                let material = ref 1
                let color = ref Color.White

                v |> List.iter ( fun s -> (
                    match (fst s) with
                        | "mass" -> mass := (float)(snd s)
                        | "immovable" -> immovable := if (snd s) <> "false" then true else false
                        | "positionX" -> posX := (float)(snd s)
                        | "positionY" -> posY := (float)(snd s)
                        | "width" -> width := (float)(snd s)
                        | "height" -> height := (float)(snd s)
                        | "material" -> material := (int)(snd s)
                        | "color" -> color := Color.FromName((snd s))
                        | "name" -> name := (snd s)
                        | "velocityX" -> velX := (float)(snd s)
                        | "velocityY" -> velY := (float)(snd s)
                ))

                let newRectangle = new Collapse.Rectangle(new Vector2D(0.0, 0.0), new Vector2D(!width, !height))
                let newBody = new Body(newRectangle, !mass, materialList.[!material], pos = new Vector2D(!posX, !posY), vel = new Vector2D(!velX, !velY), immo = !immovable, col = !color, nm = !name)

                bodies <- newBody::bodies


            bodies


        let StartEngine b =
            async{
                if b then
                    let c = loadConfig()
                    use renderer = new Renderer(1280, 1024, Graphics.GraphicsMode.Default, "Collapse Engine - Renderer" , GameWindowFlags.Default, txtBox, c)
                    do renderer.Run()
                }

        btnStart.Clicked.AddHandler(fun s a -> Async.Start(StartEngine true))


        type MyWindow() as this =
            inherit Window("Collapse Engine - Control Panel")
          
            //Create the window and it's children
            do this.SetDefaultSize(400,250)
            do this.DeleteEvent.AddHandler(fun o e -> this.OnDeleteEvent(o,e))
            do box.PackStart(btnStart, false, false, (uint32 0))
            do box.PackStart(sceneBox, false, false, (uint32 0))
            do box.PackStart(txtBox, true, true, (uint32 5))
            do this.Add(box)

            //Show the starting message
            do ["Hola!"; "To begin, select a scene" ; "Press Start to start simulating a scene" ; "Currently loaded: Default Scene"] |> List.iter(fun s -> Log s)

            //Show the window
            do this.ShowAll()


            member this.OnDeleteEvent(o,e:DeleteEventArgs) = 
                Application.Quit ()
                e.RetVal <- true
