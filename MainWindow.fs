namespace Collapse

open System
open Gtk
open System.Drawing
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open Collapse.ParseIni
open System.IO
open OpenTK.Input

    type Materials = Metal=0 | Wood=1 | Bouncy=2

    //Window where simulation is rendered
    type Renderer(width, height, mode, title, options, txtBox:TextView, scene, timestep, scale:float, endSimTime) = 
        inherit GameWindow(width, height, mode, title, options)

        let materialList = [| new Material(0.2, 0.2, 0.3, 0.2) ; new Material(0.75, 0.5, 1.2, 0.05) ; new Material(0.01, 0.01, 0.25, 0.99); |]

        let engine = new Engine(scene, endSimTime, timestep, txtBox)

        let mutable cornerA = new Vector2D(-100.0, -100.0)
        let mutable cornerB = new Vector2D(-100.0, -100.0)
        let mutable speed = new Vector2D(-100.0, -100.0)

        //Write to the TextView in the Control Panel
        let Log s =
            let iter = txtBox.Buffer.StartIter
            txtBox.Buffer.Insert((ref iter), "[" + System.DateTime.UtcNow.ToString("HH : mm : ss : ffff") + "] " + s + "\n")

        let handleInput k =
            if k.Equals(Key.B) then
                let name = "None"
                let mass = 10.0/100.0
                let immovable = false
                let posX = (cornerA.x + cornerB.x)/200.0
                let posY = (cornerA.y + cornerB.y)/200.0
                let velX = 0.0
                let velY = 0.0
                let width = abs(cornerB.x - cornerA.x) / 100.0
                let height = abs(cornerB.y - cornerA.Y) / 100.0
                let material = 1
                let color = Color.White
                let rot = 0.0
                let rS = 0.0
                               
                let vertices = [| new Vector2D(-(width/2.0), -(height/2.0)); new Vector2D((width/2.0), -(height/2.0)); new Vector2D((width/2.0), (height/2.0)); new Vector2D(-(width/2.0), (height/2.0)) |]
                let newRectangle = new Collapse.Rectangle(vertices.[0], vertices.[1], vertices.[2], vertices.[3])
                let newBody = new Body(newRectangle, mass, materialList.[material], rot, rS, new Vector2D(posX, posY), vel = new Vector2D(velX, velY), immo = immovable, col = color, nm = name)

                Log("Happy potato")

                engine.addBody(newBody)

            else 
                Log("Sad potato")

        let handleMouseInput(m:MouseButtonEventArgs) =

            match m.Button with
                | MouseButton.Left -> cornerA <- new Vector2D((float)(Mouse.GetState(0)).X, (float)(Mouse.GetState(0).Y))
                | MouseButton.Right -> cornerB <- new Vector2D((float)m.Position.X, (float)m.Position.Y)
                | MouseButton.Middle -> speed <- new Vector2D((float)m.Position.X, (float)m.Position.Y)

            Log("A: " + cornerA.ToString() + " ; B: " + cornerB.ToString() + " ; speed: " + speed.ToString())

        let handleKey =
            new EventHandler<KeyboardKeyEventArgs>(fun sender eventargs -> (handleInput(eventargs.Key)))

        let handleMouse =
            new EventHandler<MouseButtonEventArgs>(fun sender eventargs -> handleMouseInput(eventargs))
        

        let drawRectangle(b:Body) =
            let rectShape = b.shape :?> Collapse.Rectangle

            let p = b.position
            //0: bottom left, 1: bottom right, 2: top right, 3: top left (CCW)
            let vertices = [| (p + rectShape.bottomLeft) * scale; (p + rectShape.bottomRight) * scale; (p + rectShape.topRight) * scale; (p + rectShape.topLeft) * scale|]

            GL.Begin(BeginMode.Quads)
           
            GL.Color3(b.borderColor)

            let borderSize = 1.0

            GL.Vertex2(vertices.[0].x - borderSize, vertices.[0].y - borderSize) //bottom left
            GL.Vertex2(vertices.[3].x - borderSize, vertices.[3].y + borderSize) //top left
            GL.Vertex2(vertices.[2].x + borderSize, vertices.[2].y + borderSize) //top right
            GL.Vertex2(vertices.[1].x + borderSize, vertices.[1].y - borderSize) //bottom right

            GL.End()


            GL.Begin(BeginMode.Quads)

            GL.Color3(b.color)

            GL.Vertex2(vertices.[0].x + borderSize, vertices.[0].y + borderSize) //bottom left
            GL.Vertex2(vertices.[3].x + borderSize, vertices.[3].y - borderSize) //top left
            GL.Vertex2(vertices.[2].x - borderSize, vertices.[2].y - borderSize) //top right
            GL.Vertex2(vertices.[1].x - borderSize, vertices.[1].y + borderSize) //bottom right

            GL.End()

            for f in b.previousForces do
                GL.Begin(BeginMode.Lines)
                GL.Color3(b.color)
                let pC = p * scale
                GL.Vertex2(pC.x, pC.y)
                GL.Vertex2(pC.x + f.x * scale, pC.y + f.y * scale)

                GL.End()

        //Enable VSync to prevent screen tearing
        do base.VSync <- VSyncMode.On
        do base.WindowBorder <- WindowBorder.Fixed

        //Called when GameWindow is instantiated
        override this.OnLoad e =
                base.OnLoad e
                GL.ClearColor(Color.Black)

                this.Keyboard.KeyDown.AddHandler(handleKey)
                this.Mouse.ButtonUp.AddHandler(handleMouse)
        

        //Called when frame is resized, TO-DO: VIEWPORT
        override this.OnResize e = 
                base.OnResize e
                GL.MatrixMode(MatrixMode.Projection)
                GL.LoadIdentity()
                GL.Ortho((float) base.ClientRectangle.Left, (float) base.ClientRectangle.Right, (float) base.ClientRectangle.Top, (float) base.ClientRectangle.Bottom, -1.0, 1.0)
                GL.Viewport(base.ClientRectangle.Size)

        override this.OnUpdateFrame e =
            base.OnUpdateFrame e

            //Handle input
            let mouseState = Mouse.GetState(0)
            let keyboardState = Keyboard.GetState(0)

            engine.step()
                
        //Called when a frame is ready to be drawn
        override this.OnRenderFrame e =
            GL.Clear(ClearBufferMask.ColorBufferBit)

            for b in engine.bodies do drawRectangle(b)

            GL.PointSize((float32)10.0)
            GL.Begin(BeginMode.Points)
            GL.Color3(Color.White)

            GL.Vertex2(cornerA.x, cornerA.y)
            GL.Vertex2(cornerB.x, cornerB.y)
            GL.Vertex2(speed.x, speed.y)

            GL.End()

            this.Context.SwapBuffers()

            base.OnRenderFrame e

    //Control window and governing process
    module MainWindow =

        let box = new VBox()
        let btnStart = new Button("Start")

        let sceneFiles = Directory.GetFiles("Scenes/")
        let sceneBox = new ComboBox(sceneFiles)
        sceneBox.Active <- 0

        let txtBox = new TextView()
        txtBox.Editable <- false

        let settingBox = new HBox()
        let timeStepSelect = new SpinButton(1.0,60.0,0.1)
        let timeStepSelectLabel = new Label("Timestep (s)")
        timeStepSelect.Value <- 60.0


        let endSimSelect = new SpinButton(1.0, 300.0, 1.0)
        endSimSelect.Value <- 10.0
        let endSimSelectLabel = new Label("End sim (s)")
        
        let scaleSelect = new SpinButton(1.0, 100.0, 1.0)
        scaleSelect.Value <- 100.0
        let scaleSelectLabel = new Label("Scale")

        settingBox.Add(timeStepSelectLabel)
        settingBox.Add(timeStepSelect)
        settingBox.Add(endSimSelectLabel)
        settingBox.Add(endSimSelect)
        settingBox.Add(scaleSelectLabel)
        settingBox.Add(scaleSelect)

        let Log s =
            let iter = txtBox.Buffer.StartIter
            txtBox.Buffer.Insert((ref iter), "[" + System.DateTime.UtcNow.ToString("HH : mm : ss : ffff") + "] " + s + "\n")
                

        let loadConfig() =
            let materialList = [| new Material(0.2, 0.2, 0.3, 0.2) ; new Material(0.75, 0.5, 1.2, 0.05) ; new Material(0.01, 0.01, 0.25, 0.99); |]
            let mutable bodies = [||]

            let file = sceneBox.Active
            let data = ParseIni.loadConfig(sceneFiles.[sceneBox.Active])

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
                let rot = ref 0.0
                let rS = ref 0.0

                v |> List.iter ( fun s -> (
                    match (fst s) with
                        | "mass" -> mass := (float)(snd s)
                        | "immovable" -> immovable := if (snd s) <> "false" then true else false
                        | "positionX" -> posX := (float)(snd s) / 100.0
                        | "positionY" -> posY := (float)(snd s) / 100.0
                        | "width" -> width := (float)(snd s) / 100.0
                        | "height" -> height := (float)(snd s) / 100.0
                        | "material" -> material := (int)(snd s)
                        | "color" -> color := Color.FromName((snd s))
                        | "name" -> name := (snd s)
                        | "velocityX" -> velX := (float)(snd s) / 100.0
                        | "velocityY" -> velY := (float)(snd s) / 100.0
                        | "rotation" -> rot := (float)(snd s)
                        | "rotationSpeed" -> rS := ((float)(snd s) * Math.PI) / 180.0
                ))


                //0: bottom left, 1: bottom right, 2: top right, 3: top left (CCW)
                let vertices = [| new Vector2D(-(!width/2.0), -(!height/2.0)); new Vector2D((!width/2.0), -(!height/2.0)); new Vector2D((!width/2.0), (!height/2.0)); new Vector2D(-(!width/2.0), (!height/2.0)) |]
                let newRectangle = new Collapse.Rectangle(vertices.[0], vertices.[1], vertices.[2], vertices.[3])
                let newBody = new Body(newRectangle, !mass, materialList.[!material], !rot, !rS, new Vector2D(!posX, !posY), vel = new Vector2D(!velX, !velY), immo = !immovable, col = !color, nm = !name)
                newBody.rotate(!rot)

                bodies <- Array.append bodies [| newBody |]


            bodies


        let StartEngine b =
            async{
                if b then
                    let c = loadConfig()
                    use renderer = new Renderer(1280, 1024, Graphics.GraphicsMode.Default, "Collapse Engine - Renderer" , GameWindowFlags.Default, txtBox, c, timeStepSelect.Value, scaleSelect.Value, endSimSelect.Value)
                    do renderer.Run()
                }

        btnStart.Clicked.AddHandler(fun s a -> Async.Start(StartEngine true))
        sceneBox.Changed.AddHandler(fun s a -> Log("Selected: " + sceneFiles.[sceneBox.Active]))


        type MyWindow() as this =
            inherit Window("Collapse Engine - Control Panel")
          

            //Create the window and it's children
            do this.SetDefaultSize(400,250)
            do this.BorderWidth <- (uint32) 0

            do this.DeleteEvent.AddHandler(fun o e -> this.OnDeleteEvent(o,e))
            do box.PackStart(btnStart, false, false, (uint32 0))
            do box.PackStart(sceneBox, false, false, (uint32 0))
            do box.PackStart(settingBox, false, false, (uint32 0))
            do box.PackStart(txtBox, true, true, (uint32 5))
            do this.Add(box)


            //Show the starting message
            do ["Hola!"; "To begin, select a scene" ; "Press Start to start simulating a scene" ; "Selected: " + sceneFiles.[sceneBox.Active] ] |> List.iter(fun s -> Log s)

            //Show the window
            do this.ShowAll()


            member this.OnDeleteEvent(o,e:DeleteEventArgs) = 
                Application.Quit ()
                e.RetVal <- true
