

namespace Collapse
open System
open System.Drawing
open OpenTK
open OpenTK.Graphics.OpenGL4


type Renderer(width, height, mode, title, options, log) = 
    inherit GameWindow(width, height, mode, title, options)

    override this.OnLoad(e) =
            GL.ClearColor(Color.Black)
            base.OnLoad(e)

    override this.OnResize(e) = 
            base.OnResize(e)

    override this.OnRenderFrame(e) =
        GL.Clear(ClearBufferMask.ColorBufferBit)

        this.Context.SwapBuffers()

        base.OnRenderFrame(e)

