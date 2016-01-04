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



        member this.OBBvsOBB() =
            //Get the shapes
            let a = this.A.shape :?> Collapse.Rectangle
            let b = this.B.shape :?> Collapse.Rectangle

            //Get the object positions
            let aPos = this.A.position
            let bPos = this.B.position


            //Construct lists of the vertices added with the object's position
            let aVerts = a.vertices |> List.map ( fun x -> x + aPos)
            let bVerts = b.vertices |> List.map (fun x -> x  + bPos)
            //Create an empty list to store the vertices for the Minkowski Difference
            let mutable minkowskiDifference = []


            let computeCentroid (points: Vector2D list) =       
                let x = (points |> List.sumBy(fun x -> x.x))/(float)points.Length
                let y = (points |> List.sumBy(fun x -> x.y))/(float)points.Length
                new Vector2D(x, y)

            //Create the Minkowski Difference
            for aVert in aVerts do
                for bVert in bVerts do
                    minkowskiDifference <- (aVert - bVert)::minkowskiDifference

            

            let clockwise (p1:Vector2D) (p2:Vector2D) (p3:Vector2D) =
                (p2.X - p1.X) * (p3.Y - p1.Y) - (p2.Y - p1.Y) * (p3.X - p1.X) < 1e-9
                
            let hull (pts : Vector2D list) = 
                let rec fill acc pt =
                    match acc with
                    | a :: b :: _ when clockwise b a pt -> fill acc.Tail pt
                    | _ -> pt :: acc
                let p0 = pts 
                         |> List.reduce (fun p1 p2 -> 
                            if p2.Y < p1.Y || (p1.Y = p2.Y && p2.X < p1.X) then p2 else p1)
                pts 
                |> List.sortBy (fun p ->
                    let d = (p0-p).length()
                    (Math.Round((p0.X - p.X) / d, 8), d))
                |> List.fold fill []
                |> List.rev

            let lineIntersectionPoint(rayStart:Vector2D, rayEnd:Vector2D, lineStart:Vector2D, lineEnd:Vector2D) =
                let A1 = rayEnd.y - rayStart.y
                let B1 = rayStart.x - rayEnd.x
                let C1 = A1 * rayStart.x + B1 * rayStart.y

                let A2 = lineEnd.y - lineStart.y
                let B2 = lineStart.x - lineEnd.x
                let C2 = A2 * lineStart.x + B2 * lineStart.y

                let delta = A1 * B2 - A2 * B1

                match delta with
                | 0.0 -> rayStart
                | _ -> new Vector2D( (B2*C1 - B1*C2)/delta, (A1*C2 - A2*C1)/delta)

            let test = ref "Execute[{"
            a.vertices |> List.iter( fun x -> test := !test + "\"(" + x.x.ToString() + ", " + x.y.ToString() + ")\",")
            test := !test + "\"Polygon[A,B,D,C]\","

            b.vertices |> List.iter( fun x -> test := !test + "\"(" + x.x.ToString() + ", " + x.y.ToString() + ")\",")
            test := !test + "\"Polygon[E,F,H,G]\","

            minkowskiDifference|> List.iter( fun x -> test := !test + "\"(" + x.x.ToString() + ", " + x.y.ToString() + ")\",")

            printfn "%s" (!test + "}]")
            //Simplify the MDP by generating the hull using Andrew Monotone's Chain algorithm. This leaves only the outer points that make up the boundaries of the polygon
            minkowskiDifference <- hull minkowskiDifference
            let minkowskiCenter = computeCentroid(minkowskiDifference)

            //Setup the point-in-poly test
            let vertx = minkowskiDifference |> List.map ( fun x -> x.x) //x-coordinates of vertices
            let verty = minkowskiDifference |> List.map (fun x -> x.y) // y-coordinates of vertices

            //Testing coordinates (origin)
            let testx = 0.0
            let testy = 0.0


            let mutable i = 0
            let mutable j = minkowskiDifference.Length - 1
            let mutable c = false

            //Check if the MDP contains the origin (pnpoly test by W. Randolph Franklin)
            while i < minkowskiDifference.Length do
                if( ((verty.[i] > testy) <> (verty.[j] > testy)) && (testx < (vertx.[j] - vertx.[i]) * (testy-verty.[i]) / (verty.[j] - verty.[i]) + vertx.[i]) ) then
                    c <- if c then false else true
                j <- i
                i <- i + 1

            if c then
                //There is an overlap thus we can find an intersection between the ray cast from the center of the MDP in the direction of the origin to determine penetration depth
                let rayStart = minkowskiCenter
                let rayEnd = (Vector2D.get_Zero() - minkowskiCenter) * 1000000.0//Multiplied to be certain the ray goes outside of the MDP


                let mDCopy = minkowskiDifference
                let index = ref -1
                let mDLines : (Vector2D * Vector2D) list = mDCopy |> List.map(fun x ->
                     index := !index + 1
                     if (!index < mDCopy.Length-1) then (x, mDCopy.[!index + 1]) else (x,mDCopy.[0]))

                let point = ref rayStart

                mDLines |> List.iter(fun x -> if lineIntersectionPoint(rayStart, rayEnd, fst x, snd x) <> rayStart then point := lineIntersectionPoint(rayStart, rayEnd, fst x, snd x))

                let penetrationDepth = abs((Vector2D.get_Zero() - !point).length())

                let collisionNormal = (Vector2D.get_Zero() - minkowskiCenter).unit()
                let xModifier = if (collisionNormal.x < 0.0) then -1.0 else 1.0
                let yModifier = if (collisionNormal.y < 0.0) then -1.0 else 1.0

                match collisionNormal.x < collisionNormal.y with
                    | true -> this.normal <- new Vector2D(1.0 * xModifier, 0.0)
                    | _ -> this.normal <- new Vector2D(0.0, 1.0 * yModifier)

                this.overlap <- true
                this.penetration <- penetrationDepth

                if this.A.immovable <> this.B.immovable && (this.A.name <> "None" && this.B.name <> "None") then
                    printfn " == COLLISION DATA == "

                    printfn " == BODY A [%s]= = " (this.A.name)
                    aVerts |> List.iter ( fun x -> printfn "%s" (x.ToString()))
                    printfn " == BODY B [%s] == " (this.B.name) 
                    bVerts |> List.iter ( fun x -> printfn "%s" (x.ToString()))

                    printfn " == CONVEX HULL POINTS == "
                    minkowskiDifference |> List.iter ( fun x -> printfn "%s" (x.ToString()))

                    printfn " == CONVEX HULL SEGMENTS == "
                    mDLines |> List.iter( fun x -> printfn "Start: %s || End: %s" (((fst)(x)).ToString()) (((snd)(x)).ToString()))

                    printfn " == CONVEX HULL CENTER == "
                    printfn "%s" (minkowskiCenter.ToString())

                    printfn " == INTERSECT POINT == "
                    printfn "Point: %s || Depth: %f " ((!point).ToString()) penetrationDepth

                    printfn " == COLLISION NORMAL == "
                    printfn "%s" (this.normal.ToString())

                    printfn " == GEOGEBRA SNAPSHOT == "

                    let commands = ref "Execute[{"

                    a.vertices |> List.iter( fun x -> commands := !commands + "\"(" + x.x.ToString() + ", " + x.y.ToString() + ")\",")
                    commands := !commands + "\"Polygon[A,B,D,C]\","

                    b.vertices |> List.iter( fun x -> commands := !commands + "\"(" + x.x.ToString() + ", " + x.y.ToString() + ")\",")
                    commands := !commands + "\"Polygon[E,F,H,G]\","

                    minkowskiDifference|> List.iter( fun x -> commands := !commands + "\"(" + x.x.ToString() + ", " + x.y.ToString() + ")\",")

                    match minkowskiDifference.Length with
                    | 8 -> commands := !commands + "\"ConvexHull[I,J,K,L,M,N,O,P]\", \"(" + minkowskiCenter.x.ToString() + " , " + minkowskiCenter.y.ToString() + ")\""
                    | 7 -> commands := !commands + "\"ConvexHull[I,J,K,L,M,N,O]\", \"(" + minkowskiCenter.x.ToString() + " , " + minkowskiCenter.y.ToString() + ")\""
                    | 6 -> commands := !commands + "\"ConvexHull[I,J,K,L,M,N]\", \"(" + minkowskiCenter.x.ToString() + " , " + minkowskiCenter.y.ToString() + ")\""
                    | 5 -> commands := !commands + "\"ConvexHull[I,J,K,L,M]\", \"(" + minkowskiCenter.x.ToString() + " , " + minkowskiCenter.y.ToString() + ")\""
                    | 4 -> commands := !commands + "\"ConvexHull[I,J,K,L]\", \"(" + minkowskiCenter.x.ToString() + " , " + minkowskiCenter.y.ToString() + ")\""
                    | _ -> commands := !commands + ""

                    commands := !commands + ", \"minkowskiCenter=(" + minkowskiCenter.x.ToString() + "," + minkowskiCenter.x.ToString() + ")\"}]"

                    printfn "%s" !commands
                    printfn "Paste into GeoGebra's command line to create the scene"

                    printfn " == END DATA SEGMENT == "


    end