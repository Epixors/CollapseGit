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

            //Create the Minkowski Difference
            for aVert in aVerts do
                for bVert in bVerts do
                    minkowskiDifference <- (aVert - bVert)::minkowskiDifference

            

            let computeCentroid (points: Vector2D list) =       
                let c = List.fold(fun acc x -> acc + x) (Vector2D.get_Zero()) points
                c/(float)points.Length



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

                //Determines where intersection happens on two line segments l and m defined by A-B and C-D
                let intersectSegments(l:(Vector2D * Vector2D * int), m:(Vector2D * Vector2D * int)) =
                    let mutable A, B, _ = l
                    let mutable C, D, _ = m

                    let standard = (new Vector2D(System.Double.MinValue, System.Double.MaxValue), l, m)

                    if A.x = B.x && A.y = B.y || C.x = D.x && C.y = D.y then
                        standard
                    else 
                        B <- B - A
                        C <- C - A
                        D <- D - A

                        let distAB = B.length()

                        let cos = B.x/distAB
                        let sin = B.y/distAB

                        let newCX = C.x * cos + C.y * sin
                        C <- new Vector2D(newCX, C.y * cos - C.x * sin)
                        let newDX = D.x * cos + D.y * sin
                        D <- new Vector2D(newDX, D.y * cos - D.x * sin)

                        if C.y < 0.0 && D.y < 0.0 || C.y >= 0.0 && D.y >= 0.0 then
                            standard
                        else
                            let ABpos = D.x + (C.x - D.x) * D.y / (D.y - C.y)
                            if ABpos < 0.0 || ABpos > distAB then
                                standard
                            else
                                (new Vector2D(A.x + ABpos * cos, A.y + ABpos * sin), l, m)

                //Creates a collection of line segments for body A
                let aIndex = ref -1
                let aLines : (Vector2D * Vector2D * int) list = aVerts |> List.map(fun x ->
                     aIndex := !aIndex + 1
                     if (!aIndex < aVerts.Length-1) then (x, aVerts.[!aIndex + 1], !aIndex) else (x,aVerts.[0], !aIndex))

                //Creates a collection of line segments for body B
                let bIndex = ref -1
                let bLines : (Vector2D * Vector2D * int) list = bVerts |> List.map(fun x ->
                     bIndex := !bIndex + 1
                     if (!bIndex < bVerts.Length-1) then (x, bVerts.[!bIndex + 1], !bIndex) else (x,bVerts.[0], !bIndex))

                //Stores found intersections
                let intersects = ref []

                aLines |> List.iter(fun x -> bLines |> List.iter(fun y -> intersects :=  (intersectSegments(x, y))::!intersects ))

                let standard = new Vector2D(System.Double.MinValue, System.Double.MaxValue)

                //Filters out the non-intersections
                intersects := !intersects |> List.filter(fun x ->
                    let a, _, _ = x
                    a <> standard)

                //Stores the lines in body A that are intersecting with body B
                let mutable aLines = []
               
                //Fills aLines with the lines from found intersections
                for intersect in !intersects do
                  (*
                    3 ---- 2 ---- 2
                    |             |
                    |             |
                    3             1             
                    |             |
                    |             |
                    0 ---- 0 ---- 1
                  *)

                    //Determines which line is intersecting
                    let intersectPoint, (_,_,lineAIndex), (_, _,lineBIndex) = intersect
                    let lineA = match lineAIndex with
                                    | 0 -> (aVerts.[1] , aVerts.[0], 0)
                                    | 1 -> (aVerts.[2] , aVerts.[1], 0)
                                    | 2 -> (aVerts.[3] , aVerts.[2], 0)
                                    | 3 -> (aVerts.[3] , aVerts.[0], 0)

                    printfn "A%i intersected with B%i" lineAIndex lineBIndex

                   
                    aLines <- lineA::aLines

                let standardTwo = (standard, (standard, standard, 0), (standard, standard, 0))
                let mutable endPoint, _, _ = if aLines.Length >= 2 then intersectSegments(aLines.[0], aLines.[1]) else standardTwo

                if aLines.Length >= 2 && endPoint = standard then
                    let endPointTwo,_,_ = intersectSegments(aLines.[1], aLines.[0])
                    endPoint <- endPointTwo


                let collisionNormal = (Vector2D.get_Zero() - minkowskiCenter).unit()
                let xModifier = if (collisionNormal.x < 0.0) then -1.0 else 1.0
                let yModifier = if (collisionNormal.y < 0.0) then -1.0 else 1.0

                match collisionNormal.x < collisionNormal.y with
                    | true -> this.normal <- new Vector2D(1.0 * xModifier, 0.0)
                    | _ -> this.normal <- new Vector2D(0.0, 1.0 * yModifier)

                this.overlap <- true
                this.penetration <- 0.01

                if this.A.immovable <> this.B.immovable && (this.A.name <> "None" && this.B.name <> "None") then
                    printfn " == COLLISION DATA == "

                    printfn " == BODY A [%s]= = " (this.A.name)
                    aVerts |> List.iter ( fun x -> printfn "%s" (x.ToString()))
                    printfn " == BODY B [%s] == " (this.B.name) 
                    bVerts |> List.iter ( fun x -> printfn "%s" (x.ToString()))

                    printfn " == CONVEX HULL POINTS == "
                    minkowskiDifference |> List.iter ( fun x -> printfn "%s" (x.ToString()))

                    printfn " == CONVEX HULL CENTER == "
                    printfn "%s" (minkowskiCenter.ToString())

                    printfn " == INTERSECT POINT == "
                    !intersects |> List.iter(fun x -> printfn "%s" (x.ToString()))
                    printfn "End Point: %s" (endPoint.ToString())

                    printfn " == COLLISION NORMAL == "
                    printfn "%s" (this.normal.ToString())

                    printfn " == GEOGEBRA SNAPSHOT == "

                    let commands = ref "Execute[{"

                    aVerts |> List.iter( fun x -> commands := !commands + "\"(" + x.x.ToString() + ", " + x.y.ToString() + ")\",")
                    commands := !commands + "\"Polygon[A,B,D,C]\","

                    bVerts |> List.iter( fun x -> commands := !commands + "\"(" + x.x.ToString() + ", " + x.y.ToString() + ")\",")
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