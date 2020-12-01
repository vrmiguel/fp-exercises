-- addDigit adds a single digit to the right-hand end of an arbitrary-size integer
addDigit :: Int -> Int -> Int
-- assumes digit is bigger than -1 and smaller than 10
addDigit num digit = (num * 10) + digit

-- Given the type synonym type Vertex = ( Float, Float ) write a function  distance :: Vertex -> Vertex -> Float that will 
-- calculate the distance between two points, each represented as a Vertex.  Using the distance function write a function triArea :: Vertex -> Vertex -> Vertex -> Float that 
-- will calculate the area of the triangle formed by the three given vertices. 

type Vertex = (Float, Float)

vertexX :: Vertex -> Float
vertexX (x, _) = x

vertexY :: Vertex -> Float
vertexY (_, y) = y

distance :: Vertex -> Vertex -> Float
distance vA vB = 
    let xi = vertexX vA
        xf = vertexY vA
        yi = vertexX vB
        yf = vertexY vB
    in sqrt ((xf - xi) ** 2.0 + (yf - yi) ** 2.0)

