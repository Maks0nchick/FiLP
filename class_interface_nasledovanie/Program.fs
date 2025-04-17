// Интерфейс IPrint
type IPrint =
    abstract member Print: unit -> unit

// Абстрактный класс Геометрическая фигура
[<AbstractClass>]
type GeometricFigure() =
    abstract member Area: unit -> float
    // Базовая реализация ToString()
    override this.ToString() = "Фигура"

// Класс Прямоугольник
type Rectangle(width: float, height: float) =
    inherit GeometricFigure()
    member val width = width with get, set
    member val height = height with get, set
    override this.Area() = this.width * this.height
    override this.ToString() =
        $"Прямоугольник [ширина={this.width}, высота={this.height}, площадь={this.Area()}]"
    
    // Реализация интерфейса IPrint
    interface IPrint with
        member this.Print() = printfn "%O" this

// Класс Квадрат
type Square(length: float) = 
    inherit Rectangle(length, length)
    override this.ToString() =
        $"Квадрат [сторона={this.width}, площадь={this.Area()}]"
    
    // Реализация интерфейса IPrint
    interface IPrint with
        member this.Print() = printfn "%O" this

// Класс Круг
type Circle(radius: float) = 
    inherit GeometricFigure()
    let PI = 3.14
    member val radius = radius with get, set
    member this.Pi = PI
    override this.Area() = (this.radius * this.radius) * PI
    override this.ToString() =
        $"Круг [радиус={this.radius}, PI={this.Pi}, площадь={this.Area()}]"
    
    // Реализация интерфейса IPrint
    interface IPrint with
        member this.Print() = printfn "%O" this

[<EntryPoint>]
let main argv =
    let figures: IPrint[] = [|
        Rectangle(4.0, 3.0) :> IPrint
        Square(5.0) :> IPrint
        Circle(5.0) :> IPrint
    |]
    
    // Вызов метода Print() для всех фигур
    figures |> Array.iter (fun fig -> fig.Print())
    
    0