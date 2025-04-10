// Дополнительные сведения о F# см. на http://fsharp.net
// Дополнительную справку см. в проекте "Учебник по F#".

// Задание 1: Чтение списка с клавиатуры
let rec readList n = 
    if n = 0 then []
    else
        let Head = System.Convert.ToInt32(System.Console.ReadLine())
        let Tail = readList (n-1)
        Head::Tail

let readData = 
    printfn "Введите количество элементов:"
    let n = System.Convert.ToInt32(System.Console.ReadLine())
    printfn "Введите элементы:"
    readList n

// Задание 2: Вывод списка
let rec writeList = function
    [] -> 
        let z = System.Console.ReadKey()
        0
    | (head : int)::tail -> 
        printfn "%d" head
        writeList tail

// Задание 3: Общая функция свертки с фильтрацией
let rec foldFilter f p acc = function
    | [] -> acc
    | h::t when p h -> foldFilter f p (f acc h) t
    | _::t -> foldFilter f p acc t

// Задание 4: Специализированные функции
// Минимальный элемент
let listMin list = 
    foldFilter min (fun _ -> true) System.Int32.MaxValue list

// Сумма четных элементов
let sumEven list = 
    foldFilter (+) (fun x -> x % 2 = 0) 0 list

// Количество нечетных элементов
let countOdd list = 
    foldFilter (fun acc _ -> acc + 1) (fun x -> x % 2 <> 0) 0 list

// Задание 5: Самый частый элемент
let mostFrequent list =
    list
    |> List.countBy id
    |> List.maxBy snd
    |> fst

// Задание 6: Двоичное дерево со строковыми элементами
type 'a btree = 
    | Node of 'a * 'a btree * 'a btree
    | Nil

// Задание 7: Самый частый элемент (с использованием List)
let mostFrequentList list =
    list
    |> List.groupBy id
    |> List.map (fun (x, xs) -> (x, List.length xs))
    |> List.maxBy snd
    |> fst

// Задание 8: Элементы, которые могут быть квадратом другого элемента
let countSquareElements list =
    let squares = list |> List.map (fun x -> x * x) |> Set.ofList
    list 
    |> List.filter (fun x -> Set.contains x squares)
    |> List.length

// Задание 9: Создание кортежей по особым правилам
let createTuples list1 list2 list3 =
    let sorted1 = list1 |> List.sortDescending
    let sorted2 = list2 |> List.sortBy (fun x -> x.ToString().ToCharArray() |> Array.sumBy (fun c -> int c - int '0'))
    let sorted3 = list3 |> List.sortByDescending (fun x -> [1..x] |> List.filter (fun d -> x % d = 0) |> List.length)
    List.zip3 sorted1 sorted2 sorted3

// Задание 10: Сортировка строк по длине
let sortStringsByLength (strings: string list) =
    strings |> List.sortBy (fun s -> s.Length)

[<EntryPoint>]
let main argv = 
    printfn "Демонстрация работы функций:"
    
    // Тестирование
    let testList = [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5]
    printfn "\nТестовый список: %A" testList
    
    printfn "\nМинимальный элемент: %d" (listMin testList)
    printfn "Сумма четных элементов: %d" (sumEven testList)
    printfn "Количество нечетных элементов: %d" (countOdd testList)
    printfn "Самый частый элемент: %d" (mostFrequent testList)
    
    let squareTest = [1; 2; 3; 4; 9; 16; 5]
    printfn "\nКоличество элементов, являющихся квадратами: %d" (countSquareElements squareTest)
    
    let strings = ["яблоко"; "груша"; "апельсин"; "банан"]
    printfn "\nСтроки, отсортированные по длине: %A" (sortStringsByLength strings)
    
    0 // возвращение целочисленного кода выхода


