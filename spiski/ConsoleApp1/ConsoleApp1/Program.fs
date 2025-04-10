// Вспомогательные функции для ввода данных
let rec readList n = 
    if n=0 then []
    else
    let Head = System.Convert.ToInt32(System.Console.ReadLine())
    let Tail = readList (n-1)
    Head::Tail

let readData = 
    let n=System.Convert.ToInt32(System.Console.ReadLine())
    readList n

// Задача 1.7 - Циклический сдвиг списка на (n-2) элементов вправо
let task1_7_church list =
    let rec shift lst acc n =
        match lst, n with
        | [], _ -> acc
        | h::t, _ when n > 0 -> shift t (acc @ [h]) (n - 1)
        | h::t, _ -> shift t (acc @ [h]) 0
    let len = List.length list
    let n = len - 2
    let (_, secondPart) = List.splitAt n list
    secondPart @ (List.take n list)

let task1_7_list list =
    let len = List.length list
    let n = len - 2
    let second = List.skip n list
    let first = List.take n list
    second @ first

// Пример вызова:
let list1_7 = [1; 2; 3; 4; 5]
printfn "Task 1.7 Church: %A" (task1_7_church list1_7) 
printfn "Task 1.7 List: %A" (task1_7_list list1_7)     

// Задача 1.17 - Поменять местами минимальный и максимальный элементы
let task1_17_church list =
    let rec findMinMax lst idx (minV, minI, maxV, maxI) =
        match lst with
        | [] -> (minI, maxI)
        | h::t ->
            let (minV, minI) = if h < minV then (h, idx) else (minV, minI)
            let (maxV, maxI) = if h > maxV then (h, idx) else (maxV, maxI)
            findMinMax t (idx + 1) (minV, minI, maxV, maxI)
    let (minI, maxI) = findMinMax list 0 (System.Int32.MaxValue, -1, System.Int32.MinValue, -1)
    list |> List.mapi (fun i x ->
        if i = minI then List.item maxI list
        elif i = maxI then List.item minI list
        else x)

let task1_17_list list =
    let minVal = List.min list
    let maxVal = List.max list
    list |> List.map (fun x ->
        if x = minVal then maxVal
        elif x = maxVal then minVal
        else x)

// Пример вызова:
let list1_17 = [5; 3; 8; 1; 4]
printfn "Task 1.17 Church: %A" (task1_17_church list1_17) 
printfn "Task 1.17 List: %A" (task1_17_list list1_17)     

// Задача 1.27 - Циклический сдвиг влево на 1 элемент
let task1_27_church list =
    match list with
    | [] -> []
    | h::t -> t @ [h]

let task1_27_list list =
    List.tail list @ [List.head list]

// Пример вызова:
let list1_27 = [1; 2; 3; 4]
printfn "Task 1.27 Church: %A" (task1_27_church list1_27) 
printfn "Task 1.27 List: %A" (task1_27_list list1_27)    

// Задача 1.37 - Найти индексы элементов, меньших предыдущего
let task1_37_church list =
    let rec helper lst idx prev acc =
        match lst with
        | [] -> (List.rev acc, List.length acc)
        | h::t ->
            if h < prev then helper t (idx + 1) h (idx::acc)
            else helper t (idx + 1) h acc
    match list with
    | [] | [_] -> ([], 0)
    | h::t -> helper t 1 h []

let task1_37_list list =
    list
    |> List.mapi (fun i x -> (i, x))
    |> List.skip 1
    |> List.filter (fun (i, x) -> x < List.item (i - 1) list)
    |> fun l -> (List.map fst l, List.length l)

// Пример вызова:
let list1_37 = [5; 3; 8; 6; 4; 2]
printfn "Task 1.37 Church: %A" (task1_37_church list1_37) 
printfn "Task 1.37 List: %A" (task1_37_list list1_37)     

// Задача 1.47 - Найти все делители всех элементов списка (уникальные)
let task1_47_church list =
    let rec divisors n d acc =
        if d > n then acc
        elif n % d = 0 then divisors n (d + 1) (d::acc)
        else divisors n (d + 1) acc
    let rec collect lst acc =
        match lst with
        | [] -> acc
        | h::t -> collect t (List.append (divisors h 1 []) acc)
    collect list [] |> Set.ofList |> Set.toList

let task1_47_list list =
    list
    |> List.collect (fun n -> [1..n] |> List.filter (fun d -> n % d = 0))
    |> Set.ofList
    |> Set.toList

// Пример вызова:
let list1_47 = [6; 8; 10]
printfn "Task 1.47 Church: %A" (task1_47_church list1_47) 
printfn "Task 1.47 List: %A" (task1_47_list list1_47)     

// Задача 1.57 - Количество элементов, больших суммы предыдущих
let task1_57_church list =
    let rec helper lst sum acc =
        match lst with
        | [] -> acc
        | h::t ->
            if h > sum then helper t (sum + h) (acc + 1)
            else helper t (sum + h) acc
    helper list 0 0

let task1_57_list list =
    let _, count =
        list
        |> List.fold (fun (sum, acc) x ->
            if x > sum then (sum + x, acc + 1)
            else (sum + x, acc)) (0, 0)
    count

// Пример вызова:
let list1_57 = [1; 2; 5; 3; 8; 6]
printfn "Task 1.57 Church: %d" (task1_57_church list1_57) 
printfn "Task 1.57 List: %d" (task1_57_list list1_57)    