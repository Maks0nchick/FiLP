open System

// 1. Обычная рекурсия (вверх)
let rec SumDigitsUp2 n =
    if n = 0 then 0
    else (n % 10) + SumDigitsUp2 (n / 10)

// 2. Хвостовая рекурсия (вниз)
let SumDigitsTail n =
    let rec loop n acc =
        if n = 0 then acc
        else loop (n / 10) (acc + n % 10)
    loop n 0

let main2() =
    printf "Введите число: "
    let number = Int32.Parse(Console.ReadLine())
    
    printfn "Сумма цифр (рекурсия вверх): %d" (SumDigitsUp2 number)
    printfn "Сумма цифр (хвостовая): %d" (SumDigitsTail number)

main2()