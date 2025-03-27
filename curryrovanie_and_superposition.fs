let rec Sum x sumC=
    if x>0
    then Sum (x/10) (sumC+x%10)
    else sumC
Sum 257 0

open System

// Рекурсивная функция (рекурсия вверх)
let rec SumDigitsUp n =
    if n = 0 then 0
    else (n % 10) + SumDigitsUp (n / 10)

// Чтение числа и вывод результата
let main() =
    printf "Введите целое число: "
    let number = Int32.Parse(Console.ReadLine())
    let sum = SumDigitsUp number
    printfn "Сумма цифр числа %d: %d" number sum

// Запуск
main()