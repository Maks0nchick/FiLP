open System

let PI = 3.14

// Функция площади круга
let SCircle R = PI * R * R

// 1. Реализация через суперпозицию
let SCilindr_super R length = length * SCircle R

// 2. Реализация через каррирование
let SCilindr_curr R = fun length -> length * SCircle R

// Ввод данных
printf "Введите радиус круга: "
let radius = Double.Parse(Console.ReadLine())

printf "Введите высоту цилиндра: "
let height = Double.Parse(Console.ReadLine())

// Вычисления
let resultSuper = SCilindr_super radius height
let resultCurr = SCilindr_curr radius height

// Вывод результатов
printfn "Объём цилиндра (суперпозиция): %.2f" resultSuper
printfn "Объём цилиндра (каррирование): %.2f" resultCurr