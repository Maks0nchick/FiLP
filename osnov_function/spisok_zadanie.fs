// ==================== Задание 6 ====================
printfn "\nЗадание 6: Функция, возвращающая функцию"
let getOperation flag =
    match flag with
    | true -> 
        let sumDigits n =
            let rec loop num acc =
                match num with
                | 0 -> acc
                | _ -> loop (num / 10) (acc + num % 10)
            loop (abs n) 0
        sumDigits
    | false -> 
        let rec factorial n =
            match n with
            | 0 | 1 -> 1
            | _ -> n * factorial (n - 1)
        factorial

let sumFunc = getOperation true
let factFunc = getOperation false
printfn "Сумма цифр 12345: %d (ожидается 15)" (sumFunc 12345)
printfn "Факториал 5: %d (ожидается 120)" (factFunc 5)

// ==================== Задание 7 ====================
printfn "\nЗадание 7: Обход цифр с операцией"
let digitOperation number operation init =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ -> 
            let digit = n % 10
            loop (n / 10) (operation acc digit)
    loop (abs number) init

printfn "Сумма цифр 35214: %d (ожидается 15)" (digitOperation 35214 (+) 0)
printfn "Произведение цифр 123: %d (ожидается 6)" (digitOperation 123 (*) 1)
printfn "Максимальная цифра 491: %d (ожидается 9)" (digitOperation 491 max 0)

// ==================== Задание 8 ====================
printfn "\nЗадание 8: Тестирование с лямбдами"
let testNumber = 35214
printfn "Сумма квадратов цифр 35214: %d (ожидается 55)" 
    (digitOperation testNumber (fun acc x -> acc + x*x) 0)
printfn "Количество цифр в 35214: %d (ожидается 5)" 
    (digitOperation testNumber (fun acc _ -> acc + 1) 0)

// ==================== Задание 9 ====================
printfn "\nЗадание 9: Обход с условием"
let digitOperationWithCondition number operation init condition =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ ->
            let digit = n % 10
            let newAcc = if condition digit then operation acc digit else acc
            loop (n / 10) newAcc
    loop (abs number) init

printfn "Сумма чётных цифр 352814: %d (ожидается 10)" 
    (digitOperationWithCondition 352814 (+) 0 (fun x -> x % 2 = 0))
printfn "Произведение цифр >3 в 352814: %d (ожидается 280)" 
    (digitOperationWithCondition 352814 (*) 1 (fun x -> x > 3))

// ==================== Задание 10 ====================
printfn "\nЗадание 10: Тестирование на 3 примерах"
let number = 5736251
printfn "1. Количество цифр 5 в 5736251: %d (ожидается 2)" 
    (digitOperationWithCondition number (fun acc _ -> acc + 1) 0 (fun x -> x = 5))
printfn "2. Сумма цифр >4 в 5736251: %d (ожидается 21)" 
    (digitOperationWithCondition number (+) 0 (fun x -> x > 4))
printfn "3. Минимальная нечётная цифра в 5736251: %d (ожидается 1)" 
    (digitOperationWithCondition number min 9 (fun x -> x % 2 <> 0))