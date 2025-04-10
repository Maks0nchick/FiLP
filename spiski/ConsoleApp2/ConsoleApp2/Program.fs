let longestCommonSubsequence (xs: 'a list) (ys: 'a list) =
    let rec lcs x y =
        match x, y with
        | [], _ -> []
        | _, [] -> []
        | xh::xt, yh::yt when xh = yh -> xh :: (lcs xt yt)
        | xh::xt, yh::yt ->
            let l1 = lcs (xh::xt) yt
            let l2 = lcs xt (yh::yt)
            if List.length l1 > List.length l2 then l1 else l2
    lcs xs ys

// Пример использования:
// longestCommonSubsequence [1;2;3;4;5] [2;4;6;8] -> [2;4]

let transformList (lst: int list) =
    let list1 = lst |> List.filter (fun x -> x % 2 = 0) |> List.map (fun x -> x / 2)
    let list2 = lst |> List.filter (fun x -> x % 3 = 0) |> List.map (fun x -> x / 3)
    let list3 = list2 |> List.map (fun x -> x * x)
    let list4 = list3 |> List.filter (fun x -> List.contains x list1)
    let list5 = list2 @ list3 @ list4
    (list1, list2, list3, list4, list5)

// Пример использования:
// transformList [1..10] -> ([1; 2], [1], [1], [], [1; 1])

let gcd a b = 
    let rec gcd' a b = if b = 0 then a else gcd' b (a % b)
    gcd' (abs a) (abs b)

let findTuples N =
    let divisors = [1..N] |> List.filter (fun x -> N % x = 0)
    [for x in divisors do
        for y in divisors do
            if x * y = N then
                let d = gcd x y
                yield (x/d, y/d)]
    |> List.distinct

// Пример использования:
// findTuples 12 -> [(1, 12); (2, 6); (3, 4); (4, 3); (6, 2); (12, 1)]

let isPythagoreanTriple (a, b, c) = a*a + b*b = c*c

let findPythagoreanTriples lst =
    [for a in lst do
        for b in lst do
            for c in lst do
                if a < b && b < c && isPythagoreanTriple (a, b, c) then
                    yield (a, b, c)]

// Пример использования:
// findPythagoreanTriples [3;4;5;6;8;10] -> [(3, 4, 5); (6, 8, 10)]

let isPrime n =
    if n <= 1 then false
    else
        let sqrtN = int (sqrt (float n))
        [2..sqrtN] |> List.forall (fun x -> n % x <> 0)

let primeFactors n =
    let rec factors n div acc =
        if n = 1 then acc
        elif n % div = 0 then factors (n/div) div (div::acc)
        elif div = 2 then factors n 3 acc
        else factors n (div + 2) acc
    if n = 1 then [] else factors n 2 [] |> List.rev

let elementsWithAllPrimeDivisors lst =
    let allPrimes = lst |> List.collect primeFactors |> List.distinct
    lst |> List.filter (fun x -> 
        let xPrimes = primeFactors x
        xPrimes |> List.forall (fun p -> List.contains p allPrimes))

// Пример использования:
// elementsWithAllPrimeDivisors [2;3;4;5;6;7;8;9;10] -> [2;3;4;5;6;7;8;9;10]


let filterAndSortTuples tuples =
    let isAllDigits (t: int*int*int*int*int) =
        let (a,b,c,d,e) = t
        [a;b;c;d;e] |> List.forall (fun x -> x >= 0 && x <= 9)
    
    tuples
    |> List.filter isAllDigits
    |> List.sort
    |> List.map (fun (a,b,c,d,e) -> a*10000 + b*1000 + c*100 + d*10 + e)

// Пример использования:
// filterAndSortTuples [(7,3,4,5,6);(2,3,4,6,7);(2,3,4,5,6);(4,3,10,4,5)] -> [23456; 23467; 73456]


let sumDivisors n =
    if n = 1 then 1
    else [1..n/2] |> List.filter (fun x -> n % x = 0) |> List.sum

let customSort lst =
    let avg = List.averageBy float lst
    let evenPosElements = lst |> List.indexed |> List.filter (fun (i,_) -> i % 2 = 1) |> List.map snd
    let divisorsOfEvenPos = evenPosElements |> List.collect (fun x -> [1..x] |> List.filter (fun d -> x % d = 0)) |> List.distinct
    
    let p a =
        let divisors = [1..a] |> List.filter (fun d -> a % d = 0)
        divisors 
        |> List.filter (fun d -> List.contains d divisorsOfEvenPos && lst |> List.filter (fun x -> x < avg) |> List.forall (fun x -> x % d <> 0))
        |> List.sum
    
    lst |> List.sortBy p

// Пример использования:
// customSort [12; 5; 8; 3; 10] -> [3; 5; 8; 10; 12]


let digitAverage lst =
    let digits = lst |> List.collect (fun x -> x.ToString().ToCharArray() |> Array.map (fun c -> int c - int '0') |> Array.toList)
    let digitCounts = digits |> List.countBy id |> Map.ofList
    let totalDigits = List.length digits
    let frequentDigits = digitCounts |> Map.filter (fun _ count -> count > totalDigits / 2) |> Map.toList |> List.map fst
    
    lst |> List.map (fun num ->
        let numDigits = num.ToString().ToCharArray() |> Array.map (fun c -> int c - int '0') |> Array.toList
        let filtered = numDigits |> List.filter (fun d -> List.contains d frequentDigits)
        if List.isEmpty filtered then 0 else List.averageBy float filtered |> int)

// Пример использования:
// digitAverage [745; 123; 456] -> [4; 0; 4] (при определенных условиях)


let complexFilter lst =
    let sumPrevious index = lst.[0..index-1] |> List.sum
    let isPerfectSquare x = let sqrtX = sqrt (float x) in sqrtX = floor sqrtX && List.contains (int sqrtX) lst
    
    let filtered = 
        lst
        |> List.indexed
        |> List.filter (fun (i, x) -> 
            x > sumPrevious i && 
            isPerfectSquare x && 
            lst.[0..i-1] |> List.forall (fun y -> x % y = 0))
        |> List.map snd
    
    let countGreaterThan x = filtered |> List.filter (fun y -> y > x) |> List.length
    
    filtered 
    |> List.map (fun x -> (x, sumPrevious (List.findIndex ((=) x) lst), countGreaterThan x))

// Пример использования:
// complexFilter [1; 4; 2; 16; 3] -> [(4, 1, 1); (16, 23, 0)]


let specialLists lst =
    let indexed = lst |> List.indexed
    
    let list2 =
        indexed
        |> List.filter (fun (i, x) -> 
            indexed 
            |> List.exists (fun (j, a) -> 
                indexed 
                |> List.exists (fun (k, b) -> j <> k && j <> i && k <> i && a * b = x)))
        |> List.map fst
    
    let list3 =
        indexed
        |> List.filter (fun (i, x) -> 
            indexed 
            |> List.exists (fun (j, a) -> 
                indexed 
                |> List.exists (fun (k, b) -> 
                    indexed 
                    |> List.exists (fun (l, c) -> 
                        j <> k && k <> l && j <> l && 
                        j <> i && k <> i && l <> i && 
                        a + b + c = x))))
        |> List.map fst
    
    let list4 =
        indexed
        |> List.filter (fun (i, x) -> 
            lst 
            |> List.filter (fun y -> x % y = 0) 
            |> List.length = 4)
        |> List.map fst
    
    (list2, list3, list4)

// Пример использования:
// specialLists [1;2;3;4;5;6;7;8;9;10] -> ([6;8;9;10], [6;7;8;9;10], [6;8;9;10])

