module Synthesis

let abelar input =
        (input > 12) && (input < 3097) && ((input%12)= 0 )
       
        
let area l h = 
         match ((l >= 0.0 )&& (h >= 0.0)) with
         |true -> (l * h)/2.0            
         |false -> failwith "Either the height or base less than zero"
                  

let zollo input =
            match input < 0 with
            |true -> (input * (-input))/(input)
            |false -> input*2

        
let min a b =
            match a < b with
            |true -> a
            |false -> b


let max a b =
            match a < b with
            |true -> b
            |false -> a
    
let ofTime a b c =  
          (a*60*60) + (b*60) + c

let toTime seconds =
        match (seconds < 0) with
        |true-> (0,0,0)
        |false ->
        let a =seconds/3600
        let b= (seconds%3600)/60
        let c=((seconds%3600)%60)
        (a,b,c)

let digits number = 
        match (number < 0) with
        |true-> String.length(string(-number))
        |false ->String.length(string(number))


let minmax (a,b,c,d) =
      let big = max a b |> max c |>max d
      let small = min a b |> min c |>min d
      (small,big)

let isLeap year =
     match (year >= 1582) with
        |true->  match (year % 4 = 0) && (year%100 <> 0) with
                  |true -> true
                  |false ->  (year % 100 = 0) && (year % 400 = 0)                  
        |false -> failwith "year is less than 1582"
     
let month int = 
       let january = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30,31]
       let febuary = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28]
       let march = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30,31]
       let april = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30]
       let may = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30,31]
       let june = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30]
       let july = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30,31]
       let august = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30,31]
       let september = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30]
       let october = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30,31]
       let november = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30]
       let december = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24.25,26,27,28,29,30,31]
       let lst = ["january","febuary","march","april","may","june","july","august","september","october","november","december"]
       match (int < 1)||(int > 12) with 
       |true -> failwith " int is less  1 or greater than  12"
       |false -> (lst.[int]) 

let rec s number =
       match (number = 0) with 
       |true -> "0"
       |false ->
       match (number < 0) with 
                |true -> failwith "int is negative"
                |false -> 
                 let rec g number acc:string  = match ((number/2) > 0) with
                                                |true ->  g  (number/2)  (string(number%2)+ acc)
                                                |false -> (string(number % 2) + acc)
                 string(g)
let rec toBinary integer =
  match (integer < 0) with 
                |true -> failwith "int is negative"
                |false -> 
                match integer with
                | 0 | 1 -> string integer
                | _ ->
                 let binary = string (integer % 2)
                 (toBinary (integer / 2)) + binary

let bizFuzz int =
          match (int < 1) with 
          |true -> (0,0,0)
          |false -> ((int/3),(int/5),(int/(5*3)))
         
let monthDay _ _ =
    failwith "Not implemented"

let sgrt n =
   let rec calculate guess i=
     match i with 
     |10 -> guess
     |_ -> 
     let g = (guess + n/guess) /2.0
     calculate g (i+1 )
   match n<= 0.0 with 
    |true -> failwith "Imposible"
    |_-> calculate (n/2.0) 0


let coord _ =
    failwith "Not implemented"