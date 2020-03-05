
module SplitBy

open System.Collections.Generic

module List =
  let fromEnum (input : 'a IEnumerator) = 
      seq {
          while input.MoveNext() do
              yield input.Current
      }

  let getMore (input : 'a IEnumerator) = 
      if input.MoveNext() = false then None
      else Some ((input |> fromEnum |> Seq.toList) |> List.append [input.Current])
    
  let splitBy (f : 'a -> bool) (input : 'a seq)  = 
      
      use s = input.GetEnumerator()
      let rec loop (acc : 'a list list) = 
          match s |> getMore with 
          | None -> acc
          | Some x ->[x |> List.takeWhile (f >> not) |> Seq.toList ]
                     |> List.append acc
                     |> loop
      loop List.empty |> List.filter (List.isEmpty >> not)


module Seq =
  let fromEnum (input : 'a IEnumerator) = 
      seq {
          while input.MoveNext() do
              yield input.Current
      }

  let getMore (input : 'a IEnumerator) = 
      if input.MoveNext() = false then None
      else Some ((input |> fromEnum) |> Seq.append [input.Current])
    
  let splitBy (f : 'a -> bool) (input : 'a seq)  = 
      use s = input.GetEnumerator()
      let rec loop (acc : 'a seq seq) = 
          match s |> getMore with 
          | None -> acc
          | Some x ->[x |> Seq.takeWhile (f >> not) |> Seq.toList |> List.toSeq]
                     |> Seq.append acc
                     |> loop
      loop Seq.empty |> Seq.filter (Seq.isEmpty >> not)
