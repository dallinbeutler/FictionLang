namespace FS
open System.Collections.Generic
open FParsec



type NounName = NounName of string
type PropName = PropName of string
type Comment  = Comment of string
//type Access =
//|Noun of NounName
//|Prop of NounName * =

type PropVal = 
|Str of string
|Ref of NounName
|Num of float
|Custom of string

//type Comment = string
type Property = {
        name:PropName 
        value:PropVal
        }

type NounAttribute = 
|Mutation of Property
|Assertion of Property


//type AST = 
//    |NounLine of NounName
//    |Comment of string
    //|BlankLine
    //|NounAtt of NounAttribute
type QualifiedNoun = {
  name: NounName
  items: NounAttribute list
}

type TokenType =
|HComment
|HAssert
|HReference
|HString
|HNumber
//|HError of string

type ParserState = {
    values: IDictionary<NounName,IDictionary<PropName,PropVal>>
    //errors: codeError list
    currentNoun: NounName option
    highlight: TokenType -> Position -> Position ->unit
    highlightNoun: Position -> Position -> NounName -> unit
    highlightProp: Position -> Position ->NounName option -> string -> unit
    //reEvalParagraph: int -> unit
}

module ParserStateChange =
  // standard syntax for writing a parser. Still not sure how to write one that works without (|>) operator...
  let updateCurNoun (p: Parser<NounName,ParserState>) :Parser<NounName,ParserState> =
    fun stream ->
      let reply = p stream
      stream.UserState <- {stream.UserState with currentNoun = Some reply.Result}
      reply


/////////////////
//Syntax Highlighting n such
/////////////////
module Highlighting = 
  //The idea is to get the start and end position in the code for the highlighter (we're injecting it via the func in ParserState)
  let highlight hType parser =
    let highlightIt (left:Position) content (right:Position) (state:ParserState) =
        state.highlight hType left right
        content
    pipe4 getPosition parser getPosition getUserState highlightIt
  //What if we want a little more context? this could provide intellisense since we're handing the string over
  let highlightNoun parser=
    let highlightIt (left:Position) content (right:Position) (state:ParserState) =
      state.highlightNoun left right (content)
      content
    pipe4 getPosition parser getPosition getUserState highlightIt
  //Repeated code. smelly
  let highlightProp parser=
    let highlightIt (left:Position) content (right:Position) (state:ParserState) =
      state.highlightProp left right state.currentNoun content
      content
    pipe4 getPosition parser getPosition getUserState highlightIt

module ParserValidation =
  
  //CAUTION: this assumes you've already checked for a valid noun and have one
  //Run validation on 'assert' line
  let checkAssertion (assertion:Property)  (state:ParserState): Result<_,string>=    

    match state.values.TryGetValue state.currentNoun.Value with
    | true, props ->
      match props.TryGetValue assertion.name with
      |true,propVal -> match assertion.value = propVal with
                       |true -> Result.Ok ()
                       |_ -> Result.Error "value does not match"
      |_ -> Result.Error (sprintf "property doesn't exist on noun %A. (yet?)" (unbox state.currentNoun.Value))
    | _ -> Result.Error (sprintf "noun %A doesn't exist! (yet?)" (unbox state.currentNoun.Value))
  
  //If noun doesn't exist, create it
  let checkCreateNoun (noun:NounName) (state:ParserState): ParserState =
    if state.values.ContainsKey noun |> not
    then state.values.Add(noun, dict[])
    {state with currentNoun = Some noun}
    

  //CAUTION: this assumes you've already checked for a valid noun and have one
  //Assign property value 
  let assignVal (prop:Property)  (state:ParserState): ParserState = //Result<ParserState,string> =
    state.values.[state.currentNoun.Value].Add(prop.name,prop.value)    
    state