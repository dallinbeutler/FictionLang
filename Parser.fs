namespace FS


open FParsec
open FParsec.Primitives
open FParsec.CharParsers
//open FS
open FParsec.Pipes

module Parser =

  let test parser text = 
      let state = 
        {values = dict[] 
         highlight = (fun ht p1 p2 ->() )
         highlightNoun = (fun p1 p2 str->() )
         highlightProp = (fun p1 p2 nounOption str->() )
         currentNoun = None 
        }
      runParserOnString parser state "test" text


  let comment = 
    %% ["//";"#"] -- +. restOfLine false 
    -|> Comment 
    |> Highlighting.highlight TokenType.HComment  

  let multiline= 
      %% "/*" -- +. manyCharsTill (anyChar<?> "comment block end") (pstring "*/" <??> "comment block end" )     
      -|> Comment
      |> Highlighting.highlight TokenType.HComment


  let comments = %[comment ; multiline] <?> "comment"
  
  let pQuotedString = 
    (between (%'"') (%'"') (charsTillString "\"" false System.Int32.MaxValue) )
    |> Highlighting.highlight TokenType.HString   

  let pName = 
    many1Chars ( letter <|> digit <??> "noun")

  let pNounName = 
    pName
    |>> NounName
    |> Highlighting.highlightNoun
    //|> Highlighting.highlightContext TokenType.HReference      
    

       


  let pFieldProp :Parser<PropName, ParserState> = 
      %% '.' -- +. (pName |> Highlighting.highlightProp) -- spaces -- "="
      -|> PropName <?> "prop name"

  let pFieldVal = 
      %[
          %% %'@' -- +. pNounName -|> PropVal.Ref 
             
          %p<float> |>> PropVal.Num   |> Highlighting.highlight TokenType.HNumber               
          
          pQuotedString |>> PropVal.Str |> Highlighting.highlight TokenType.HString

          
          //not sure what would be custom yet...
          //(restOfLine true |>> PropVal.Custom )
      ]   
      

  let pPropLine = 
      %% +. pFieldProp -- spaces -- +.  pFieldVal 
      -|> (fun p v-> {name = p; value = v} )
      <?> "prop value" 
      
  let pAssert =
      %% (%"assert" |> Highlighting.highlight TokenType.HAssert) -- spaces -- +. pPropLine -|> (Assertion)

  let propParse = 
      %[
          pAssert
          pPropLine |>> (Mutation)
          //comments
          //(newline <?>"blank line") >>% BlankLine <??>"blank line"
      ] <??> "property"


  let emptystuff = skipMany %[spaces1 <?> "empty space"; comments  <?> "empty space"|>>ignore ; eof <?> "empty space"]
  let emptystuff' = skipMany1 %[spaces1 <?> "empty space"; comments  <?> "empty space"|>>ignore ]
  
  let fullParse =
    many (%% emptystuff' -- +. (pNounName|>ParserStateChange.updateCurNoun) -- +. many (attempt( emptystuff' >>. propParse)) -|> (fun n list -> {name = n; items = list} ) )
         
     

  test fullParse ""


