// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.


//#if true//INTERACTIVE
//#else
#r "C:\\Users\\111770\\.nuget\\packages\\fparsec\\1.1.1\\lib\\net45\\FParsecCS.dll"
#r "C:\\Users\\111770\\.nuget\\packages\\fparsec\\1.1.1\\lib\\net45\\FParsec.dll"
#r "C:\\Users\\111770\\.nuget\\packages\\fparsec-pipes\\1.1.1\\lib\\net45\\FParsec-Pipes.dll"

//#load "SplitBy.fs"
//#endif
#load "Ast.fs"
#load "Parser.fs"

open FS.Parser

let fullTest = """
 
 
  //Dougs a foool

  doug
  .weight = 200 // kinda fat
  .friend = @Joanne //she can't be trusted
  //dougs a bit of a loser

  Jamal
  assert .catchphrase = "wowsers" 
  .fat = 1"""

//open GDFS
test 
// comment "//furrk me"
// multiline "/*furrk me\n*/"
// pQuotedString "\"Doug sucks\"  "
// pNounName "sdfsdf "
// pFieldProp ".funky =" 
// pFieldVal "\"53  \n\"  
// pPropLine ".Dougery = 57"
// pAssert "assert .funky = 57"
// propParse ".funky = 57"
 fullParse fullTest

 