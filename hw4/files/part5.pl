% List -> list of numbers | Right -> Left to Right | 
% mergeEq calculates the possible equations 

% splitLists :- splits the list List into 2 parts:  List1 and List2

splitLists(List,List1,List2) :- append(List1,List2,List), List1 = [_|_], List2 = [_|_].

mergeEq(List,Left,Right) :-
   splitLists(List,Left1,Right1),              
   term(Left1,Left),                 
   term(Right1,Right),                
   Left =:= Right.                 

% term :- List of numbers which are the leaves in

term([X],X).                    

term(List,Term) :-                    
   splitLists(List,LeftL,RightL),              
   term(LeftL,Left),                 
   term(RightL,Right),                 
   binaryT(Left,Right,Term).            

% binaryT :- binary combined term

binaryT(Left,Right,Left+Right).
binaryT(Left,Right,Left-Right).
binaryT(Left,Right,Left*Right).
binaryT(Left,Right,Left/Right) :- Right =\= 0.   % avoids division by 0



main :-
   open('input.txt',read,Str),
   read(Str,equat),
   close(Str),
   open('output.txt',write,Stream),
   write(Stream, ""),
   close(Stream),
   mergeEq(equat,LeftSide,RightSide),
   open('output.txt',append,Out),
   write(Out, LeftSide = RightSide ),
   write(Out, "\n"),
   close(Out),
   fail.