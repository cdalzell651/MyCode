-module(index).
-export([index/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.

%Given a file returns the indexed words in the order they appear first
index(File) ->
    C = indexedLines(get_file_contents(File)),
    index(C,C,[]).

index([],_,_) ->
    [];
index([[Word|Words]|Lines],File_lines,Used_words) ->
    case not member(Word,Used_words) of
        true ->
            [Word,joinNums(indexingWord(File_lines,Word))|index([Words|Lines],File_lines,[Word|Used_words])];
         false ->
                    index([Words|Lines],File_lines,Used_words)
     end;
index([_|Lines],File_lines,Words) ->
    index(Lines,File_lines,Words).

%% Return List of lines, each of which are lists of words
indexedLines([]) ->
    [];
indexedLines([Line|Lines]) ->
    [makeWords(removePunc(Line))|indexedLines(Lines)].

%%Returns true if X is in Y; member(X,Y)
member(_X,[]) ->
    false;
member(X,[X|_]) ->
    true;
member(X,[_|Ys]) ->
    member(X,Ys).


%%Takes preformatted lines and a word and returns a list of tuples containing the line numbers in which it is contained
indexingWord(Lines,Word) ->
    indexingWord(Lines,Word,0).
indexingWord([],_,_) ->
    [];
indexingWord([Line|Lines],Word,LineNo) ->
    case member(Word,Line) of
        true ->
            [LineNo|indexingWord(Lines,Word,LineNo+1)];
        false ->
            indexingWord(Lines,Word,LineNo+1)
    end.


%Takes a list of numbers and joins them into words
joinNums([])->
    [];
joinNums([Num|Nums]) ->
   tl(joinNums([Num|Nums],Num,Num)).

joinNums([],X,Y) ->
    [{Y,X}];
joinNums([Num|Nums],X,Y) ->  %X is the most recent number in a series of numbers and Y is the first number in a repeating form
    case Num == X+1 of
        true ->
            joinNums(Nums,Num,Y);
        false ->
            [{Y,X}|joinNums(Nums,Num,Num)]
         end.

%% Take a line and return a list of words
makeWords(Line) ->
    makeWords(Line,[],[]).

makeWords([],X,Y) ->
    Y++[X];
makeWords([Letter|Letters],X,Y) ->
    case Letter==32 of   %if letter is a space
        true ->
            makeWords(Letters,[],Y++[X]);
        false ->
            makeWords(Letters,X++[Letter],Y)
     end.

%%Remove all punctuation from a line
removePunc([]) ->
    [];
removePunc([X|Xs]) ->
    case member(X,",().!?\'\":;") of
        true ->
            removePunc(Xs);
        false ->
            [X|removePunc(Xs)]
    end;
removePunc(_) ->
    [].

