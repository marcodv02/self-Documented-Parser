-- HIDE
module Main where

import System.IO hiding (getLine, putStr)
import Prelude hiding (getLine, putStr, identity)

import Data.Char
-- notHIDE
-- Il nostro principale interesse sarà comprendere quali siano i costituenti di un Parser, e mostrarne l'efficacia sviluppando una "calcolatrice" che si occupi di valutare espressioni aritmetiche intere: con operazioni di somma, sottrazione, moltiplicazione e divisione intera.
-- Come strumento utilizzeremo il linguaggio Haskell, la cui peculiarità rispetto ai più comuni linguaggi basati su paradigmi imperativi e/o ad oggetti è l'uso di un paradigma funzionale più prossimo al ragionamento nella matematica. Questa è, programmazione funzionale, un tipo di programmazione sviluppata dagli stessi logici, di fatti lo stesso nome Haskell si riferisce al matematico e logico Haskell Curry.

-- Nel modulo Utils ci sono funzionalità basilari il cui funzionamento è intuitivo e dato per noto. In aggiunta anticipiamo che forma abbia il tipo di un Parser, dandone una leggera spiegazione.
import Utils

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

runParse :: Parser String -> String -> String
runParse p inp=
    case parse p inp of
    [(res, _)]  -> res
    _           -> ""

-- L'istanza di un Parser a, è un "oggetto" il cui scopo è elaborare una stringa String e produrre un [risultato] dove per risultato si ha un tipo a, ed una stringa di coda. La funzione parse non fa altro che applicare ad un Parser a l'input inp. Dove convenzionalmente utilizziamo una lista vuota per associare il fallimento del parsing, altrimenti vi sarà una lista contenente una tupla: (a,String) ovvero la restituzione del tipo a ed nuova stringa.
-- La nuova stringa è da pensare come "cibo" per il prossimo parser, ovvero quel che rimane da elaborare. Questo ci suggerisce anche come debba essere definita la legge di composizione tra Parser.

-- Imperative functional programming

-- IO Monad, the type IO is an abstract type in the sense we are not told how its values, which are called actions or command, are rappresented. But can be think of this type
-- type IO a = World -> (a,World) perciò un'azione è una funzione che prende un world e porta un valore di tipo a assieme a un nuovo World. Il nuovo World viene poi usato come input per la prossima azione. Avendo cambiato il mondo con input-output action, non si può ritornare al vecchio World. Non si può duplicare il mondo o ispezionare i suoi componenti.

-- Tutto quello che si può fare è operarci tramite delle azioni primitive, e unire in sequenze di azioni ad esempio putChar aspetta che l'utente inserisca un carattere.
{- CODE
 - putChar :: Char -> IO ()
 - done :: IO ()
-}
-- Where done function does nothing. It leaves the world unchanged and also returns the null tuple.

-- La generalizzazione di done è return :: a -> IO a, non fa nulla e ritorna il valore a. Quindi possiamo definire done a partire da return,

{- CODE
 - done= return ()
-}

-- Per sequenziare le azioni si utilizza (>>), che dati due IO () deve restituire un IO () dato dall'applicare sequenzialmente le azioni
{- CODE
 - (>>) :: IO () -> IO () -> IO ()
-}

-- Dunque data una stringa xs siamo capaci di stamparla, carattere dopo carattere,

putStrln ""= putChar '\n'
putStrln (x:xs)= putChar x >> putStrln xs

{- > putStrln "ciao, come state"
 - ciao, come state
 - >
-}

-- La generalizzazione di (>>) :: IO a -> IO b -> IO b è (>>=), se (p >> q), che esegue p e getta il suo ritorno per poi eseguire q per evitare di perdere il risultato di p serve avere allora una funzione di tipo IO a -> (a -> IO b) che prende p :: IO a, valuta ottenendo a, per poi applicare q :: IO a. Ad esempio con una sequenza di IO Char: getChar siamo capaci di definire la lettura di intere righe

{- CODE
 - getLine :: IO String
 - getLine= getChar >>= f
 -    where f x= if x=='\n' then return []
 -               else getLine >>= g
 -               where g xs = return (x:xs)
-}

-- Una soluzione più elegante, conseguenza della nozione di Monade, è l'applicabilità della do-notation in questo senso:
{- CODE
 - getLine = do {x <- getChar;
 -             if x == '\n'
 -              then return []
 -              else do {xs <- getLine;
 -              return (x:xs)}}
-}

-- Tutto ciò come accennato, è permesso dalle monadi che possiamo pensare a una classe della seguente forma:

{- CODE
 - class Monad m where
 -    return  :: a -> m a
 -    (>>=)       :: m a -> (a -> m b) -> m b
-}

-- Un esempio di monade particolarmente facile, ed utile nel controllare gli errori è dato da Maybe a. Un tipo di dato che può essere Nothing, oppure puramente un valore.

{- CODE
 - instance (Eq a) => Eq (Maybe a) where
 -    Nothing == Nothing      =   True
 -    Nothing == Just y       =   False
 -    Just    x   ==  Nothing =   False
 -    Just    x   ==  Just y  = (x == y)
 - d
 - instance (Ord a) => Ord (Maybe a) where
 -    Nothing <= Nothing  =   True
 -    Nothing <= Just y   =   True
 -    Just    x   <=  Nothing =   False
 -    Just    x   <=  Just y  = (x <= y)
 - d
 - instance Monad Maybe where
 -    return x = Just x
 -    Nothing >>= f   = Nothing
 -    Just x >>= f    = f x
-}

-- Allora per apprezzare cosa possa fare una monade, considereremo la funzione lookup :: Eq a => a -> [(a,b)] -> Maybe b, lookup x alist restituisce Just y dove y è un secondo elemento della coppia (x,y) e questa coppia è la prima coppia trovata con primo elemento coincidente con x cercato. Tale coppia ovviamente non sempre esiste, e in tal caso lookup x alist non sarà altro che Nothing. Allora se volessimo cercare in tre liste diverse: alist, blist, clist i tre valori x,y,z potremmo scrivere qualcosa del tipo:

{- CODE
 - case lookup x alist of
 -    Nothing -> Nothing
 -    Just y  -> case lookup y blist of
 -                    Nothing -> Nothing
 -                    Just z  -> lookup z clist
-}

-- In particolare stiamo esplicitamente trasportando il valore Nothing, cosa che con la struttura di monade ed in particolare tramite la composizione 

{- CODE
 - >>= (ricordiamo Nothing >>= _ = Nothing)
-}

-- ed utilizzando la do-notation basta solo scrivere

{- CODE
 - do {
 -    y <- lookup x alist;
 -    z <- lookup y blist;
 -    return (lookup z clist)
 - }
-}

-- Parser 
item :: Parser Char
item = P (\inp -> case inp of
            [] -> []
            (x:xs) -> [(x, xs)]
            )

-- If we instance the parser type in functor, applicative and monad classes we can use apply do notation to combine parsers in sequence.

instance Functor Parser where
    -- fmap :: (a->b) -> Parser a -> Parser b
    fmap g p = P(\inp -> case parse p inp of
                    [] -> []
                    [(v, out)] -> [(g v, out)]
                )

    -- In definitiva applichiamo in maniera naturale la trasformazione su tutti i risultati dei parser, mentre il "cibo" deve giustamente rimanere invariato

-- Now we can map to parser results, as example toUpper after got an item from the word "abc"

{- > parse (fmap toUpper item) "abc".
 - [('A',"bc")]
-}

-- The function parse has the work to come back from the world of parsers, and to work with results we defined an Functor instance with fmap, as example we can apply getUpper to all Parser String and get a Parser String (with all results in uppercase). Obviously we can develop something more complex, but in this text we'll not because we'll work with just simple structures as Int, String and [String].


instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a->b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
            [] -> []
            [(g, out)] -> parse (fmap g px) out)


-- One aspect of functional programming is than even far simple function have very useful behaviour, as pure v example gives just the struct of a Parser than works every time and has v as result value without need of feed. Now with composition, one of the most archivement about this kind of thinking functionaly we'll see how pure is essential.
-- The infix operator <*> is associative at left and don't do nothing than compose in Parser way,
-- pure g <*> x <*> y = pure (g <*> x <*> y) = pure ((g <*> x) <*> y)
-- So with pure we ensure than we don't leave by the world of the Parsers, in turn the <*> permits us to compose applicate in sequence parsers. <*> applies
-- ITEMIZE
--    a parser that returns a function, namely (a->b)
--    to a parser, Parser a, that returns an argument, namely b
--    to give a parser that returns the result of applying the function g to the argument b
-- It'll only succeeds if all the components succeed.
-- ETEMIZE
--

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P ( \inp -> case parse p inp of
        [] -> []
        [(v,out)] -> parse (f v) out)

-- That is, the parser p >>= f fails if the application of the parser p to the input string inp fails, and otherwise applies the function f to the result value v to give another parser f v, which is then applied to the output string out that was produced by the first parser to give the final result.
-- Because Parser is a monadic type, the do notation can now be used to sequence parsers and process their result values.

three :: Parser (Char, Char)
three = do {
            x <- item;
            item;
            y <- item;
            return (x,y)
            }


-- Recall that the monadic function return is just another name for the applicative function pure, which in this case builds parsers that always succeed.

-- Making choices
-- The do notation combines parsers in sequence, with the output string from each parser in the sequence becoming the input string for the next. Another natural way of combining parsers is to apply one parser to the input string, and if this fails to then apply another to the same input instead. We now consider how such a choice operator can be defined for parsers.
-- Making a choice between two alternatives isn't specific to parsers, but can be generalised to a range of applicative types. This concept is captured by the following class declaration in the library Control.Applicative

{- CODE
 - class Applicative f => Alternative f where
 -    empty :: f a
 -    (<|>) :: f a -> f a -> f a
-}

-- HIDE
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    tryOn :: f a -> f b -> f b
    many  :: f a -> f [a]
    some  :: f a -> f [a]
    
    many x = some x <|> pure []
    some x = pure (:) <*> x <*> many x
-- notHIDE

-- That is, for an applicative functor to be an instance of the Alternative class, it must support empty and <|> primitives of the specified types. (The class also provides two further primitives, which will be discussed in the next section)

-- The intuition is that empty represents an alternative that has failed, and <|> is an appropriate choice operator for the type. The two primitives are also required to satisfy the following identity and associativity laws:
{- CODE
 -          empty <|> x     =   x
 -          x <|> empty     =   x
 -          x <|> (y <|> z) = (x <|> y) <|> z
-}

-- The motivating example of an Alternative type is the Maybe type, for which empty is given by the failure value Nothing, and <|> returns its first argument if this succeeds, and its second argument otherwise:

instance Alternative Maybe where
    -- empty :: Maybe a
    empty = Nothing

    -- (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> my = my
    (Just x) <|> _ = Just x
-- HIDE
    tryOn Nothing my = my
    tryOn _ _        = Nothing
-- notHIDE

-- The instance for the Parser type is a natural extension of this idea, where empty is the parser that always fails regardless of the input string, and <|> is a choice operator that returns the result of the first parser if it succeeds on the input, and applies the second parser to the same input otherwise:

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                [] -> parse q inp
                [(v,out)] -> [(v,out)])

    -- tryOn :: Parser a -> Parser b -> Parser b
    tryOn p q=  P (\inp -> case parse p inp of
                    [] -> parse q inp
                    [(v,out)] -> [])

{- > parse empty "abc"
 - []
 - > parse (item <|> return 'd') "abc"
 - [('a', "bc")]
 - > parse (empty <|> return 'd') "abc"
 - [('d',"abc")]
-}

-- Derived primitives
-- We now have three basic parsers: item than consumes a single character if the input string is non-empty, return v always succeeds with the result value v, and empty always fails. In combination with sequencing and choice, these primitives can be used to define a number of other useful parsers. First of all, we define a parser sat p for single characters that satisfy the predicate p:

sat :: (Char -> Bool) -> Parser Char
sat p = do {
            x <- item;
            if p x then return x else empty
            }


-- Now we can define parsers for single: digits, lower-case letter, upper-case, arbitrary letters, alphanumeric characters.

digit, lower, upper, letter, alphanum :: Parser Char
digit=  sat isDigit
lower=  sat isLower
upper=  sat isUpper
letter= sat isAlpha

alphanum=   sat isAlphaNum

char :: Char -> Parser Char
char x= sat (==x)

-- In turn, using char we can define a parser string xs for the string of characters cs, with the string itself returned as the result value:

{- CODE
 - string :: String -> Parser String
 - string [] = return []
 - string (x:xs) = do {
 -                    char x;
 -                    string xs;
 -                    return (x:xs)
 -                    }
-}

-- Note, than string only succeeds if the entire target is consumed from the input. Because if it's not the char x will return empty at some point, and recursively with return will be all empty.

-- Next two parsers, many p and some p, apply a parser p as many times as possible until it fails, with the result values from each successful application of p being returned in a list. The difference between these two repetition primitives is that many permits zero or more application of p, whereas some requires at least one successful application
{- > parse (many digit) "123abc"
 - [("123","abc")]
-}

-- Now we'd complete the Alternative class knowledge:

{- CODE
 - class Applicative f => Alternative f where
 -    empty :: f a
 -    (<|>) :: f a -> f a -> f a
 -    many  :: f a -> f [a]
 -    some  :: f a -> f [a]
 -    many x = some x <|> pure []
 -    some x = pure (:) <*> x <*> many x
-}

-- Note that the two new functions are defined using mutual recursion. In particular, the above definition for many x states that x can either be applied at least once or not at all, while the definition for some x states that x can be applied once and then zero or more times, with the results being returned in a list. These functions are provided for any applicative type that is an instance of the class, but are primarily intended for use with parsers.

-- Using many and some, we can now define parsers for identifiers (variable names) comprising a lower-case letter followed by zero or more alphanumeric characters, natural numbers comprising one or more digits, and spacing comprising zero or more space, tab, and newline characters:

ident :: Parser String
ident = do {
            x <- lower;
            xs <- many alphanum;
            return (x:xs)
            }

nat :: Parser Int
nat = do{
xs <- some digit;
            return (read xs);
        }

space :: Parser ()
space = do {
           many (sat isSpace);
           return ()
          }

int :: Parser Int
int = do{
            char '-';
            n <- nat;
            return (-n)
        } <|> nat;

{- > parse int "-123 abc"
 - [(-123," abc")]
 - > parse nat "-123 abc"
 - []
-}

-- Handling spaces
-- In real-life, for a human pov, parsers allow spacing to be freely used around the basic tokens in their input string. For example, the strings 1+2 and 1 + 2 are both parsed in the same way by GHC. To handle such spacing, we define a new primitive that ignores any space before and after applying a parser for a token:

{- CODE
 - token :: Parser a -> Parser a
 - token p = do{
 -                space;
 -                v <- p;
 -                space;
 -                return v;
 -            }
-}

-- Using token, we can now define parsers that ignore spacing around identifiers, natural numbers, integers and special symbols:

identifier :: Parser String
identifier = token ident

natural, integer :: Parser Int
natural = token nat
integer = token int

{- CODE
 - symbol :: String -> Parser String
 - symbol xs = token (string xs)
-}

-- Using these primitives we can define a parser for a non-empty list of natural numbers that ignores spacing around tokens can be defined as follows

nats :: Parser [Int]
nats = do {
            symbol "[";
            n <- natural;
            ns <- many (do {symbol ","; natural});
            symbol "]";
            return (n:ns)
            }

-- Arithmetic expressions
-- We conclude this chapter with two extended programming examples concerning arithmetic expressions. For our first example, consider a simple form of expressions that are built up from natural numbers using addition, multiplication and parentheses. We assume that addition and multiplication associate to the right, and that multiplication has higher priority than addition. For example, 2+3+4 means 2+(3+4) , while 2*3+4 means (2*3)+4
-- The syntactic structure of a language can be formalised using the mathematical notion of a grammar, which is a set of rules that describes how strings of the language can be constructed. For example, a grammar for out language of arithmetic expressions can be defined by the following two rules:

{- CODE
 - exp ::= expr + expr | expr * expr | ( expr ) | nat
 - nat ::= 0 | 1 | 2 | ...
-}

{- HIDE
 - expr :: Parser Int
 - expr =  prod <|> somma <|> parenthesis <|> nat
 - somma :: Parser Int
 - somma = do{
 -        a <- nat;
 -        do {
 -           symbol "+";
 -           b <- expr;
 -           return (a+b)
 -           } <|> return a
 -       }
 - prod :: Parser Int
 - prod = do{
 -       a <- nat;
 -       do {
 -           symbol "*";
 -           b <- expr;
 -           return (a*b)
 -           } <|> return a
 -       }
 - parenthesis :: Parser Int
 - parenthesis = do{
 -                  symbol "(";
 -                  e <- expr;
 -                  symbol ")";
 -                  return e
 -               }
-}

--The first rule states that an expression is either the addition or multiplication of two expressions, a parenthesised expression, or a natural number. The second rule states what a natural number is.

-- The problem is that our grammar for expressions does not take account of the fact that multiplication has higher priority than addition. However, this can easily be addressed by modifying the grammar to have a separate rule for each level of priority, with addition at the lowest level of priority, multiplication at the middle level, parentheses and numbers at the highest level:

{- CODE
 - expr ::= expr + expr | term
 - term ::= term * term | factor
 - factor ::= ( expr ) | nat
-}

{- CODE
 - expr :: Parser Int
 - expr = do{
 -          t<- term;
 -          do {
 -              symbol "+";
 -              e <- expr;
 -              return (t+e)
 -              } <|> return t
 -       }
-}

{- CODE
 - term :: Parser Int
 - term= do{
 -      f <- factor;
 -      do {
 -          symbol "*";
 -          t <- term;
 -          return (f*t)
 -          }
 -      <|> return f
 -      }
-}

{- CODE
 - factor :: Parser Int
 - factor = do{
 -       symbol "(";
 -       e <- expr;
 -       symbol ")";
 -       return e
 -       } <|> natural
-}

-- Note that each of the above parsers returns the integer value of the expression that was parsed, rather than some form of expression tree. Combing parsing and evaluation in this manner is easy to achieve using our approach. For example, expr first parses a term with integer value t, then parses an addition symbol followed by an expression with value e and returns the value t+e, or else parses nothing further and simply returns the value t.
-- Notice than we can generalize an operator of the kind + as terms, * as factor:
-- For example + need a term, try to see if can added: + expr and return itself of the add with another term. So we can think to build a parser than builds those operations

chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
chainl1 p op = do{
                    x <- p;
                    rest x;
                 } where rest x= do{
                                    f <- op;
                                    y <- p;
                                    rest (f x y)
                                 } <|> return x


-- It take an operand a, try to take an operator: a->a->a, and return the result a. Informally this take an general operand, and it feed itself with operator until there is operator food: term op ...

{- CODE
 - addop :: Parser (Int -> Int -> Int)
 - addop = symbol "+" *> pure (+)
-}

-- *> lives in the typeclass Applicative and as <*> is used to sequencing actions, but ignoring the result of the first. So this definition says apply symbol "+" than get feeded from inp, and if it's succeed return the pure function (+). With this behaviour we can define for subtractions too,

addop = (symbol "+" *> pure (+))
        <|> (symbol "-" *> pure (-))

mulop = (symbol "*" *> pure (*))
        <|> (symbol "/" *> pure div)

expr :: Parser Int
expr = term `chainl1` addop

-- Operand, and operation with chains make the "(+ | -) eater" for expression.

term :: Parser Int
term = factor `chainl1` mulop

-- As same, we take a "term" factor an operation (* | /) and get with chain a (* | /) eater.
-- Now, the factor is nothing than an expression :: Parser Int between two symbols. So we need take the result in the middle from two application, using the idea of *> and extended to <* that take in account just the first result we can write

between :: Applicative f => f open -> f close -> f a -> f a
between open close p= open *> p <* close

-- So in Parser Language as type
{- CODE
 - between :: Parser open -> Parser close -> Parser a -> Parser a 
-}

factor :: Parser Int
factor = between (symbol "(") (symbol ")") expr
        <|> nat


-- So now, we're capable to evaluate arithmetical expression with operators +,-,*,div
{- CODE
 - eval :: String -> Int
 - eval cs = case (parse expr xs) of
 -   [(n,[])] -> n
 -   [(_,out)] -> error ("Unused input " ++ out)
 -   []        -> error "Invalid input"
-}

box :: [String]
box= [
    "+---------------+",
    "|               |",
    "+---+---+---+---+",
    "| q | c | d | = |",
    "+---+---+---+---+",
    "| 1 | 2 | 3 | + |",
    "+---+---+---+---+",
    "| 4 | 5 | 6 | - |",
    "+---+---+---+---+",
    "| 7 | 8 | 9 | * |",
    "+---+---+---+---+",
    "| 0 | ( | ) | / |",
    "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
    where
        standard= "qcd=123+456-789*0()/"
        extra= "QCD \ESC\BS\DEL\n"
showbox :: IO ()
showbox = sequence_ [writeAt (1,y) b | (y,b)<- zip[1..] box]

display xs = do{
                writeAt (3,2) (replicate 13 ' ');
                writeAt (3,2) (reverse (take 13 (reverse xs)));
                }

calc :: String -> IO ()
calc xs = do {
            display xs;
            c <- getCh;
            if elem c buttons then
                process c xs;
            else
                calc xs;
            }


-- The function process takes a valid character and the current string, and performs the appropriate action depending upon the character: 

process :: Char -> String -> IO ()
process c xs
    | elem c "qQ\ESC" = quit
    | elem c "=\n" = eval xs
    | elem c "cC" = clear
    | otherwise = press c xs


-- ITEMIZE
-- ITEM
-- Quitting moves the cursor below the calculator box and terminates:
quit :: IO ()
quit= goto (1,14)


-- ITEM
-- Deleting a character has no effect if the current string is empty, and otherwise removes the last character from this string
delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)


-- ITEM
-- Evaluation displays the result of parsing and evaluating the current string, sounding a beep if this process is unsuccessful:
eval :: String -> IO ()
eval xs= case parse expr xs of
                [(n,[])]    -> calc (show n)
                _           -> calc xs


-- ITEM
-- Clearing the display resets the current string to empty:
clear :: IO ()
clear = calc []


-- ITEM
-- Any other character is appended to the end of the current string:
press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])


-- ETEMIZE

-- Finally, we define a top-level function hat runs the calculator, by clearing the screen, displaying the box, and starting with an empty display:

runCalc :: IO ()
runCalc = do {
           cls;
           showbox;
           clear;
       }


-- Now we're ready to define our grammar, to make a documentation about this code. For practical use we redefine an Output purpose Parser, while a succeeds means a ready string to write on the documentation file. Even we've a short resume of all structures,

{- CODE
 - many :: Parser a -> Parser [a]
 - many p = \s -> case parse p s of
 -                []     -> ([], s)
 -                [(v,s')] -> let (vs,s'') = many p s' in (v:vs, s'')
-}

-- notice than isSpace return True on newline: '\n', so we'll define justSpace too. And we'll have no case sensitive directives: RULE, CODE, ITEMIZE, ITEM so we need more generalized char Parser than apply char transformation. Consequently there is the natural generalization of stringT

justSpace :: Parser ()
justSpace= do {
                many (sat (\x -> x==' '));
                return ()
        }

charT :: Char -> (Char -> Char)-> Parser Char
charT x f= sat (==(f x))

stringT :: String -> (Char -> Char) -> Parser String
stringT [] _= return []
stringT (x:xs) f= do {
                    charT x f;
                    stringT xs f;
                    return (x:xs)
                }

-- To remove trailing characters, as spaces or newlines we define token and ntoken Parsers. Rispectively one look for justSpace, and isSpace (newline included). Consequently, with Parser a constant string and a parser of trailing characters it'll be the symbol Parser: 
token :: Parser a -> Parser a
token p = justSpace *> p <* justSpace

ntoken :: Parser a -> Parser a
ntoken p = space *> p <* space

symbolT :: String -> (Char -> Char) -> Parser String
symbolT xs f= token (stringT xs f)

string, symbol :: String -> Parser String
string xs= stringT xs identity
symbol xs= symbolT xs identity
nsymbol xs= ntoken (string xs)

-- Now we think the source code as a list of blocks: multiple comments: {- -}, inline comments than begins with -- without left trailing characters and finally the lines of codes. On those blocks we want for inlines comment to distinguish latex paragraph and directives (as example hide a part of the source CODE, magic functions than nobody wants to share or as well known function than are needed to define). For multi-line comments we want to: hide it's content, show code without compile it or show an interactive session of ghci. The rest we'll consider as code, or useless newlines.
-- Notice than "our" inline comments "--" starts at the beggining of the line, so spaced -- can be used to make a view of code with comments in the result pdf.

sourceCode :: Parser String
sourceCode= fmap getString (many block)

block :: Parser String
block= multiLine <|> inline <|> code <|> string "\n"

-- It's not Parser content but almost I'll show codeEnvironment code, than just wrap a string in a latex environment: lstlisting. And a important Parser than have the goal to take the entire line, and fails just if it's an empty line. We define plainrow without removing spaces at left.

notNewline= sat (\x -> x/='\n')

wsplainrow= (((some notNewline) <* (string "\n" <|> pure "")) <|> (pure "" <* string "\n"))

plainrow= justSpace *> (((some notNewline) <* (string "\n" <|> pure "")) <|> (pure "" <* string "\n"))
-- Observe than with space instead of justSpace the string "\n" will become "" so correctly plainrow throws []. Behind the logic of this parser every "complex" Parser have to start at newlines, so if it's sees \n that means we're on an empty line.
{- CODE
 - plainrow= space *> (do {
 -                          x<- some (sat (\x->x/='\n'));
 -                          (string "\n" <|> pure "");
 -                      } <|> (pure "" <* string "\n"))
-}

codeEnvironment :: String -> String -> String
codeEnvironment style= wrap ("\\begin{lstlisting}[style="++style++"]\n", "\\end{lstlisting}\n")

ghci= "ghcistyle"
haskell= "haskellstyle"

-- Code block Parser,

code= do{
            y<- ntoken (fmap getString codeLines);
                if y=="" then
                    return ""
                else
                    return (codeEnvironment haskell y)
      }

codeLines :: Parser [String]
codeLines= tryOn (symbol "{-" <|> string "--") (
                    do {
                        y<-wsplainrow;
                        xs<-(codeLines <|> pure []);
                        return ([y++"\n"]++xs)
                    })
-- tryOn :: Parser a -> Parser b -> Parser b, is a custom function in the Applicative class and his purpose is to ensure than doesn't start another parser. It's need comes from: "almost everything is code", so if starts a comment we've to notice it. This is not a smart parsers, the low numbers of steps in the grammar comes at the cost to be pretty sensitive. tryOn purpose is try parser p, and be empty parser in case of success otherwise it's q as suggest the type declaration.

{- CODE
 - tryOn Nothing my = my
 - tryOn _ _        = Nothing
-}

-- For inline comments there is not a lot of work, we've just to distinguish paragraph and or rules. In this version we'll consider just hide, until --"notHIDE"

inline = do {
            symbol "--";
            hide <|> do {
                        s<- plainrow;
                        return (safeText (s++"\n\n"))
                    }
            }
hide= do {
            symbol "HIDE";
            jump "-- notHIDE";
            return "";
        }

-- Multiline: HIDE comments or lines of code, CODE viewed but not compiled, > an interactive session or just lines. As parenthesis in expressions we'll use the between function, and the commentlines Parser. Notice than commentlines wants - at begin of each line, but it's not needed we're just force the good developer to use - for more readable on source code.

multiLine :: Parser String
multiLine= between (symbol "{-") (symbol "-}") commentlines

commentlines :: Parser String
commentlines= multiHide <|> multiCode <|> interactiveGHCI <|> noRule

multiHide= do {
                symbolT "HIDE" toUpper;
                plainrow;
                getLines;
                return "";
            }

-- For practical use of the prompt line, we'll use getLines to Parse lines as [String]. So to get the unified string we'll use getString [String] -> String after have worked on every string with map.

multiCode= do{
                symbolT "CODE" toUpper;
                plainrow;
                x <- fmap (getString . addNewlines) getLines;
                return (codeEnvironment haskell x)
            }
noRule= do {
                g<- plainrow;
                x <- getLines;
                if g=="" then
                    return (safeText (getString (map (wrap ("\\[","\\]\n")) x)))
                else
                    return ("Not understood rule: "++g)
            }

interactiveGHCI= do {
                symbol ">";
                x <- plainrow;
                xs <- fmap addNewlines getLines;
                return (
                    codeEnvironment ghci (getString ([wrap ("@ghci>@ ", "\n") x] ++ pr xs))
                )
            } where
                pr = map (\y ->
                                if not (null y) && head y == '>'
                                then "@ghci>@ "++tail y
                                else y
                            )

getLines :: Parser [String]
getLines= many unrawComment

unrawComment= tryOn (symbol "-}")
                    (
                        do{
                            string " - ";
                            x <- (wsplainrow <|> pure "");
                            return x
                        }
                    )
-- As codeLines the recognize of - in "-}" gives too food for our parser, so I'd to check the stop before with tryOn.
-- Introdurremo successivamente in un'altra versione la possibilità di gestire l'environment itemize di latex,
{- CODE
 - itemize= between (symbol "--ITEMIZE\n") (symbol "--ETEMIZE\n") items
 -            where items= concat <$> many (do{
 -                                symbol "--ITEM";
 -                                document
 -                             })
-}

-- Mentre la definizione di jump xs è particolarmente semplice, riconosce il simbolo xs oppure si va avanti "mangiandosi" i caratteri.

jump :: String -> Parser ()
jump xs= (symbol xs >> pure ()) <|> (item >> jump xs)

-- Riassuntivamente stiamo seguendo le seguenti convenzioni
{- CODE
 - lineComment ::= -- hide | -- paragraph
 - multiComment ::= -\{ RULE\n multiText \n -\}
 - code ::= alphabet text \n
 - hide ::= HIDE | notHIDE
 - rule ::= CODE | HIDE | notHIDE
 - title ::= CAP | SEC | ITEMIZE digit | ETIMIZE | ITEM
 - paragraph ::= alphaNum | space | paragraph
 - text ::= alphaNum | space | text
-}

outputFile= "Main.tex"

main :: IO ()
main= do{
            content <- readFile "Main.hs";
            begin <- readFile "documentBegin.tex";

            writeFile outputFile begin;
            appendFile outputFile (runParse sourceCode content);
            appendFile outputFile "\\end{document}";

            putStr("Buona lettura\n");
         }
