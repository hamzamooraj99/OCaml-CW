(* This is an executable OCaml file *)
   exception Question of string

   let _cw_fail q = raise (Question q)
   
   
   (* ****************************** Start of Questions ********************************* *)
   
   
   
   
   (* ****************************** Start of Question 1 ********************************* *)
   
   (* Question 1: functions over boolean lists *)
   
   (* Write a function to compute the disjunction of a list of booleans *)
   
   (* Q1a: using direct recursion *)
   
   let rec or_list (bs : bool list) =
     match bs with
     | [] -> false
     | h::t -> h || or_list t;;
   
   let test_q1a_1 = (or_list [] = false)
   let test_q1a_2 = (or_list [false] = false)
   let test_q1a_3 = (or_list [true] = true)
   let test_q1a_4 = (or_list [false;false] = false)
   let test_q1a_5 = (or_list [true;false] = true)
   let test_q1a_6 = (or_list [false;true] = true)
   let test_q1a_7 = (or_list [true;true] = true)
   let test_q1a_8 = (or_list [false;false;false] = false)
   let test_q1a_9 = (or_list [false;true;false] = true);;
   
   
   (* Q1b: using List.fold_right *)
   
   let or_list_fold = 
     function
     | [] -> false
     | h::t -> List.fold_right (fun x y -> x||y) t h;;
   
     let test_q1b_1 = (or_list_fold [] = false)
     let test_q1b_2 = (or_list_fold [false] = false)
     let test_q1b_3 = (or_list_fold [true] = true)
     let test_q1b_4 = (or_list_fold [false;false] = false)
     let test_q1b_5 = (or_list_fold [true;false] = true)
     let test_q1b_6 = (or_list_fold [false;true] = true)
     let test_q1b_7 = (or_list_fold [true;true] = true)
     let test_q1b_8 = (or_list_fold [false;false;false] = false)
     let test_q1b_9 = (or_list_fold [false;true;false] = true);;
   
   (* ****************************** End of Question 1 ********************************* *)
   
   
   
   
   (* ****************************** Start of Question 2 ********************************* *)
   
   (* Question 2: the Sudan function *)
   
   (* Look up the definition of the Sudan function in Wikipedia:
      https://en.wikipedia.org/wiki/Sudan_function
      and implement it in OCaml; 
      use the tabulated values of sudan(n,x,y) there as tests for your code
    *)
   
   (* Q2a: as a function of type int -> int -> int -> int
   
      use recursion directly, testing arguments using 'if then else' *)
   
   let rec sudan_int (n : int) (x : int) (y : int) = 
     if n=0 then x+y
     else if y=0 then x
     else sudan_int (n-1) (sudan_int (n) (x) (y-1)) (y + sudan_int (n) (x) (y-1));;
   
   let test_q2a_1 = (sudan_int 0 3 4 = 7)
   let test_q2a_2 = (sudan_int 1 4 0 = 4)
   let test_q2a_3 = (sudan_int 1 0 4 = 26)
   let test_q2a_4 = (sudan_int 1 2 3 = 27)
   let test_q2a_5 = (sudan_int 2 2 1 = 27);;
   
   
   
   
   (* Q2b: as a function of type nat -> nat -> nat -> nat 
      where the type nat is defined below; 
      you may use the supplied definition of addition on nats
      in your solution
    *)
   
   type nat = Zero | Succ of nat;;
   
   let add_nat m n =
     let rec helper m = match m with Zero -> n | Succ m -> Succ (helper m)
     in helper m;;
   
   let zero_nat = Zero
   let one_nat  = Succ zero_nat             
   let two_nat  = Succ one_nat           
   let four_nat  = add_nat two_nat two_nat
   
   (*Tests add_nat*)
   let test_q2b_add_nat = ((add_nat one_nat one_nat) = two_nat);;
   
   (*----------- Q2b Ans ---------*)
   
   (*Created a function that Decrements nat to help with sudan function*)
   let dec_nat n = 
     match n with
     | Zero -> Zero (*If n is Zero, it cannot be decremented, so return Zero*)
     | Succ n -> n;; (*If n is Successor of n then return n (e.g if n is 1 then return 0)*)
                  
   let rec sudan_nat (n : nat) (x : nat) (y : nat) = 
     if n = zero_nat then add_nat x y
     else if y = zero_nat then x
     else sudan_nat (dec_nat (n)) (sudan_nat (n) (x) (dec_nat (y))) (add_nat (y) (sudan_nat (n) (x) (dec_nat (y))));;
   
   (*----------- Q2b Ans ---------*)
   
   let three_nat = dec_nat four_nat;;
   
   (*Tests sudan_nat*)
   let test_q2b_1 = ((sudan_nat Zero three_nat four_nat) = (add_nat (four_nat) (three_nat)))
   let test_q2b_2 = ((sudan_nat one_nat four_nat Zero) = (four_nat))
   let test_q2b_3 = ((sudan_nat one_nat two_nat one_nat) = (add_nat (four_nat) (one_nat)))
   let test_q2b_4 = ((sudan_nat two_nat one_nat one_nat) = (add_nat (four_nat) (four_nat)));;
   
     
   (* ****************************** End of Question 2 ********************************* *)
   
   
   
   
   (* ****************************** Start of Question 3 ********************************* *)
   
   (**** Q3: essay ****)
   
   (* Write a short essay about the functional programming (FP) in OCaml, 
      illustrated with code examples as appropriate (* Which should compile correctly *)
      which addresses the following points:
   
      a. the roles of types in programs, including parametric polymorphism
      b. expressions evaluation, focusing on function application and pattern-matching
      c. the use of the Y combinator in implementing recursion - let rec y f x = f (y f) x;;
      d. contemporary applications of FP technology
   
   *)
   
   (*
     OCaml is a functional programming language. Functional programming is a programming paradigm where 
     programs are built on creating and applying functions. This allows programs to have a more clean and
     maintanable code.
   
   a.
     Types are one of the most important things in all types of programming; not just functional. The types
     in programming define what inputs are allowed for functions, and essentially what the outputs are. They
     are somewhat like a set of rules set for functions created, that keep them from giving unnecessary and 
     unexpected outputs. Types are also vital for recursion. When using recursion, the output type is very
     important, so that we know that no errors may arise when using the wrong type for any predefined 
     operations and functions. 
     
     Parametric polymorphism is a technique that allows the parameters for a function be of a general type
     by using variables rather than types. For example:*)
       let s x y z = (x z) (y z);; (*is an example of a parametric polymorphic function*)
       (*and*)
       let s x y (z : int) = (x z) (y z);; (*is an example of the above function, but only with an integer
       input, thus making it non-polymorphic*)
   (*Just because in parametric polymorphism the types of the parameters are of a generic type, does not 
     mean that types have no role. In fact they still play the same role they do in non-polymorphic 
     functions. The type of the output computed is still completely dependent on the type of the input. In
     the above examples, the code does the same thing. The only difference is that the polymorphic version 
     works with integers, strings, booleans etcetera, and the non-polymorphic version only works with
     integers
   
   b.
     Expression evaluation is exactly what it sounds like. It is the evaluation of an expression defined in
     OCaml. An example of a very basic expression is:*)
       let x = 1 + 2 * 3;; (*This would follow the rules of BODMAS resulting in the answer of 7*)
   (*Functions are basically a collection of expressions to be executed as a collection. A technique commonly
     used in OCaml functions is pattern-matching. An example of pattern-matching is:*)
       let non_0_dec (x : int) = 
         match x with
         | 0 -> 0
         | x -> x-1;;(*This function decrements the integer x by 1. If x is 0 then it simply returns 0*)
   (*The above function used pattern matching which is similar to Python's switch case statement(also known as 
     pattern matching). It allows the function to somewhat white-list the possible inputs of the parameter.
     In the above example, x is matched with the possible values 0, or something else. The match is followed 
     by an expression that is to be executed if that match case is met. If x is not 0, then the expression
     x-1 should be executed.
   
   c.
     Recursion is the technique of calling a function within a function. An example of a recursive function
     is:*)
     let rec factorial n =
       if n = 0 then 1
       else n * (factorial (n-1));;(*This recursive function calculates teh factorial of a number*)
   (*The recursive part of this function runs factorial on n-1.
     A Y-combinator is a fixed-point higher-order function that operates on other functions. It enables recursion 
     when you can't refer to the function from within itself. In computer-science theory, it generalizes recursion, 
     abstracting its implementation, and thereby separating it from the actual work of the function in question.
      
   d.
     Functional programming languages are specially designed to handle symbolic computation and list processing 
     applications. Functional programming techniques and abstractions are viewed as essential in any programming
     language, and that is why these FP techniques have been adopted by the most popular languages used today, 
     such as, Java and Python. Modern UI Design is influenced by such FP techniques. In fact, Google's MapReduce
     is a vital implementation of a FP concept. The increase in demand for concurrent software has forced programmers
     to learn FP techniques and apply them. FP is also used where many operations on a single set of data must be performed.
     This would play right into the hand of Artificial Intelligence, which makes great use of data analysis. Due to FP
     being designed on mathematical functions, FP has a very important part to play in Modern Science and research.
   *)
   
   (* ****************************** End of Question 3 ********************************* *)
   
   
   
   
   (* ****************************** Start of Question 4 ********************************* *)
   
   (**** Q4: the s, k and i combinators ****)
   
   (* using the definitions below:
   
      a. write down the types of s, k and i
      b. what are the values of (i 3), (i true)?
      c. explain your answer to b. by showing in general how i evaluates its arguments
   
    *)
   
   let s x y z = (x z) (y z);;
   
   let k x y = x;;
   
   let i x = s k k x;;
   
   let test_q4_1 = ((i 1) = 1)              
   let test_q4_2 = ((k 2 3) = 2)
   let test_q4_3 = ((s (fun x y -> y + x) (k 4) 5) = 9);;
   
   (*
   a. Firstly, the s, k and i combinators are all functions. However, their definition states that their output type is dependant on their input type. This means that when we write 
      the code for the i combinator, for example, 
         # let i x = s k k x;;
         val i : 'a -> 'a = <fun>
      we get the above. The " 'a -> 'a " shows that whatever the type of the input x is, within the i combinator, will be the type of the output as well. So if x was of type integer,
      then the output would be of type integer, making the i combinator of generic_type. This is the same for the s and k combinators as well:
         # let s x y z = (x z) (y z);;
         val s : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c = <fun> (*As you can see with the input of type 'c, the output is of type 'c as well*)
   
         # let k x y = x;;
         val k : 'a -> 'b -> 'a = <fun> (*The output type is dependent on the first input type ('a)*)
   
   b. The values of (i 3) and (i true) are as follows:
         # (i 3);;
         - : int = 3
         # (i true);;
         - : bool = true
   
   c. To explain the above, we need to break down all three of the combinators. For the s combinator, we can deduce that the inputs x and y are functions, and z is the value input.
      so if we look at the i combinator, we can see that it uses the k combinator as the function inputs for the s combinator:
         s k k z = (k z) (k z)
      A missing input in k results in the following definition:
         # k 3;;
         - : '_weak1 -> int = <fun> (*The int would be bool in the case of (k true)*)
      Since OCaml knows that the type of the above cannot change, and that the second input does not affect the final result, it simply waits for the second input to be given, before 
      giving a final result.
      The i combinator is defined i x = s k k x. This replaces z in my above examples with x, so now it reads:
         s k k x = (k x) (k x)
       Another way of looking at this is:
         s k k x = k (x) (k x)
       This would give k its two inputs where x is (x), and y is (k x). The k combinator is quite simply defined that it returns its first input, so the final result would give us whatever
       is stored within x.
       Finally, looking at the i combinator as a whole, i x = s k k x, we can comfortably say that basically the i combinator is 
         let i x = x;;
   *)
   
   (* ****************************** End of Question 4 ********************************* *)
   
   
   
   
   (* ****************************** Start of Question 5 ********************************* *)
   
   (**** Q5: triangular numbers ****)
   
   (* Q5a. write a function ints : int -> int list which given an argument (n : int), 
      returns the list consisting of [0; 1; 2;...; (n-1)], provided n > 0, and returns [] if n <= 0. 
    *)
   
   let ints (n : int) = 
     if n <= 0 then []
     else 0 :: List.init (n-1) (fun x -> x+1);;
   
   let test_q5a_1 = (ints 0 = [])       
   let test_q5a_2 = (ints 3 = [0; 1; 2])      
   let test_q5a_3 = (ints 1 = [0])
   let test_q5a_4 = (ints ~-1 = [])(*Negative test case*);;
   
   
   
   
   (* Q5b. implement the function tri : int -> int which given an argument (n : int), 
         returns (n - 1) + (n - 2) + ... + 1 + 0, using the function ints defined in a. *)
   
   let tri (n : int) = 
     List.fold_right (+) (ints n) (0);;
   
     let test_q5b_1 = (tri 3 = 3)
     let test_q5b_2 = (tri 7 = 21)
     let test_q5b_3 = (tri 10 = 45);;
   
   
   
   
   (* Q5c. how else might you implement the function tri directly?
         how would you try to show that the two implementations agree? *)
   
   (* The function tri can be implemented directly using recursion and without
   the use of the ints function. We simply do not need a list and can use the idea of 
   (n-1) + ((n-1) - 1). All we need to do is find a way to decrement (n-1) every
   iteration, until n = 0. Using recursion, we can do this. The code can be
   implemented as follows: *)
   
   let rec tri2 (n : int) = 
       if n <= 0 then 0
       else (n-1) + (tri2 (n-1));;
   
   (* We can show the two implementations agree with a few test cases, such as: *)
       let test_tri_1 = ( (tri 0 = tri2 0) )
       let test_tri_2 = ( (tri 7 = tri2 7) );;
   (* The above statement will return true if the two function agree with one another. *)
   
   (* ****************************** End of Question 5 ********************************* *)
   
   
   
   
   (* ****************************** Start of Question 6 ********************************* *)
   
   (**** Q6: expression trees ***)
   
   (* Q6a: show how to express the following type of expression trees in OCaml:
   
      an expression tree consists of either: 
      - a literal integer constant, eg (Con 3)
      - a 'variable', identified by a string value, eg (Var "v42")
      - two subtrees connected with the binary operator Plus
   
    *)
   
    type tree = Con of int | Var of string | Plus of tree * tree;;
   
   
   
   
   (* Q6b: write down the corresponding 'fold' operator for the type exp_tree *)
   
   let rec tree_fold f init t = 
     match t with
     | Plus(l, r) -> f (tree_fold f init l) (tree_fold f init r)
     | Con x -> f (Con x) (init)
     | Var s -> f (Var s) (init);;
   
   
   
   
   (* Q6c: show how to express an evaluation function for trees, 
           taking an argument env of type (string * int) list, 
           and returning an int, such that:
   
      - literal constants evaluate to themselves
      - Plus expressions evaluate to the sum of the values of their components 
   
      In your answer, pay careful attention to what kinds of potential errors might arise, 
      and how you might handle them
    *)
   
   (* Returns the integer value associated with string in the (string*int) list in the form of Con of int*)
   let rec find (str : string) = function
     | [] -> raise Not_found
     | (h, value)::t -> 
       if str = h then Con (value)
       else find str t;;
   (*val find : string -> (string * int) list -> tree = <fun>*)
   (*Test cases for find:*)
   let test_find_1 = (find ("v3") ([ ("v1",3) ; ("v2",4) ; ("v3",7) ]) = Con 7)
   let test_find_2 = (find ("v1") ([ ("v1",54) ]) = Con 54);;
   
   (*Converts Var of string to string (Var "v1" -> "v1")
     So that we can compare it with string in the (string*int) list*)
   let str_var v = 
     match v with
     | Var s -> s
     | Plus(_, _) -> "Plus" (*Cases of Plus and Con are set to strings so that comparison in below function does not give error*)
     | Con _ -> "Con";;
   (*val str_var : tree -> string = <fun>*)
   (*Test cases for str_var:*)
   let test_str_var_1 = (str_var (Var "v1") = "v1")
   let test_str_var_2 = (str_var (Plus(Con 4 , Con 5)) = "Plus" )
   let test_str_var_3 = (str_var (Con 4) = "Con");;
   
   (*Evaluates Expression tree*)
   let rec tree_eval (env : (string * int) list) = 
     match env with
     | [] -> (*If env is an empty list, then either Var does not exist in the tree, or if it does, then it can not be numericalized*)
       (function
       | Plus(l, r) -> (tree_eval [] l) + (tree_eval [] r) (*Recursive case to traverse down the tree on both sides - (+) operator builds up to produce final int*)
       | Con x -> x (*Con of int returns the int value in int form*)
       | Var _ -> raise Not_found (*Since env is [], any associating value of Var cannot be found*)
       )
     | (h, _)::t -> 
       (function
       | Plus(l, r) -> 
         (if h = str_var (l) then
           (tree_eval (t) (find (h) (env))) + (tree_eval (t) (r))
         else if h = str_var (r) then
           (tree_eval (t) (l)) + (tree_eval (t) (find (h) (env)))
         else
           (tree_eval (env) (l)) + (tree_eval (env) (r))
         )
       | Con x -> x
       | Var s -> tree_eval (env) (find (s) (env))
       );;
   
       
   let test_q6_1 = (tree_eval ([]) (Con 3) = 3)                  
   let test_q6_2 = (tree_eval ([]) (Plus(Con 3 , Con 4)) = 7)
   let test_q6_3 = (tree_eval ([]) (Plus(Plus(Con 3 , Con 4) , Con 3)) = 10)
   
   let test_q6_4 = (tree_eval ([ ("v1",3) ]) (Var "v1") = 3)
   let test_q6_5 = (tree_eval ([ ("v1",3) ; ("v2",4) ]) (Plus (Var "v1" , Var "v2")) = 7)
   let test_q6_6 = (tree_eval ([ ("v1",7) ; ("v2",3) ; ("v3",5) ]) (Plus(Plus(Var "v1" , Var "v2") , Var "v3")) = 15)
   
   let test_q6_7 = (tree_eval ([ ("v1",3) ]) (Plus(Con 3 , Var "v1")) = 6)
   let test_q6_8 = (tree_eval ([ ("v1",3) ; ("v2",4) ]) (Plus(Plus(Con 3 , Var "v1") , Var "v2")) = 10);;
   
   (* ****************************** End of Question 6 ********************************* *)
   
   
   
   
   (* ****************************** Start of Question 7 ********************************* *)
   
   (**** BONUS Q7: program comprehension/list comprehensions ****)                
   
   (* This question asks you to consider the following function, `scp`, defined in Python *) 
   
   (* 
   * *** start of definition of function `scp` *** *
   
   def scp(xss):
       if [] == xss: return [[]]  
       else: return [ [x] + xs for x in xss[0] for xs in scp(xss[1:]) ]
   
   * *** end of definition of function `scp *** *
   *)
   
   (* Q7a. by considering the possible types of the function `scp`, 
      and its behaviour on suitable values of the correct type(s), 
      describe what `scp` does *)
   
                        (*The scp function joins the elements in one list, to the elements in another list, wrapped in an outer list.
                             For example: scp([ [1,2] , [3] ]) would return [ [1,3] , [2,3] ]
                         This imitates the idea of multiplication of two brackets in mathematics:
                             (1 + 2) * (3) = ((1*3) + (2*3)) 
                         But instead of multiplying, the values are concatenated within a list
                         The scp function is recursive and it requires a' list as an input                     
                         *)
   
   
   
   
   (* Q7b. write an equivalent definition of the function `scp`, in OCaml 
   
                        *** as an OCaml function ***.
   
      NB you may NOT use OCaml extensions that support list comprehension syntax directly.  Answers that do this may score zero marks.
    *)
   
   let rec scp xss = 
     match xss with
     | [] -> [[]] 
     | [[]] -> [] 
     | xs::xss -> List.concat_map (fun x -> List.map (fun xs -> x::xs) (scp xss)) xs;;
     (*The above code can be looked at as:
        List.concat (List.map (fun x -> List.map (fun xs -> x::xs) (scp xss)) (xs))
       as List.concat_map f l is the same as List.concat (List.map f l)
       Looking at the commmented interpretation of the code, we can notice the following:
       - List.concat simply joins the list of lists into a singular list
           So that in the case of [ [1;2] ; [3] ], the first element returned can be [1;3] rather than [1];[3]
       - The outer List.map (fun x) (xs) imitates the loop over x in xss[0]
       - The inner List.map (fun xs) (scp xss) imitates the loop over xs in scp(xss[1:])
     *)
   
   let test_q7_1 = (scp [] = [[]])           
   let test_q7_2 = (scp [[]] = [])
   let test_q7_3 = (scp [ [1] ; [2] ; [3] ; [4] ] = [ [1;2;3;4] ])
   let test_q7_4 = (scp [ [1;2] ; [3] ] = [ [1;3] ; [2;3] ])
   let test_q7_5 = (scp [ [1;2] ; [3;4] ] = [ [1;3] ; [1;4] ; [2;3] ; [2;4] ]);;      
   
   (* ****************************** End of Question 7 ********************************* *)
   
   
   (* ****************************** End of Questions ********************************* *)
   