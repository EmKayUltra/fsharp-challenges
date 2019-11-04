## palindrome
  First coding exercise is the palindrome problem we used to use in interviews at CA:
  Take a string and return an array containing every instance of a palindrome (same front->back and back->front) contained within it that is >1 character in length (including duplicates). The order they are returned in doesn't matter.
  Test cases/examples (again, order doesn't matter):
  "abcba" => ["abcba", "bcb"]
  "cat" => []
  "a" => []
  "aa" => ["aa"]
  "baaab" => ["baaab", "aaa", "aa", "aa"]
  "apopcdada" => ["pop", "dad", "ada"]
  "dadxdad" => ["dadxdad", "dad", "adxda", "dxd", "dad"]


## spiralize
  Given a matrix of m x n elements (m rows, n columns), return all elements of the matrix in spiral order.
  Example 1:
  Input:
  [
  [ 1, 2, 3 ],
  [ 4, 5, 6 ],
  [ 7, 8, 9 ]
  ]
  Output: [1,2,3,6,9,8,7,4,5]
  Example 2:
  Input:
  [
    [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9,10,11,12]
  ]
  Output: [1,2,3,4,8,12,11,10,9,5,6,7]
  Example 3:
  Input:
  [
    [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9,10,11,12],
    [13,14,15,16]
  ]
  Output: [1,2,3,4,8,12,16,15,14,13,9,5,6,7,11,10]
  It's probably okay to assume that the input matrix is rectangular or square. Values in the input can be any single character though, so don't count on them being in ascending numerical order (i.e. 1, 2, 3, etc.). (edited) 

0,0                 0,Y



X,0                 X,Y


0,0->0,Y
0,Y->X,Y
X,Y->X,0
X,0->0+1,0
0+1,0->0,Y-1
0,Y-1->X-1,Y-1
X-1,Y-1->X-1,


or...
more broadly,
draw the outermost box
then the next one
(layer by layer)

[
  [1, 2, 3, 4],
  [5, 6, 7, 8],
  [9,10,11,12]
]


4x3

0,0 0,1 0,2 0,3
1,3 1,0
2,3 2,0

[
  [1, 2, 3, 4],
  [5, 6, 7, 8],
  [9,10,11,12],
  [13,14,15,16]
]

4x4

0,0 0,1 0,2 0,3
1,3 1,0
2,3 2,0
3,3 3,2 3,1 3,0
1,1 1,2

2,2 2,3
3,3 2,3


[
  [1, 2, 3, 4, 0],
  [5, 6, 7, 8, 0],
  [9,10,11,12, 0],
  [9,10,11,12, 0],
  [13,14,15,16,0]
]

spiralize(matrix)
    draw_outside(matrix,0)

get_heads(matrix)
    matrix.map a -> a.head

    
get_tails(matrix)
    matrix.map a -> a.tail

draw_outside(matrix)
    outside = matrix[0] : get_tails(matrix[1]:matrix[matrix.height-1]) : get_heads_matrix(matrix[1]:matrix[matrix.height-1]).reverse() : matrix[matrix.height].reverse

    if (matrix.length < 3 || matrix.height < 3) return outside
    else return outside+draw_outside(matrix_minus_a_layer)



// spiralize
 let test = [
  [1; 2; 3; 4];
  [5; 6; 7; 8];
  [9;10;11;12];
  [13;14;15;16]
]

let get_firsts (matrix:list<list<int>>) = matrix |> List.map (fun (x) -> x.[0]) 
let get_lasts (matrix:list<list<int>>) = matrix |> List.map (fun (x) -> x.[x.Length-1])
let get_submatrix(matrix:list<list<int>>) = if (matrix.Length <= 1 || matrix.[0].Length <=1) matrix; else matrix |> List.
let rec spiralize matrix =
    matrix |> 
        
## candidate numbers
  Given a set of candidate numbers (candidates) (without duplicates) and a target number (target), find all unique combinations in candidates where the candidate numbers sums to target.
  The same repeated number may be chosen from candidates unlimited number of times.
  Note:
  All numbers (including target) will be positive integers.
  The solution set must not contain duplicate combinations.

// candidate numbers
// Example 1:
// Input: candidates = [2,3,6,7], target = 7,
// A solution set is:
// [
//   [7],
//   [2,2,3]
// ]
//
// Example 2:
// Input: candidates = [2,3,5], target = 8,
// A solution set is:
// [
//   [2,2,2,2],
//   [2,3,3],
//   [3,5]
// ]
// candidate_combos = foreach - multiply until just about equal to the target, then add next #

let candidates = [2;3;6;7]
let target = 7

let reduced_candidates = 
  candidates |> 
  List.filter (fun n -> n < target) |>
  List.sort

let multipliers = 
  reduced_candidates |> 
  List.filter (fun n -> target % n = 0) |>
  List.map (fun n -> [ for i in 1 .. (target/n) -> n ])


## binary search tree
  Implement a binary search tree. The original tree should be constructed of nodes with values from a provided list of integers (reject duplicate values, and no sorting it first...the first number in the list is the root node). For the final product, you should be able to:
  1) Build a tree from a list of integers
  2) Given the root node of a tree and a value, insert the value into the tree in the right place (reject it if it already exists)
  3) Given the root node of a tree and a value, delete the value from the tree and keep it intact
  4) Given the root node of a tree, print out every value in the tree in ascending order

  Insert example:
  Given the tree:
          4
        / \
        2   7
      / \
      1   3

  And the value to insert: 5
  You can return this binary search tree:
            4
        /     \
        2        7
      / \      /
      1   3   5

  This tree is also valid:
          5
        /   \
        2     7
      / \   
      1   3
          \
            4


## monty hall
  The Scenario:
  You are given a choice of 3 cases. One contains a fabulous prize, the other two are empty. After you choose a case, one of the other two that is known to be empty is removed.
  You are now given the choice - do you keep the case you chose originally, or do you switch to the other remaining case.
  The Problem:
  Does it matter whether you switch or keep your original choice? Is either outcome mathematically superior?
  Coding:
  Write some code that runs each scenario 10,000 times (i.e. after randomly choosing one of three cases, an empty case you didn't choose is removed, then you either switch or keep the original). Record the win rate of each version, and report your findings here! 