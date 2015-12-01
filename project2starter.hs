-- CPSC 312 - Project 2
-- by Khurram Ali Jaffery

-- Main Components:
-- minimax algorithm
-- a board evaluator
-- state search
-- movement generators (and by extension, tree generator, new state generator)
-- crusher
-- custom data types (already done)

-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--		 W is a piece of the White player
--		 B is a piece of the Black player
--




import Debug.Trace


data Piece = D | W | B deriving (Eq, Show)

--
-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
--

-- changing the point type.
type Point =  (Int, Int)

-- Tile is a tuple of 2 elements 
-- representing what a point is occupied by
-- where the first element represents a piece 
--       the second element represents a point
--

type Tile  = (Piece, Point)

--
-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
--

type Board = [Piece]

--
-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate 
-- system to easily maintain and make moves on the board
--

type Grid = [Point]

--
-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
--

type State = [Tile]

--
-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
-- 
-- Next consists of 4 elements
-- where usedDepth is an integer reprsenting the current depth level
--		 newBoard is the next board to add to the tree
-- 		 seenBoards is the updated history to avoid possible future trouble boards
-- 		 cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of 
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
-- 		 board is the game state at that node
-- 		 nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move over
--		 the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
-- 		 the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--		 is that a jump can be reduced to a move as in effect 
--		 nothing happens the point moved over in a jump
--

type Move = (Point,Point)

--
-- Some test results to see what functions are producing 
--
--run = crusher ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"] 'W' 2 3

grid0 = generateGrid 4 2 4 []  


--if the n == y then we get rid of one of the cases... 
--	we get rid of the - - or the + + and the same goes for x...


-- can have things on the grid that are unreachable from a point.

--slides0 = generateSlides grid0 3  
slides0 = generate_Slides grid0 3    -- this is going to test the new slide function...

jumps = generateLeaps grid0 3 slides0 
 								

board0 = sTrToBoard "WWW-WW-------BB-BBB"
--newBoards0 = generateNewStates board0 [] grid0 slides0 jumps0 W
--tree0 = generateTree board0 [] grid0 slides0 jumps0 W 4 3
--heuristic0 = boardEvaluator W [] 3

--
-- crusher
--
-- This function consumes a list of boards, a player, the depth of 
-- search tree, the size of the provide boards, and produces the 
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--

--crusher :: [String] -> Char -> Int -> Int -> [String]
--crusher (current:old) p d n = -- To Be Completed



--
-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, otherwise False
-- type Board = [Piece]
-- data Piece = D | W | B deriving (Eq, Show)

--		 [(0,0),(1,0),(2,0)
--	   (0,1),(1,1),(2,1),(3,1)
--	(0,2),(1,2),(2,2),(3,2),(4,2)
--	   (0,3),(1,3),(2,3),(3,3)
--		 (0,4),(1,4),(2,4)]

gameOver0 = gameOver ([W,W,W,
					 D,D,D,D,
					D,D,D,D,D,
				 	 D,D,D,D,
				  	  B,B,B]) 
				([])  
				(3)  

gameOver1 = gameOver (		[D,D,D,
				 	  	    D,B,D,W,
					 	   D,B,D,D,D,  
 				 	  		D,D,D,D,
				       		 D,D,B]      )
						([  
							([W,W,W,
				 	  		 D,D,D,D,
					 		D,D,D,D,D,  
				 	  		 D,D,D,D,
				       		  B,B,B]),

							([D,W,W,
				 	  	    D,W,D,D,
					 	   D,D,D,D,D,	
				 	  		D,D,D,D,
				       		 B,B,B]),

							([D,W,W,
				 	  	    D,W,D,D,
					 	   D,D,D,D,D,  
				 	  		D,B,D,D,
				       		 D,B,B]),
							
							([D,W,W,
				 	  	    D,D,D,D,
					 	   D,W,D,D,D,  
				 	  		D,B,D,D,
				       		 D,B,B]),
							
							([D,W,W,
				 	  	    D,D,D,D,
					 	   D,B,D,D,D,  
				 	  		D,B,D,D,
				       		 D,D,B]),

							([D,D,W,
				 	  	    D,W,D,D,
					 	   D,B,D,D,D,  
				 	  		D,B,D,D,
				       		 D,D,B]),
							
							([D,D,W,
				 	  	    D,W,D,D,
					 	   D,B,D,D,D,  
 				 	  		B,D,D,D,
				       		 D,D,B]),
							
							([D,D,D,
				 	  	    D,W,D,W,
					 	   D,B,D,D,D,  
 				 	  		B,D,D,D,
				       		 D,D,B])
							])  
						(3)  

-- this one should check a board that has been seen.
gameOver3 = 	gameOver		([W,W,W,
				 	  	    	D,D,D,D,
					 	   	    D,D,D,D,D,  
 				 	  			D,D,D,D,
				       			 B,B,B]      )	
			([  
							([W,W,W,
				 	  		 D,D,D,D,
					 		D,D,D,D,D,  
				 	  		 D,D,D,D,
				       		  B,B,B]),

							([D,W,W,
				 	  	    D,W,D,D,
					 	   D,D,D,D,D,	
				 	  		D,D,D,D,
				       		 B,B,B]),

							([D,W,W,
				 	  	    D,W,D,D,
					 	   D,D,D,D,D,  
				 	  		D,B,D,D,
				       		 D,B,B]),
							
							([D,W,W,
				 	  	    D,D,D,D,
					 	   D,W,D,D,D,  
				 	  		D,B,D,D,
				       		 D,B,B]),
							
							([D,W,W,
				 	  	    D,D,D,D,
					 	   D,B,D,D,D,  
				 	  		D,B,D,D,
				       		 D,D,B]),

							([D,D,W,
				 	  	    D,W,D,D,
					 	   D,B,D,D,D,  
				 	  		D,B,D,D,
				       		 D,D,B]),
							
							([D,D,W,
				 	  	    D,W,D,D,
					 	   D,B,D,D,D,  
 				 	  		B,D,D,D,
				       		 D,D,B]),
							
							([D,D,D,
				 	  	    D,W,D,W,
					 	   D,B,D,D,D,  
 				 	  		B,D,D,D,
				       		 D,D,B])
							])  
						(3)  

 --gameOver :: Board -> [Board] -> Int -> Bool
-- gameOver recentB prevB n = True  




gameOver :: Board -> [Board] -> Int -> Bool
gameOver board loboards n = gameOver_players (board) (0) (0) (n) ||   -- there is going to be an or here ||   
							gameOver_seen_it (board) loboards 
-- now we need to figure out the second part which is if the other person has any moves left...
-- algo... :
-- 			1.) we just check whether the current board is in the history of boards...


-- takes in recent board, and history of board  
-- checks whether that board is located in that one...
gameOver_seen_it :: Board -> [Board] -> Bool
gameOver_seen_it cur_board (a:ax)
			| ax == [] = False -- that means we could not find a board
			| cur_board == a = True  -- we have found a matching board thus the game must end..
			| otherwise = gameOver_seen_it (cur_board) (ax)  -- recurse...

-- helper function that checks whether the current board has enough players.
-- function takes the board we have two accumulators that checks the number of certain players seen also has the number the board started with
-- returns a boolean...



gameOver_players :: Board -> Int -> Int -> Int ->  Bool
gameOver_players (a:ax) acc_bl acc_w n
	| (ax) == [] = if (fromIntegral(acc_bl) < (fromIntegral(n) / 2.0))
						then True
						else if (fromIntegral(acc_w) <  (fromIntegral(n) / 2.0))   -- this is not working??
							then True 
							else False 
	| otherwise = if a == D then gameOver_players (ax) (acc_bl) (acc_w) (n)
								else if a == W 
									then gameOver_players (ax) (acc_bl) (acc_w + 1) (n)
									else if a == B 
										then gameOver_players (ax) (acc_bl + 1) (acc_w) (n)
										else False


			-- trace ("value of n div 2 " ++ show acc_w) False							
-- lets count up everything first and then have our if statements at the end... 



-- three instances where the game can be ended... 
-- 1.) 
-- two ways a player can win this game...
-- 1.) removed more then half of the other players pieces...
-- 2.) if its someones turn and he cannot make a legal move.
	--  legal move : 

-- algo : 
-- 			1.) go through the board and check whether the board has atleast n amount of each player... 
--			2.) go through the board and check if there are moves that are available for each point...



--
-- sTrToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation

-- Note: This function would convert "WWW-WW-------BB-BBB" to
-- 	     [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--
-- Returns: the Board corresponding to the string
--

sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
	where 
		check 'W' = W
		check 'B' = B
		check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and 
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Note: This function would convert [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B] 
-- 	     to "WWW-WW-------BB-BBB"
--
-- Returns: the String corresponding to the board 
--

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
	where 
		check W = 'W'
		check B = 'B'
		check D = '-'

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid 
--		   initialized to []
--
-- Note: This function on being passed 3 2 4 [] would produce

	  


--
-- Returns: the corresponding Grid i.e the acc when n3 == -1
--

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
	| n3 == -1		= acc
	| otherwise 	= generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
		where
			row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]  -- all x's for the current y
			nn1 = if n2 > 0 then n1 + 1 else n1 - 1

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible slides is only generated once; and when 
-- 		 generating next moves, the program decides which slides out of 
--		 all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
-- Grid [Point] a list of points
-- Point = (int,int)
-- Slide = (Point,Point)


----- >>> this is my second try at the board evaluator
generate_Slides :: Grid -> Int -> [Slide]   -- this should be returning [Slide]
generate_Slides b n = generateSlideshelper2 b b [] n  -- input the grid twice... 

generateSlideshelper2 :: Grid -> Grid ->[Slide] -> Int -> [Slide]
generateSlideshelper2 grid (a:ax) acc n -- one grid is kept in tact the other one is traversed.
			| ax == [] =  acc  -- THERE IS STILL ONE MORE EXECUTION HERE POSSIBLY NEED TO CEHCK THIS...
			| otherwise = generateSlideshelper2 (grid) (ax) (merge (generateSlideshelper (grid)(a)(n)) (acc)) n


-- this function is going to find all adjacent points for a
generateSlideshelper :: Grid -> Point -> Int -> [Slide]
generateSlideshelper grid (x,y) n = if y == n
										then  point_maker1 grid (x,y)-- we use N,NW,SW,S,E,W which is (x-1, y-1) and ()
										else if y < n
											then point_maker2 grid (x,y)-- N,NW,S,SE,E,W
											else point_maker3 grid (x,y)-- N, NE, S,SW, E,W


-- the first case
point_maker1 :: Grid -> Point-> [Slide]
point_maker1 grid (p1,p2) =			
 			map (\x -> (((p1,p2),(x))))  -- this creates the slides...
			(filter (\x -> elem x grid)([(p1,p2-1),(p1-1,p2-1),(p1-1,p2+1),(p1,p2+1),(p1+1,p2),(p1-1,p2)]))
										-- N          NW  		SW 				S        E        W

-- the second case...
point_maker2 :: Grid -> Point-> [Slide]
point_maker2 grid (p1,p2) =			
 			map (\x -> (((p1,p2),(x))))  -- this creates the slides...
			(filter (\x -> elem x grid)([(p1,p2-1),(p1-1,p2-1),(p1,p2+1),(p1+1,p2+1),(p1+1,p2),(p1-1,p2)]))
										-- N          NW  		S				SE        E        W

-- the third case
point_maker3 :: Grid -> Point-> [Slide]
point_maker3 grid (p1,p2) =			
 			map (\x -> (((p1,p2),(x))))  
			(filter (\x -> elem x grid)([(p1,p2-1),(p1+1,p2-1),(p1,p2+1),(p1-1,p2+1),(p1+1,p2),(p1-1,p2)]))
										-- N          NE 		S 				SW        E        W

test :: Grid -> Point -> [Point]
test grid (p1,p2) = (filter (\x -> elem x grid) ([(p1-1,p2+1),(p1+1,p2),(p1,p2+1),(p1+1,p2-1),(p1-1,p2),(p1,p2-1)]))
				

merge :: Eq(a) => [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs	






{-N = x, y-1
NW = x-1, y-1 
NE = x+1, y-1
S = x, y+1 
SW = x-1, y+1
SE = x+1, y+1
E = x+1, y
W = x-1, y 
-}







--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible leaps is only generated once; and when 
-- 		 generating next moves, the program decides which leaps out of 
--		 all these possible leaps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--


-- we should use this for gameover...
-- gameover when you reach the end of a certain grid tree.
-- first function to work on... 
				
-- Grid [Point] a list of points
-- Point = (int,int)
-- Slide = (Point,Point)
-- type Jump = (Point,Point,Point)  
-- 				starting , jumping over, End point.

-- just try to find the correct slides for every grid point first...

generateLeaps :: Grid -> Int -> [Slide] -> [Jump]
generateLeaps b n slides = generateLeaps_helper b b slides []

generateLeaps_helper :: Grid -> Grid -> [Slide] -> [Jump]-> [Jump]
generateLeaps_helper grid (a:ax) slides acc
			| ax == [] = acc  -- check that we have reached the eend here...
			| otherwise = generateLeaps_helper grid (ax) (slides) (merge (generateLeaps_helper2 grid (a) (slides) []) (acc))
 			

-- going to take the current grid point and find every corresponding slide,
-- then with every corresponding slide we need to find the jump...
generateLeaps_helper2 :: Grid -> Point -> [Slide] -> [Slide] -> [Jump]  
generateLeaps_helper2 grid point slides acc = 
	-- need to filter...
	jump_generator (grid) (point) (filter (\(p1,p2) -> p1 == point) (slides))  -- take the list of slides and find the point.

-- takes in a slide for the point.
-- for each slide 
jump_generator :: Grid -> Point -> [Slide] -> [Jump]
jump_generator grid point slides = 
  filter  (\(po1,po2,po3) -> elem (po3)(grid)) (map (\(p1,p2) -> (p1, p2, (determine (grid) (p1,p2)))) (slides)) 

determine :: Grid -> Slide -> Point
determine grid ((a,b),(c,d))
	-- first we need to determine the direction... 
		| (c-a) == -1 && (d-b) == -1 = (c-1,d-1)  -- that is the point that we return... NorthWest
		| (c-a) == -1 && (d-b) == 0 = (c-1,d) -- West  
		| (c-a) == -1 && (d-b) == 1 = (c-1,d+1) -- SouthWest
		| (c-a) == 0 && (d-b) == 1 = (c,d+1)-- South
		| (c-a) == 1 && (d-b) == 1 = (c+1,d+1)-- SouthEast
		| (c-a) == 1 && (d-b) == 0 = (c+1,d)-- East
		| (c-a) == 1 && (d-b) == -1 = (c+1,d-1)-- NorthEast 
		| (c-a) == 0 && (d-b) == -1 = (c,d-1)-- North 
		| otherwise = (99,99)   

--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the 
-- board, else generate a search tree till the specified depth and apply 
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over, 
--          otherwise produces the next best board
--

--stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
--stateSearch board history grid slides jumps player depth num = -- To Be Completed

--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--


board1 = 					[D,D,D,
				 	  	    D,B,D,W,
					 	   D,B,D,D,D,  
 				 	  		D,D,D,D,
				       		 D,D,B]        -- board we can use.

tree0 = generateTree (		[D,D,D,
				 	  	    D,B,D,W,
					 	   D,B,D,D,D,  
 				 	  		D,D,D,D,
				       		 D,D,B]      )  -- the most recent board
				([  
							([W,W,W,
				 	  		 D,D,D,D,
					 		D,D,D,D,D,  
				 	  		 D,D,D,D,
				       		  B,B,B]),

							([D,W,W,
				 	  	    D,W,D,D,
					 	   D,D,D,D,D,	
				 	  		D,D,D,D,
				       		 B,B,B]),

							([D,W,W,
				 	  	    D,W,D,D,
					 	   D,D,D,D,D,  
				 	  		D,B,D,D,
				       		 D,B,B]),
							
							([D,W,W,
				 	  	    D,D,D,D,
					 	   D,W,D,D,D,  
				 	  		D,B,D,D,
				       		 D,B,B]),
							
							([D,W,W,
				 	  	    D,D,D,D,
					 	   D,B,D,D,D,  
				 	  		D,B,D,D,
				       		 D,D,B]),

							([D,D,W,
				 	  	    D,W,D,D,
					 	   D,B,D,D,D,  
				 	  		D,B,D,D,
				       		 D,D,B]),
							
							([D,D,W,
				 	  	    D,W,D,D,
					 	   D,B,D,D,D,  
 				 	  		B,D,D,D,
				       		 D,D,B]),
							
							([D,D,D,
				 	  	    D,W,D,W,
					 	   D,B,D,D,D,  
 				 	  		B,D,D,D,
				       		 D,D,B])
							])   -- history of boards...
						(grid0)  -- the current grid that is being played.
						(slides0)	-- the list of slides\...
						(jumps)		-- the list of jumps
						(W)  -- the player the program is
						(1)  -- just check that we only have one depth for nwo...
						(3)  -- int representing the size of the board...						






-- data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)
-- type BoardTree = Tree Board 
-- type Board = [Piece]

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n = Node 1 board1 []


--{depth = 1, board = board1, nextBoards = loboards} 










--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate 
-- a list of next boards, and then checks whether or not that move would 
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

--generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
--generateNewStates board history grid slides jumps player = -- To Be Completed
	
--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps, 
-- a list of possible slides and a player from whose perspective 
-- to generate moves, to check which of these jumps and slides 
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Note: This is the only instance where the program makes use of the
--		 type State, for our purposes it is zipping the board and the
--		 grid together for making it easier to make moves.
--
-- Note:
-- -- oP is opponentsPieces
-- -- pP is playersPieces
-- -- vS is validSlides
-- -- vJ is validJumps
--
-- Returns: the list of all valid moves that the player could make
-- type Tile  = (Piece, Point)  
-- type State = [Tile]		 
-- data Piece = D | W | B deriving (Eq, Show) -- D means empty.
--type Move = (Point,Point)
-- State = [(D, (0, 1)),(W, (0, 1)) ]  -- gives the state of the game...

--		 [(0,0),(1,0),(2,0)
--	   (0,1),(1,1),(2,1),(3,1)
--	(0,2),(1,2),(2,2),(3,2),(4,2)
--	   (0,3),(1,3),(2,3),(3,3)
--		 (0,4),(1,4),(2,4)]

-- lets make something we can test here... 
move = moveGenerator ([(W, (0,0)),(W, (1,0)),(W, (2,0)),  
					(D, (0,1)),(D, (1,1)),(D, (2,1)),(D,(3,1)), 
				(D, (0,2)),(D, (1,2)),(D, (2,2)),(D,(3,2)),(D,(4,2)), 
					(D, (0,3)),(D, (1,3)),(D, (2,3)),(D,(3,3)), 
						(B, (0,4)),(B, (1,4)),(B, (2,4))])
					(slides0)	
					(jumps)
					(W)
-- tests for potential jumps...
move1 = moveGenerator(

	[					(D, (0,0)),(W, (1,0)),(W, (2,0)),  
					(D, (0,1)),(D, (1,1)),(D, (2,1)),(D,(3,1)), 
				(D, (0,2)),(W, (1,2)),(D, (2,2)),(D,(3,2)),(D,(4,2)), 
					(B, (0,3)),(D, (1,3)),(D, (2,3)),(D,(3,3)), 
						(D, (0,4)),(B, (1,4)),(B, (2,4))]

	) (slides0) (jumps) (B) 

-- TODO... the jumps doesnt check where the piece needs to be behind...

moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player = moveGenerator_helper state state slides jumps player []

-- we are going to go through states here...
moveGenerator_helper :: State -> State -> [Slide] -> [Jump] -> Piece -> [Move] -> [Move]
moveGenerator_helper state ((piece,(p1,p2)):ax) slides jumps player acc
		| ax == [] = acc 
		| otherwise = if piece == player -- if this tile has a piece that is part of players turn then we add to the moves lsit...
						then moveGenerator_helper (state) (ax) (slides) (jumps) (player) (merge (create_moves player state ((p1,p2)) (jumps) (slides) ([])) (acc))
						else  moveGenerator_helper (state) (ax) (slides) (jumps) (player) (acc) -- here we are not going to add to acc and just recurse


-- returns a list of moves for that certain point...
--  takes Point, lo jumps, lo Slides,
-- just see if this will compile first...
create_moves:: Piece -> State -> Point->[Jump]->[Slide] ->[Move] -> [Move]  
create_moves player state p jmps slides acc =  merge (map (\(a,b,c)-> (a,c)) 
								(filter (\(a,b,c) -> a == p && (piece_checkers (state) (b) (player))) (jmps)))  -- we need a further filter to see if those jumps can be made... 
										(filter(\(a,b)-> a==p)(slides)) -- merges both the moves that will be creates from the filter...

-- this has to return a boolean..
-- checks whether the current has the opposite value of Piece so that we can add it as jump...
-- algo - first find the point in state that we are looking at...
		-- then check whether that point has the opposite of player... 
piece_checkers:: State -> Point -> Piece -> Bool
piece_checkers state point player = piece_checker_helper (filter (\(pl,p)-> point == p)(state))  (point) (player)


-- takes in the tile that has the same coordinate as the one we inserted,
-- takes in the point of player b although we dont need it... 
-- takes the current players turn...
piece_checker_helper :: State -> Point -> Piece -> Bool
piece_checker_helper ((pl,p):ax) point player 
				| player == W && pl == B = True   -- basically if they are opposites then the move can be made...
				| player == B && pl == W = True   -- if they are opposites...
				| otherwise = False


-- the jumps might not be valid because there is no black piece there... we need to check

--((e,f):ex)
--((a,b,c):ax)
		-- we want to filters going into anotehr function that is going to create the list of moves.
		-- first map 
			-- 
			-- list to go through is going to be jumps
	--algo -- extract all the jumps where the origin starts at t and add it to acc... 
		-- extract all the slides where the origin starts at t and add it to acc...
	-- we have to go through jumps and we have to go through slides...

--we are going to merge the moves from jmps and the moves from slides then to moes...


--[((1,0),(1,2)), ((1,0),(1,1))]  -- from one point to another


{-
type Jump = (Point,Point,Point)
algo
1.) go through the elements within state.
	- for every piece we check if there is a Piece there...
		- if there is then we generate moves for it, 
		
	 - add all the appropriate slides and add to the moves list...
		- else we just keep on moving...



-}
-- To Be Completed										 

--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by 
-- taking into account whose perspective the program is playing from, the list 
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and 
-- accordingly produce a goodness value of the given board 
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--

-- just make a simple one for now.
--boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
--boardEvaluator player history n board myTurn = 



-- if you have won then board value + 10... 
-- else if oppoen has won then board value - 10... 
-- else board value = number of pawns - number of opponents pawns...





-- To Be Completed

--
-- minimax
--
-- This function implements the minimax algorithm, it consumes a search tree, 
-- and an appropriate heuristic to apply to the tree, by applying minimax it
-- produces the next best board that the program should make a move to
--
-- Arguments:
-- -- (Node _ b children): a BoardTree to apply minimax algorithm on
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
--
-- Returns: the next best board
--

--minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
--minimax (Node _ b children) heuristic = -- To Be Completed

--
-- minimax'
--
-- This function is a helper to the actual minimax function, it consumes 
-- a search tree, an appropriate heuristic to apply to the leaf nodes of 
-- the tree, and based on whether it would have been the maximizing 
-- player's turn, it accordingly propogates the values upwards until
-- it reaches the top to the base node, and produces that value.
--
-- Arguments:
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
-- 				 or miniziming the goodness values of its children
--
-- Returns: the minimax value at the top of the tree
--

--minimax' :: BoardTree -> (Board -> Bool -> Int) -> Bool -> Int
--minimax' boardTree heuristic maxPlayer = -- To Be Completed


