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


data Piece = D | W | B deriving (Eq, Show, Read)  -- read should make it that Piece would work with 'C'

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

grid0 = generateGrid 3 2 4 []  


--if the n == y then we get rid of one of the cases... 
--	we get rid of the - - or the + + and the same goes for x...


-- can have things on the grid that are unreachable from a point.

--slides0 = generateSlides grid0 3  
slides0 = generateSlides grid0 3    -- this is going to test the new slide function...

--jumps = generateLeaps grid0 3 slides0 
jumps = generateLeaps grid0 3 								

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

-- the first argument is either current or old and represents the most recent board,
-- the second argument is a piece representing the player the program is
-- d the depth seasrch of the search tree
-- n dimensions of the baord
-- returns a list of String with the new current board.... 
--sTrToBoard :: String  -> Board
--boardToStr :: Board -> String
--minimax :: BoardTree -> (Piece -> Board -> Int) -> Board
--generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
crushTest = crusher(["-----------BWW", "-WWW----------B--BB","----W-W--------B--BB"]) ('W') (1) (3)
-- this is going to be the first move of crusher...
crushTest1 = crusher(["WWW-WW-------BB-BBB"])('W')(1)(3)  
-- the output should be crusher (['WWW'])  with the move that minimax gave...

-- we need to generate a tree
-- convert char into piece 



-- we should test this after we know minimax is working... 
--minimax :: BoardTree -> (Piece -> Board -> Int) -> Board
testPlay = play ["WWW-WW-------BB-BBB"] 'W' 1 3
testPlay2 = play ["WWW-WW-------BB-BBB"] 'W' 2 3  
testPlay3 = play ["WWW-WW-------BB-BBB"] 'W' 3 3 


play :: [String] -> Char -> Int -> Int -> IO ()
play history@(current:old) player depth n
  | gameOver (char_to_piece player) (grid0) (jumps) (slides0) (sTrToBoard current) (map sTrToBoard old) (n) = putStrLn "Game over."
  | otherwise = do 
       let history'@(new:_) = crusher history player depth n
       putStrLn $ player:" played: " ++ new
       play history' (if player == 'W' then 'B' else 'W') depth n

char_to_piece ::Char -> Piece 
char_to_piece c 
		| c == 'B' = B
		| c == 'W' = W
		| otherwise = D

--gameOver :: Piece -> Grid -> [Jump] -> [Slide]-> Board -> [Board] -> Int -> Bool 
--gameOver piece grid jump slides board history n 

testCrusher = crusher ["WWW-W-----B--BB-B-B", "WWW-W-----W--BB-BBB", "WWW-WW-------BB-BBB"] 'W' 1 3  -- this is giving nothin as output...



crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) p d n = 
						-- this thing gives us the best move based on the current board and now we add it...
					[(boardToStr	(minimax 	
									(generateTree (sTrToBoard current) 
										(sTrToBoard_list old) 
										(grid0) 
										(slides0) 
										(jumps) 
										(char_to_piece p)   -- we need minimax to take in another function...
										 (d)
										  (n)) 
									(boardEvaluator2)
									(char_to_piece p) -- this is going to tell the mini max function where its at.
									))]  
											++ (current:old) -- this should work?


--crusher_helper::  
--crusher_helper

--algo-- 
	-- 1.) for the most recent board generate a tree for the specified depth
	--		i.) convert the current board from a string to a tyoe board
	-- 		ii.) convert the list of boards into a list of baords
	-- 			iii.) we already haev the grid, slides, jump 
	-- 		iv.) we also have to input the player that the person is
	-- 		v.)  input the depth as well as the dimensions of the board...		
	-- 2.) use the tree to find the best board using minimax function. for the current board.
	-- 3.) once we find that board turn it into a string and place it onto the front of the list of boards.
	--[("its working"),("hopefully")]


sTrToBoard_list :: [String] -> [Board]
sTrToBoard_list loString = (map (\str -> sTrToBoard str)(loString))
-- this is failing right now?
		

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


-- should rreturn true


-- this one should check a board that has been seen.
{-gameOver3 = 	gameOver		(W)
								([W,D,D,
				 	  	    	D,D,D,D,
					 	   	    D,D,D,D,D,  
 				 	  			D,D,D,D,
				       			 D,D,D])	
			([  
							([D,W,D,
				 	  		 D,D,D,D,
					 		D,D,D,D,D,  
				 	  		 D,D,D,D,
				       		  D,D,D]),

							([D,D,D,
				 	  	    D,W,D,D,
					 	   D,D,D,D,D,	
				 	  		D,D,D,D,
				       		 D,D,D]),

							([D,D,D,
				 	  	    W,D,D,D,
					 	   D,D,D,D,D,  
				 	  		D,D,D,D,
				       		 D,D,D]),
							
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
-}
 --gameOver :: Board -> [Board] -> Int -> Bool
-- gameOver recentB prevB n = True  

--gameOver_players2 (board) (n) || 

	

gameOver :: Piece -> Grid -> [Jump] -> [Slide]-> Board -> [Board] -> Int -> Bool 
gameOver piece grid jump slides board history n = if (
								(gameOver_seen_it2 (board) (history)

															(movestoBoard (piece) 
																	(boardtoState board grid [])

																(moveGenerator 	(boardtoState (board)(grid)([]))
																			(slides)
																			(jump)
																			(piece)

																			)
																(history)
																([]))
																				)
															||
									(gameOver_players2 (board) (n))
															)
							then True
							else False
							
								
--moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
-- now we need to figure out the second part which is if the other person has any moves left...
-- algo... :
-- 			1.) we just check whether the current board is in the history of boards...

gameOver_seen_it2 :: Board -> [Board]-> [Board] -> Bool 
gameOver_seen_it2 board history lomoves = if (intersect_Over lomoves history == lomoves)
												then True
												else False


intersect_Over :: [Board] -> [Board] -> [Board]
intersect_Over a b = intersection_Over a b []

-- intersect's helper
intersection_Over :: [Board] -> [Board] -> [Board] -> [Board]
intersection_Over a b c
  | null a = c
  | null b = []
  | elem (head a) b = (intersection_Over (tail a) b (c++[(head a)]))
  | otherwise = intersection_Over (tail a) b c



gameOver_seen_it :: Board -> [Board] -> Bool
gameOver_seen_it cur_board (loboards)  
			| loboards == [] = False  
			| tail loboards == [] = if cur_board == head loboards
							then True
							else False
			| cur_board == head loboards = True  -- we hav fe found a matching board thus the game must end..
			| otherwise = gameOver_seen_it (cur_board) (tail loboards)  -- recurse...

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


gameOver_players2 :: Board -> Int -> Bool
gameOver_players2 board n = 
				if ((countPiecesB board 0 < n) || (countPiecesW board 0 < n))
								then True
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

--		 [(0,0),(1,0),(2,0)
--	   (0,1),(1,1),(2,1),(3,1)
--	(0,2),(1,2),(2,2),(3,2),(4,2)
--	   (0,3),(1,3),(2,3),(3,3)
--		 (0,4),(1,4),(2,4)]


generateSlides :: Grid -> Int -> [Slide]
generateSlides [] n = []
generateSlides (p:ps) n 
  | otherwise = (map (\x -> (p,x)) (neighbour_space p n))++(generateSlides ps n)

-- Use to test the returning Slides
-- Input is the dimension of the board (n)
generateSlidesTest :: Int -> [Slide]
generateSlidesTest n = generateSlides g n
	where
		g = generateGrid n (n-1) (2*n-2) [] 

oddNatural = [1,3..21]
evenNatural = [2,4..20]

-- Finds the Neighbour Spaces to Slide to, based abstractly 
-- on the picture of the board in Project Description
-- removed (elem n oddNatural)&&
neighbour_space :: Point -> Int -> [Point]
neighbour_space p n 
  -- odd size rows above the middle row
  | (elem ((snd(p))+1) evenNatural)&&((n-1) < (snd(p)))
	= (intersect filt [(u,v)|(u,v)<-[((u-1),(v+1)),(u,(v+1)),((u-1),v),((u+1),v),(u,(v-1)),((u+1),(v-1))]])
  -- odd size rows below the middle row
  | (elem ((snd(p))+1) evenNatural)&&((n-1) > (snd(p)))
    = (intersect filt [(u,v)|(u,v)<-[(u,(v+1)),((u+1),(v+1)),((u-1),v),((u+1),v),((u-1),(v-1)),(u,(v-1))]])
  -- even sized rows above the middle row  
  | (elem ((snd(p))+1) oddNatural)&&((n-1) < (snd(p)))
	= (intersect filt [(u,v)|(u,v)<-[((u-1),(v+1)),(u,(v+1)),((u-1),v),((u+1),v),(u,(v-1)),((u+1),(v-1))]])
  -- even sized rows below the middle row	
  | (elem ((snd(p))+1) oddNatural)&&((n-1) > (snd(p)))
    = (intersect filt [(u,v)|(u,v)<-[(u,(v+1)),((u+1),(v+1)),((u-1),v),((u+1),v),((u-1),(v-1)),(u,(v-1))]])
  -- middle row (largest length)
  | (elem ((snd(p))+1) oddNatural)&&((n-1) == (snd(p)))
	= (intersect filt [(u,v)|(u,v)<-[((u-1),(v+1)),(u,(v+1)),((u-1),v),((u+1),v),(u,(v-1)),((u-1),(v-1))]])  
	where 
		filt =[(x,y)|(x,y)<-(grid_filter p n)]
		u = fst(p)
		v = snd(p)
		
-- Finds the Intersect of the General Space and the Generated Grid with the Center Point removed
grid_filter :: Point -> Int -> [Point]
grid_filter p n = remove_center p (intersect g gen)
	where
		g = generateGrid n (n-1) (2*n-2) [] 
		gen = general_space p
		
-- Creates the list of coordinates around the given center value without bounds (inclusive)
general_space :: Point -> [Point]
general_space p = [(j,k)|j<-[f, f-1, f+1], k<-[s-1, s, s+1]]	
		where
		f = fst(p)
		s = snd(p)
		
-- removes a point from the given subgrid (desire is to remove center point) 	 
remove_center :: Point -> [Point] -> [Point]
remove_center c [] = []
remove_center c (p:ps)
  | c==p = remove_center c ps
  | otherwise = p:(remove_center c ps)  
  
-- finds the Intersection of two lists
intersect :: [Point] -> [Point] -> [Point]
intersect a b = intersection a b []

-- intersect's helper
intersection :: [Point] -> [Point] -> [Point] -> [Point]
intersection a b c
  | null a = c
  | null b = []
  | elem (head a) b = (intersection (tail a) b (c++[(head a)]))
  | otherwise = intersection (tail a) b c



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



generateLeaps :: Grid -> Int -> [Jump]
generateLeaps [] n = []
generateLeaps (p:ps) n = convertJumpLists(removeIncompleteJump(generateJumpLists (p:ps) n))

-- test that this works. input is dimension (n) of the board
generateLeapsTest :: Int -> [Jump]
generateLeapsTest n = convertJumpLists(removeIncompleteJump(generateJumpLists (generateGrid n (n-1) (2*n-2) []) n))

-- assume that the list has length 3. Converts into Jump type
convertJumpLists :: [[Point]] -> [Jump]
convertJumpLists [] = []
convertJumpLists (p:ps) = (convertJumpList p):(convertJumpLists ps) 

-- converts a list that denotes a jump to a jump type
convertJumpList :: [Point] -> Jump
convertJumpList ps = (x,y,z)
	where
		x=head ps 
		y=head(tail ps) 
		z=head(tail (tail ps))

-- Removes all incomplete jumps
removeIncompleteJump :: [[Point]] -> [[Point]]
removeIncompleteJump [] = [] 
removeIncompleteJump (p:ps)
  | length(p)==3 = p:(removeIncompleteJump (ps))
  | otherwise = removeIncompleteJump (ps)

-- creates a appended list of jumps for given points
generateJumpLists :: [Point]-> Int -> [[Point]]
generateJumpLists [] n = []
generateJumpLists (p:ps) n = (generateJumpList p n)++(generateJumpLists ps n)		
	
-- creates a list of "jumps"- (complete or incomplete) for a given point (in all six directions)
generateJumpList :: Point -> Int -> [[Point]]
generateJumpList p n
  | (snd(p)) < (n-1) =
                   [(p:(northwestTop p n))++(generateFinalJumpPointNwT (northwestTop p n) n)] ++
                   [(p:(northeastTop p n))++(generateFinalJumpPointNeT (northeastTop p n) n)] ++
				   [(p:(east p n))++(generateFinalJumpPointE (east p n) n)] ++
				   [(p:(southeastTop p n))++(generateFinalJumpPointSeT (southeastTop p n) n)] ++
				   [(p:(southwestTop p n))++(generateFinalJumpPointSwT (southwestTop p n) n)] ++
				   [(p:(west p n))++(generateFinalJumpPointW (west p n) n)]
				   
  | (snd(p)) > (n-1) = 
                   [(p:(northwestBottom p n))++(generateFinalJumpPointNwB (northwestBottom p n) n)] ++
                   [(p:(northeastBottom p n))++(generateFinalJumpPointNeB (northeastBottom p n) n)] ++
				   [(p:(east p n))++(generateFinalJumpPointE (east p n) n)] ++
				   [(p:(southeastBottom p n))++(generateFinalJumpPointSeB (southeastBottom p n) n)] ++
				   [(p:(southwestBottom p n))++(generateFinalJumpPointSwB (southwestBottom p n) n)] ++
				   [(p:(west p n))++(generateFinalJumpPointW (west p n) n)]
				   
  | (snd(p)) ==(n-1) = 
                   [(p:(northwestTop p n))++(generateFinalJumpPointNwT (northwestTop p n) n)] ++
                   [(p:(northeastTop p n))++(generateFinalJumpPointNeT (northeastTop p n) n)] ++
				   [(p:(east p n))++(generateFinalJumpPointE (east p n) n)] ++
				   [(p:(southeastBottom p n))++(generateFinalJumpPointSeB (southeastBottom p n) n)] ++
				   [(p:(southwestBottom p n))++(generateFinalJumpPointSwB (southwestBottom p n) n)] ++
				   [(p:(west p n))++(generateFinalJumpPointW (west p n) n)]
				   
-- Given an intermediate jump point, finds the FINAL jump point. 
--- For the starting points ABOVE the Middle Row
generateFinalJumpPointNwT :: [Point] -> Int -> [Point]
generateFinalJumpPointNwT [] n = []
generateFinalJumpPointNwT ps n = (northwestTop (head ps) n)

generateFinalJumpPointNeT :: [Point] -> Int -> [Point]
generateFinalJumpPointNeT [] n = []
generateFinalJumpPointNeT ps n = (northeastTop (head ps) n)

generateFinalJumpPointE :: [Point] -> Int -> [Point]
generateFinalJumpPointE [] n = []
generateFinalJumpPointE ps n = (east (head ps) n)

generateFinalJumpPointSeT :: [Point] -> Int -> [Point]
generateFinalJumpPointSeT [] n = []
generateFinalJumpPointSeT ps n
  | (snd(head ps))==(n-1) = (southeastBottom (head ps) n)
  | otherwise = (southeastTop (head ps) n)

generateFinalJumpPointSwT :: [Point] -> Int -> [Point]
generateFinalJumpPointSwT [] n = []
generateFinalJumpPointSwT ps n 
  | (snd(head ps))==(n-1) = (southwestBottom (head ps) n)
  | otherwise = (southwestTop (head ps) n)
  
generateFinalJumpPointW :: [Point] -> Int -> [Point]
generateFinalJumpPointW [] n = []
generateFinalJumpPointW ps n = (west (head ps) n)

--- For the starting points BELOW the Middle Row 
  
generateFinalJumpPointNwB :: [Point] -> Int -> [Point]
generateFinalJumpPointNwB [] n = []
generateFinalJumpPointNwB ps n 
  | (snd(head ps))==(n-1) = (northwestTop (head ps) n)
  | otherwise = (northwestBottom (head ps) n)

generateFinalJumpPointNeB :: [Point] -> Int -> [Point]
generateFinalJumpPointNeB [] n = []
generateFinalJumpPointNeB ps n 
  | (snd(head ps))==(n-1) = (northeastTop (head ps) n)
  | otherwise = (northeastBottom (head ps) n)

generateFinalJumpPointSeB :: [Point] -> Int -> [Point]
generateFinalJumpPointSeB [] n = []
generateFinalJumpPointSeB ps n = (southeastBottom (head ps) n)

generateFinalJumpPointSwB :: [Point] -> Int -> [Point]
generateFinalJumpPointSwB [] n = []
generateFinalJumpPointSwB ps n = (southwestBottom (head ps) n)  
  
 
-- There are two systems of "movement"
-- Above the Middle row and Below the Middle row
-- note: intersect returns a list

-- Above Middle, NorthWest	c	
northwestTop :: Point -> Int -> [Point]
northwestTop p n = intersect g [((fst(p)-1),(snd(p)-1))]
	where	
		g = generateGrid n (n-1) (2*n-2) [] 
		
-- Below Middle, NorthWest	c		 
northwestBottom :: Point -> Int -> [Point]
northwestBottom p n = intersect g [(fst(p),(snd(p)-1))]
	where	
		g = generateGrid n (n-1) (2*n-2) [] 
		
-- Above Middle, NorthEast	c	
northeastTop :: Point -> Int -> [Point]
northeastTop p n = intersect g [(fst(p),(snd(p)-1))]
	where	
		g = generateGrid n (n-1) (2*n-2) [] 
		
-- Below Middle, NorthEast	c		
northeastBottom :: Point -> Int -> [Point]
northeastBottom p n = intersect g [((fst(p)+1),(snd(p)-1))]
	where	
		g = generateGrid n (n-1) (2*n-2) [] 		

-- East	                    c		
east :: Point -> Int -> [Point]
east p n = intersect g [((fst(p)+1),snd(p))]
	where	
		g = generateGrid n (n-1) (2*n-2) [] 
		
-- Above Middle, SouthEast	c	
southeastTop :: Point -> Int -> [Point]
southeastTop p n = intersect g [((fst(p)+1),(snd(p)+1))]
	where	
		g = generateGrid n (n-1) (2*n-2) [] 
		
-- Below Middle, SouthEast	c		
southeastBottom :: Point -> Int -> [Point]
southeastBottom p n = intersect g [((fst(p)),(snd(p)+1))]
	where	
		g = generateGrid n (n-1) (2*n-2) [] 
		
-- Above Middle, SouthWest	c	
southwestTop :: Point -> Int -> [Point]
southwestTop p n = intersect g [(fst(p),(snd(p)+1))]
	where	
		g = generateGrid n (n-1) (2*n-2) [] 
		
-- Below Middle, SouthWest	c		
southwestBottom :: Point -> Int -> [Point]
southwestBottom p n = intersect g [((fst(p)-1),(snd(p)+1))]
	where	
		g = generateGrid n (n-1) (2*n-2) [] 
		
-- West                     c
west :: Point -> Int -> [Point]
west p n = intersect g [((fst(p)-1),(snd(p)))]
	where	
		g = generateGrid n (n-1) (2*n-2) []   



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
						(3)  -- just check that we only have one depth for nwo...
						(3)  -- int representing the size of the board...						



-- data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)
-- type BoardTree = Tree Board 
-- type Board = [Piece]
-- type State = [Tile]
-- type Tile  = (Piece, Point)   
-- type Tile  = (Piece, Point)
-- type Grid  = [Points]
-- type Move = (point, point)
-- data Piece = D | W | B  the type of the piece 
-- this function is going to take the current board and create all possible boards next to a current depth.

-- t Node 0 [D,D,D,d,d,d,d] [(Node 1 [d,d,d,d,d] [])  
--gameOver :: Piece -> Board -> [Board] -> Int -> Bool 

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n
			| depth == 0 = Node depth board ([])  -- this never gets run...
			| otherwise =  --0  this would be zero																-- how do we know to stop?   we need another depth value in there?
				-- Node (depth-depth) (generateTree_helper (board) (history) (grid) (slides) (jumps) (player) (depth-depth+1) (n))  
				Node (depth-depth) (board) (generateTree_helper (board) ([board]++history) (grid) (slides) (jumps) (player) (depth-depth+1) depth (n)) 


--gameOver :: Board -> [Board] -> Int -> Bool

generateTree_helper :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Int -> [BoardTree]
generateTree_helper board history grid slides jumps player curdepth depth n  -- current depth should be used for everything in this function... we only use depth when comparing
				 | curdepth == depth + 1 =   [] 
				 | otherwise = 					-- gameOver piece grid jump slides board history n
								map 			(\ boa -> if (gameOver player grid jumps slides boa history n) 
															then Node (curdepth) (boa) ([]) -- if its gameOver then we dont continue recursing
															else if (player == W)
																	then 
																	Node (curdepth) (boa) 
																			(generateTree_helper boa ([boa]++history) grid slides jumps B (curdepth+1) depth n) -- if its not gameover then we continue redcursing
																	else
																	Node (curdepth) (boa) 
																			(generateTree_helper boa ([boa]++history) grid slides jumps W (curdepth+1) depth n)) -- if its not gameover then we continue redcursing	


										(exclusive	(movestoBoard (player) 
														  (boardtoState board grid [])
															
															(moveGenerator  (boardtoState board grid[])
																			(slides)
																			(jumps)
																			(player)) 
															(history)  -- this is going to be history...
															([])
															)
														(history))	 -- this place right here is where newStates would be... 

exclusive :: [Board] -> [Board] -> [Board]
exclusive a b = exclusive_helper a b []

-- intersect's helper
exclusive_helper :: [Board] -> [Board] -> [Board] -> [Board]
exclusive_helper a b c
  | null a = c
  | null b = []
  | elem (head a) b = exclusive_helper (tail a) b c
  | otherwise = (exclusive_helper (tail a) b (c++[(head a)]))

											
testing0 = boardtoState board1 grid0 []	
testing1 = movestoBoard (W) (testing0)(moveGenerator(testing0)(slides0)(jumps)(W)) ([]) 

-- this is pretty much genertate new states...
-- we should add in the history here...
-- returns a list of baords from a particular list of moves.
movestoBoard :: Piece ->State -> [Move] -> [Board] -> [Board] -> [Board]
movestoBoard player state moves history acc = (map (\ move -> (movestoBoard_helper (player) (state) (move) ([]))) 
																			(moves))


-- this is just going to return a single board
movestoBoard_helper :: Piece ->State -> Move -> Board -> Board
movestoBoard_helper player ((piece,point):ax) (p1,p2) acc 
			| ax == [] =  -- youd have to check this one more time here... 
						if (point == p1) then ([D] ++ acc)  -- change all of these and it should change the way they are added to the list
										else if (point == p2)
												then ([player]++acc)
												else ([piece]++acc)  -- we are outputting a a board...
			| point == p1 =  movestoBoard_helper (player) (ax) (p1,p2) ([D] ++ acc) 
			| point == p2 = movestoBoard_helper (player) (ax) (p1,p2) ( [player] ++ acc)
			| otherwise =   movestoBoard_helper (player) (ax) (p1,p2) ([piece] ++ acc)

-- function takes a grid and a board and makes a state for moveGenerator.
boardtoState :: Board -> Grid -> State -> State 
boardtoState (a:ax) (b:bx) acc  -- board and grid
				| ax == [] =  ([(a,b)]++acc) 
				| otherwise =  boardtoState (ax) (bx) ([(a,b)] ++ acc)		





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

-- this is correct
move = moveGenerator ([(W, (0,0)),(W, (1,0)),(W, (2,0)),  
					(D, (0,1)),(W, (1,1)),(W, (2,1)),(D,(3,1)), 
				(D, (0,2)),(D, (1,2)),(D, (2,2)),(D,(3,2)),(D,(4,2)), 
					(D, (0,3)),(B, (1,3)),(B, (2,3)),(D,(3,3)), 
						(B, (0,4)),(B, (1,4)),(B, (2,4))])
					(slides0)	
					(jumps)
					(W)

-- this is correct
move1 = moveGenerator(

	[					(D, (0,0)),(W, (1,0)),(W, (2,0)),  
					(D, (0,1)),(D, (1,1)),(D, (2,1)),(D,(3,1)), 
				(D, (0,2)),(W, (1,2)),(D, (2,2)),(D,(3,2)),(D,(4,2)), 
					(B, (0,3)),(D, (1,3)),(D, (2,3)),(D,(3,3)), 
						(D, (0,4)),(B, (1,4)),(B, (2,4))]

	) (slides0) (jumps) (B) 

-- this is correct!!
move2 = moveGenerator(

	[					(D, (0,0)),(W, (1,0)),(W, (2,0)),  
					(D, (0,1)),(D, (1,1)),(D, (2,1)),(D,(3,1)), 
				(D, (0,2)),(W, (1,2)),(D, (2,2)),(D,(3,2)),(D,(4,2)), 
					(D, (0,3)),(B, (1,3)),(D, (2,3)),(D,(3,3)), 
						(D, (0,4)),(B, (1,4)),(B, (2,4))]

	) (slides0) (jumps) (B) 

move3 = moveGenerator(

	[					(W, (0,0)),(W, (1,0)),(W, (2,0)),  
					(D, (0,1)),(D, (1,1)),(W, (2,1)),(D,(3,1)), 
				(D, (0,2)),(D, (1,2)),(W, (2,2)),(D,(3,2)),(D,(4,2)), 
					(D, (0,3)),(B, (1,3)),(B, (2,3)),(D,(3,3)), 
						(B, (0,4)),(B, (1,4)),(B, (2,4))]

	) (slides0) (jumps) (B) 

move4 = moveGenerator(

	[					(D, (0,0)),(W, (1,0)),(W, (2,0)),  
					(D, (0,1)),(W, (1,1)),(W, (2,1)),(D,(3,1)), 
				(D, (0,2)),(D, (1,2)),(W, (2,2)),(D,(3,2)),(D,(4,2)), 
					(D, (0,3)),(B, (1,3)),(B, (2,3)),(D,(3,3)), 
						(B, (0,4)),(B, (1,4)),(B, (2,4))]

	) (slides0) (jumps) (B) 

-- type Tile  = (Piece, Point)  
-- type State = [Tile]		 
-- data Piece = D | W | B deriving (Eq, Show) -- D means empty.
--type Move = (Point,Point)
-- State = [(D, (0, 1)),(W, (0, 1)) ]  -- gives the state of the game...

moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player = moveGenerator_helper state state slides jumps player []

-- we are going to go through states here...
moveGenerator_helper :: State -> State -> [Slide] -> [Jump] -> Piece -> [Move] -> [Move]
moveGenerator_helper state ((piece,(p1,p2)):ax) slides jumps player acc
		| ax == [] =  if piece == player 
						 then (create_moves player state ((p1,p2)) (jumps) (slides) ([])) ++ (acc)
						 else acc
		| otherwise = if piece == player -- if this tile has a piece that is part of players turn then we add to the moves lsit...
						then moveGenerator_helper (state) (ax) (slides) (jumps) (player) ((create_moves player state ((p1,p2)) (jumps) (slides) ([])) ++ (acc))
						else  moveGenerator_helper (state) (ax) (slides) (jumps) (player) (acc) -- here we are not going to add to acc and just recurse


-- returns a list of moves for that certain point...
--  takes Point, lo jumps, lo Slides,
create_moves:: Piece -> State -> Point->[Jump]->[Slide] ->[Move] -> [Move]  
create_moves player state p jmps slides acc =  (map (\(a,b,c)-> (a,c)) 
								(filter (\(a,b,c) -> a == p && (color_checker (state)(b)(c)(player)(D)(D))) (jmps))) ++  -- we need to find the piece behind it is the same color...
										(filter (\(a,b) -> slide_checker (state) (b) (player) )(filter(\(a,b)-> a==p)(slides)) )
										-- we need to check and see if we can move to that spot...		
-- this has to return a boolean..
-- checks whether the current has the opposite value of Piece so that we can add it as jump...
-- algo - first find the point in state that we are looking at...
		-- then check whether that point has the opposite of player... 
-- function takes in the state of the game,
-- 			takes in the point of interest
--			takes in the pieces turn,

--(piece,point)

-- non exhaustive patterns in function...
color_checker:: State -> Point -> Point -> Piece -> Piece-> Piece -> Bool 
color_checker ((pl,po):ax) point_b point_c player player_b player_c -- this is the acc for the colors so we have everything and we can check at the end 
						| ax == [] =   			
						-- null (pl,po) =   -- we do our logic here
										-- just do this for one more case here

										if (po == point_b) -- player_b now pl instead.
											then if (player == pl) -- is player == second color
													then if (player == player_c) 
														then False
														else True
											else False			
											else if (po == point_c) -- if the last point is c
												then if (player == player_b) --  then we say are the first two points the same
													then if (player == pl) -- is the last point the same color
														then False     -- false if it is
														else True
													else False	
												else if (player == player_b)
													then if (player == player_c)
														then False
														else True
													else False

										-- if (player == player_b)  
										-- 	then if(player == player_c) 
										-- 			then False
										-- 			else True
										-- 	else False		
						| po == point_b = color_checker (ax)(point_b)(point_c)(player)(pl)(player_c)  -- set the color of b which is pl
						| po == point_c = color_checker (ax) (point_b)(point_c)(player)(player_b)(pl)  -- if we have found a point c set c
						| otherwise = color_checker (ax) (point_b)(point_c)(player)(player_b)(player_c)



slide_checker :: State -> Point -> Piece -> Bool
slide_checker ((pl,po):ax) point_b player 
				| ax == [] = if po == point_b 
								then if (pl==D)
										then True
										else False 
								else True  

				| po == point_b = if (pl == D)
									then True  -- we can slide there...
									else False  -- there is another piece there...
				| otherwise = slide_checker (ax) point_b player	 				

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
-- -- (REMOVED) history: a list of Boards of representing all boards already seen 
-- -- (REMOVED) n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- (REMOVED) myTurn: a Boolean indicating whether it is the program's turn or the opponents.  
--
-- Returns: the goodness value of the provided board

-- This boardEvaluator will return the number of pieces that are left for the opposing player
-- the fewer pieces your opponent has the more desirable that board will be.
-- a winning score will be when your opponent has n-1 pieces left so you should take this
-- score the evaluator gives you and have it subtracted by n-1, if that value = 0 you won!

-- we need a better boardEvaluator function...
-- do this later...

-- given a board we want to give it a value... 
-- if its white then we plus 

	-- get rid of piece and lets just give the board a goodness level non dependant on whos turn it is...



board1 = 					[D,D,D,
				 	  	    D,B,D,W,
					 	   D,B,D,D,D,  -- this returns -2 
 				 	  		D,D,D,D,
				       		 D,D,B]        
board2 = 					[D,D,D,
				 	  	    D,W,D,W,  -- this should return +2
					 	   D,W,D,D,D,  
 				 	  		D,D,D,D,
				       		 D,D,B]        				       		 


{-boardEvaluator :: Piece -> Board -> Int
boardEvaluator player board 
	| player == W 	= countPiecesW board 0
	| player == B	= countPiecesB board 0
-}	
countPiecesW :: Board -> Int -> Int
countPiecesW board acc 
	| null board			= acc			
	| (head board) == B		= countPiecesW(tail board) (acc + 1)
	| otherwise 			= countPiecesW (tail board) acc
	
countPiecesB :: Board -> Int -> Int
countPiecesB board acc 
	| null board			= acc			
	| (head board) == W		= countPiecesB (tail board) (acc + 1)
	| otherwise 			= countPiecesB (tail board) acc

--boardTest = boardEvaluator B board1


-- work on the minimax function right now... 

--
boardEvaluator2 :: Board -> Int 
boardEvaluator2 board = 
	  (countPiecesB board 0) - (countPiecesW board 0)  
	  -- this would give the value of the board... statically... if its lower its better for black
	  -- if its higher its better for white... 


boardTest2 = boardEvaluator2 board1  -- this should be -2
boardTest3 = boardEvaluator2 board2
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

-- we place the baord evaluator inside for now...
-- mini = minimax (tree1) (boardEvaluator (W) (board1))
mini = minimax (tree1) (boardEvaluator2) (B)  -- we just send the heuristic inside... 



tree1 = generateTree(		[W,W,W,
				 	  	    D,W,D,D,
					 	   D,D,W,D,D,  
 				 	  		D,B,B,D,
				       		 B,B,B]      )  
							([])  -- there is no history this is the first move.
							(grid0)
							(slides0)
							(jumps)
							(W)		
							(1)		-- the depth we need to search.
							(3)   -- the size

-- the tree that we are going to be traversing here...

type BoardVal = (Board, Int)  -- custom type that allows for every board to be assigned a value...

--minimax :: BoardTree -> (Board -> Bool -> Int) -> Board	

-- whos turn is it right now...

-- its currently whites turn...
-- a single board...
minimaxTest = minimax (generateTree 					(sTrToBoard "WWW-W-----B--BB-B-B")  
										(sTrToBoard_list ["WWW-W-----W--BB-BBB", "WWW-WW-------BB-BBB"]) 
										(grid0) 
										(slides0) 
										(jumps) 
										(W)   -- we need minimax to take in another function...
										 (1)
										  (3))
			(boardEvaluator2)
			(W) 


			-- we should test the tree that comes out here
tree3 = generateTree 					(sTrToBoard "WWW-W-----B--BB-B-B")  
										(sTrToBoard_list ["WWW-W-----W--BB-BBB", "WWW-WW-------BB-BBB"]) 
										(grid0) 
										(slides0) 
										(jumps) 
										(W)   -- we need minimax to take in another function...
										 (1)
										  (3)
							


minimax :: BoardTree -> (Board -> Int)-> Piece -> Board
minimax (Node _ b children) heuristic player = 
								-- we need an if statement here -- what if there is not children... 
								-- there must be though...

									if (player == W)   -- if the player is
											then
											max' (map (\child_tree -> (board child_tree,
																	(minimax' (child_tree) (heuristic) (False)))) -- you start off the function with max
														(children))     -- for every child tree
													(([],-10000000000))  -- just start off with an empty board and no value inside...
											else 
											min' (map (\child_tree -> (board child_tree,
																	(minimax' (child_tree) (heuristic) (True)))) -- you start off the function with max
														(children))     -- for every child tree
													(([],1000000000)) -- we need to start this off with a huge value...
												

-- we need to give a heuristic to every level of the board... 
--algo :
--		1.) check the children and see which one has the lowest or highest value based on if it is max or min.

--		1.) we want to cycle through all the trees within children, 
--			gather a heuristic value from each and choose the highest one.
-- this function traverses a list of BoardVal and returns the highest rated board.
-- takes the list of boardvals, board as an accumulator
max':: [BoardVal] -> BoardVal -> Board 
max' ((board, val):ax) (cur_board,cur_val)  -- this is the currently accumulated board val...
		| ax == [] = if (val >= cur_val) 
					 	 then board -- we return the last item board ..
					 	 else cur_board -- we return the accumulated board...
		| otherwise = if (val >= cur_val) 
						  then max' (ax) ((board,val)) -- then we recurse with the new val 
						  else max' (ax) ((cur_board, cur_val)) -- else we recurse with the accumulated one


-- it takes the boardVal and finds the minimum board from a list of boardVals.
min':: [BoardVal] -> BoardVal -> Board 
min' ((board, val):ax) (cur_board,cur_val)  -- this is the currently accumulated board val...
		| ax == [] = if (val <= cur_val) 
					 	 then board -- we return the last item board ..
					 	 else cur_board -- we return the accumulated board...
		| otherwise = if (val <= cur_val) 
						  then min' (ax) ((board,val)) -- then we recurse with the new val 
						  else min' (ax) ((cur_board, cur_val)) -- else we recurse with the accumulated one
						  

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

-- data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)
-- type BoardTree = Tree Board 
-- type Board = [Piece]
-- type State = [Tile]
-- type Tile  = (Piece, Point)   
-- type Tile  = (Piece, Point)
-- type Grid  = [Points]
-- type Move = (point, point)
-- data Piece = D | W | B  the type of the piece 

--minimax' :: BoardTree -> (Board -> Bool -> Int) -> Bool -> Int
-- is the computer white?  lets just go with that for now.
-- bottom up recursion

-- make a function that checks the size of children... so we can terminate...
childrenSizeTest = childrenSize [] 0 



childrenSize :: [BoardTree] -> Int -> Int
childrenSize children acc = foldr (\child acc -> (acc + 1)) (0) (children)  
								-- this goes through children and for every child add 1 to the accumulator
								-- this should return 0...


	--map (\ c -> (acc + 1)) (children)
--			| ax == [] = acc -- what if there was exactly one?
--			| otherwise = childrenSize (ax) (acc + 1)  -- just keep recursing with the accumulator..
maxTest = maximum ([1,2,-5])
maxTest1 = maximum ([-5,-4])
minTest = minimum ([1,2,-5])

minBoolTest = (-1 <= -1)
minBoolTest1 = (-2 > -5)

minimax' :: BoardTree -> (Board -> Int) -> Bool -> Int
minimax' (Node depth b children) heuristic maxPlayer 
			| childrenSize (children) (0) == 0 = heuristic (b) 
									
			| otherwise =
										if (maxPlayer)   -- if its true then its whites turn and we want the max... and we want the next level to be mini
											then maximum (map (\child_tree -> (minimax' (child_tree) (heuristic) (False))) 
																	(children))


														-- if its false its blacks turn and we want the min 
											else minimum (map (\child_tree -> (minimax' (child_tree) (heuristic) (True)))
																	(children))  
															







----

minimaxTest1 = minimax_ (generateTree 					(sTrToBoard "WWW-W-----B--BB-B-B")  
										(sTrToBoard_list ["WWW-W-----W--BB-BBB", "WWW-WW-------BB-BBB"]) 
										(grid0) 
										(slides0) 
										(jumps) 
										(W)   -- we need minimax to take in another function...
										 (1)
										  (3))
			(boardEvaluator2)
			(W) 

minimax_ :: BoardTree -> (Board -> Int)-> Piece -> IO ()
minimax_ (Node _ b children) heuristic player = 
					
									if (player == W)   -- if the player is
											then print 
											(max' (map (\child_tree -> (board child_tree,
																	(minimax' (child_tree) (heuristic) (False)))) -- you start off the function with max
														(children))
														([],0)
															)     
														
													  
											else putStrLn "the one that shouldnt run" 
												

