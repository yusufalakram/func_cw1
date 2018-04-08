

-------------------------

snoc :: a -> [a] -> [a]
snoc x []     = [x]
snoc x (y:ys) = y:snoc x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge    xs (y:ys)
    | x == y    = x : merge    xs    ys
    | otherwise = y : merge (x:xs)   ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = msort (take n xs) `merge` msort (drop n xs)
  where
    n = length xs `div` 2

type Node = Int
type Map  = [(Node,Node)]

bothWays :: Node -> Map -> [Node]
bothWays _ [] = []
bothWays n ((x,y):xs)
    | n == x    = y : bothWays n xs
    | n == y    = x : bothWays n xs
    | otherwise =     bothWays n xs

type Location  = String
type Character = String

type Party = [Character]


------------------------- PART 1: Events

data Game  = Won | Game Node Party [Party]
  deriving (Eq,Show)


start :: Game
start =  Game 6 [] characters

end :: Game
end = Won

type Event = Game -> Game

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt i f (x:xs)
    | i > length xs = error "Index out of bounds"
    | i == 0         = f x : xs
    | otherwise      = x : applyAt (i-1) f xs

updateAt :: Node -> Party -> Party -> Event
updateAt _ _ _ Won = Won
updateAt m xs ys (Game n p ps) = Game n p (applyAt m ((merge ys).(flip minus xs)) ps)

update :: Party -> Party -> Party -> Event
update _ _ _ Won = Won
update xs ys zs (Game n p ps) = Game n (((merge ys).(flip minus xs)) p) (applyAt n ((merge zs).(flip minus xs)) ps)


------------------------- PART 2: Dialogues

data Dialogue = End     String
              | Choice  String  [( String , Dialogue )]
              | Action  String  Event

exitWords = ["X","x","Q","q","Exit","exit","Quit","quit"]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

dialogue :: Game -> Dialogue -> IO Game
dialogue g (End s)           = do
  putStrLn s
  return g
dialogue g (Action s e)      = do
  putStrLn s
  return (e g)
dialogue g (Choice s xs)     = do
  putStrLn s
  let possibleReplies = [ x | (x,y) <- xs]
  putStr (enumerate 1 possibleReplies)
  input <- getLine
  if elem input exitWords
  then do
    return g
  else do
    let chosenDialogue = read input
    let possibleDialogues = [ y | (x,y) <- xs]
    dialogue g (possibleDialogues !! (chosenDialogue-1))

findDialogue :: Party -> Dialogue
findDialogue p = searchDialogues numOfDialogues p
  where numOfDialogues    = length dialogues
        possibleParties   = [ x | (x,y) <- dialogues]
        possibleDialogues = [ y | (x,y) <- dialogues]
        searchDialogues :: Int -> Party -> Dialogue
        searchDialogues n p
            | n == 0                          = End "There is nothing we can do."
            | p == (possibleParties !! (n-1)) = (possibleDialogues !! (n-1))
            | otherwise                       = searchDialogues (n-1) p


------------------------- PART 3: The game loop

loop :: Game -> IO ()
loop Won           = return ()
loop (Game n party ps) = do
  -- Formatting
  putStrLn("-----------------------------")

  -- YOU ARE IN
  putStrLn ("You are in " ++ (locations !! n))

  -- YOU CAN TRAVEL TO
  let accessibleNodes = (bothWays n theMap)
  let accessibleLocations = (map (locations !!) (accessibleNodes))
  putStrLn "You can travel to"
  putStr (enumerate 1 accessibleLocations)

  -- WITH YOU ARE
  putStr "With you are\n"
  putStr (enumerate ((length accessibleLocations)+1) party)

  -- YOU CAN SEE
  putStr "You can see\n"
  let objects = (ps !! (n))
  putStr (enumerate ((length accessibleLocations) + (length party) + 1) objects)

  -- WHAT WILL YOU DO?
  putStrLn "What will you do?"
  putStr "INPUT: "
  str<-getLine
  if elem str exitWords
  then do
    return ()
  else do
    let splitInput = words str
    let is = map read splitInput
    if ((length is) == 1) && ((is!!0) <= (length accessibleLocations))
    then do
      loop (Game (accessibleNodes!!((is!!0)-1)) party ps)
    else do
      let js = msort [ x | (x,y) <- (zip (party ++ objects) [((length accessibleLocations)+1)..]) , elem y is]
      newGame <- dialogue (Game n party ps) (findDialogue js)
      loop newGame

game :: IO ()
game = loop start

------------------------- PART 4: Solving the game

talk' :: Dialogue -> [(Event,[Int])]
talk' (End _)       = []
talk' (Action _ e)  = [(e,[])]
talk' (Choice _ xs) = search ([ ([y],d) | (y,(_,d)) <- zip [1..] xs])
  where search :: [([Int], Dialogue)] -> [(Event, [Int])]
        search []                   = []
        search ((x,Choice _ ys):xs) = (search [ ((snoc y x),d) | (y,(_,d)) <- zip [1..] ys]) ++ (search xs)
        search ((x,Action _ e):xs)  = [(e,x)] ++ (search xs)
        search ((x,End _ ):xs)      = [] ++ (search xs)

talk :: Dialogue -> [(Event,String)]
talk d = formatTalk (talk' d)
  where formatTalk :: [(Event,[Int])] -> [(Event,String)]
        formatTalk xs = [(event, "In the dialogue, choose: " ++ (unwords [show i ++ " " | i<-directions])) | (event,directions) <- xs]


event :: String -> Event
event s _ = Game 0 ["Event: " ++ s] []


testDialogue :: Dialogue
testDialogue = Choice "Morpheus opens his palms"
 [("Take the blue pill", Action "" (event "You wake up in bed"))
 ,("Take the red pill",  Action "" (event "You are a battery"))]

testTalk' :: [(Game,[Int])]
testTalk' = [ (e Won,xs) | (e,xs) <- talk' testDialogue]

testTalk :: [(Game,String)]
testTalk = [ (e Won,str) | (e,str) <- talk testDialogue]

-------------------------
extend :: Map -> (Node,[Int]) -> [(Node,[Int])]
extend m (n,path) = [ (i, x:path) | (x, i) <- (zip [1..] (bothWays n m)) ]

travel' :: Map -> [(Node,[Int])] -> [(Node,[Int])] -> [(Node,[Int])]
travel' m xs []         = xs
travel' m xs (y:ys)
    | (visited y xs) = travel' m xs ys
    | otherwise      = travel' m (snoc y xs) ((extend m y) ++ ys)
    where visited :: (Node,[Int]) -> [(Node,[Int])] -> Bool
          visited _ []     = False
          visited x (y:ys)
              | fst x == fst y = True
              | otherwise      = visited x ys

travel :: Map -> Game -> [(Game,String)]
travel m (Game location p ps) = createGames (travel' m [(location,[])] (extend m (location,[]))) p ps
  where createGames :: [(Node,[Int])] -> Party -> [Party] -> [(Game,String)]
        createGames [] _ _          = []
        createGames ((x,y):xs) p ps = ((Game x p ps),(createDirections x y)) : (createGames xs p ps)

        createDirections :: Node -> [Int] -> String
        createDirections x xs = "Travel to " ++ (locations!!x) ++ ": " ++ (unwords [show i ++ " " | i<-(reverse xs)])

-------------------------
member :: Eq a => [a] -> a -> Bool
member    []  _ = False
member (x:xs) y = (x==y) || (member xs y)

members :: Eq a => [a] -> [a] -> Bool
members xs    []  = True
members xs (y:ys) = member xs y && members xs ys

act :: Game -> [(Game,String)]
act Won           = []
act (Game n p ps) = concat [ (createInstructions dialogue party (Game n p ps)) | (party,dialogue)<-dialogues, members (merge p (ps!!n)) party]
  where createInstructions :: Dialogue -> Party -> Game -> [(Game,String)]
        createInstructions d p g = [ ((event g),("\n" ++ "Talk to " ++ (p!!0) ++ "\n" ++ instructions)) | (event,instructions) <- (talk d), (suitable g event)]

suitable :: Game -> Event -> Bool
suitable game event = checkSuitable (event game) game
  where checkSuitable :: Game -> Game -> Bool
        checkSuitable Won _ = True
        checkSuitable (Game n x xs) (Game m y ys)
            | (length x) > (length y)                     = True
            | (length (concat xs)) > (length (concat ys)) = True
            | otherwise                                   = False

-- SOLVE FUNCTION DOESN'T WORK

solve :: IO ()
solve = do
  putStrLn (solveLoop (start,""))
  where
    solveLoop :: (Game,String) -> String
    solveLoop (Won,s) = s
    solveLoop (g,s)
        | ((length (act g)) >= 1)    = solveLoop ((fst ((act g)!!0)),(s++(snd ((act g)!!0))))
        | otherwise                  = solveLoop ((fst ([(game,directions) | (game,directions)<-(travel theMap g), (length (act game) >= 1)]!!0)),(s++(snd ([(game,directions) | (game,directions)<-(travel theMap g), (length (act game) >= 1)]!!0))))


------------------------- Game data

characters :: [Party]
characters =
  [ ["Duke"]
  , ["Portal Gun"]
  , ["Priest"]
  , ["Lee"]
  , ["Chell","Cortana","Mario","Master Chief"]
  , ["Team Rocket"]
  , ["Peach","Rochelle"]
  ]

locations :: [Location]
locations =
  [ "You are not supposed to be here" -- 0
  , "Aperture Science" -- 1
  , "Church of Halo"   -- 2
  , "Macon"            -- 3
  , "Nintendo Land"    -- 4
  , "Pallet Town"      -- 5
  , "Princess Castle"  -- 6
  ]

theMap :: Map
theMap = [(1,5), (2,4), (2,6), (3,5), (4,5), (4,6)]


dialogues :: [(Party,Dialogue)]
dialogues =
 [ (["Mario"] , Choice "I need to save the Princess."
     [("Sure." ,          Action "Let's go." (update ["Mario"] ["Mario"] []))
     ,("Not right now." , Action "Ok."       (update ["Mario"] [] ["Mario"]))
     ])
 , (["Mario","Peach"] , Choice "Save me, Mario!"
    [("Sure." , Action "Thank you for bringing me my hero. Now I can conveniently leave this hat behind." (update ["Mario","Peach"] [] ["Baseball Cap"]))
    ,("Not right now." , End "Mario, pls.")])
 , (["Peach"] , End "That's *Princess* Peach to you, please. And where's my Mario?")
 , (["Master Chief"] , Choice "I want to marry Cortana. Can you escort us to the Church of Halo?"
     [("Sure." ,          Action "Let's go." (update ["Master Chief"] ["Master Chief"] []))
     ,("Not right now." , Action "Ok."       (update ["Master Chief"] [] ["Master Chief"]))
     ])
 , (["Cortana"] , Choice "I must go with Master Chief."
     [("Sure." ,          Action "Let's go." (update ["Cortana"] ["Cortana"] []))
     ,("Not right now." , Action "Ok."       (update ["Cortana"] [] ["Cortana"]))
     ])
 , (["Master Chief","Priest"] , End "I can't marry you without your bride-to-be.")
 , (["Cortana","Priest"] , End "I can't marry you without your husband-to-be.")
 , (["Priest"] , Choice "Welcome, my child. Have you accepted Master Chief as your savior?"
     [("Hail Master Chief (Blessed Be His Name)" , End "")])
 , (["Cortana","Master Chief","Priest"] , Choice "Do you, Master Chief, accept Cortana to be your beloved bride?"
      [("I don't", End "The Wedding is cancelled"),
       ("I do", Choice "And do you, Cortana, take Master Chief to be your beloved Husband?"
        [("I don't", End "The Wedding is cancelled"),
         ("I do", Action "What a beautiful wedding said the bridesmaid to the waiter. But what a shame, that there's some child lurking nearby." (update ["Cortana","Master Chief","Priest"] [] ["Clementine (hiding)"]) )
   ])])
 , (["Baseball Cap"] , Choice "It's a bit grubby, shall I take it?"
     [("Sure." ,          Action "Let's go." (update ["Baseball Cap"] ["Baseball Cap"] []))
     ,("Not right now." , Action "Ok."       (update ["Baseball Cap"] [] ["Baseball Cap"]))
     ])
 , (["Clementine (hiding)"] , End "I'm scared. Where are my parents?")
 , (["Baseball Cap", "Clementine (hiding)"] , Choice "Give the girl the hat?"
    [("Sure." , Action "I feel safe." (update ["Baseball Cap","Clementine (hiding)"] ["Clementine"] []))
    ,("Not right now." , End "")
    ])
 , (["Duke"] , End "Time to k*** a** and chew bubble gum. And I'm all outta gum.")
 , (["Clementine"] , Choice "Will you help me find my parents?"
      [("What do they look like?", Choice "My father's name is Lee"
        [("I asked what do they look like!", End "Sorry")
        ,("I know him, Let's go!", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
      ])
      ,("Do you know your address?", Choice "I can't remember, I think it rhymes with Bacon"
        [("How do you not know your own address?", End "Sorry!"),
         ("Are you thinking of Macon?", Choice "Yes! That's it! Do you know where it is?"
          [("Sure, I can take you there", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
          ,("I don't know how to get there", End "Okay then")
          ])
        ,("Are you hungry?", Choice "Yes! Do you have any chocolate?"
          [("I don't", End "Okay")
          ,("I can go find some", Action "Thanks, I'll stay here in case my parents come back" (update ["Clementine"] [] ["Clementine"]))
   ])])])
 , (["Clementine","Lee"] , Choice "GIVE ME BACK CLEMENTINE!"
     [("Sure...", Action "" (update ["Clementine","Lee"] ["Zombie Lee"] []))
     ])
 , (["Lee"] , End "Clem? Clem, where are you?!")
 , (["Zombie Lee"] , Choice "Uuurrurrhgghghhghgg."
     [("This way." ,  Action "Urg" (update ["Zombie Lee"] ["Zombie Lee"] []))
     ,("Not today." , Action "Hhuuuurgh" (update ["Zombie Lee"] [] ["Zombie Lee"]))
     ])
 , (["Rochelle"] , End "Girl, you should pray there aren't no Zombies around.")
 , (["Rochelle", "Zombie Lee"] , Action "What?! A zombie? You've left me for dead!" (update ["Rochelle","Zombie Lee"] [] ["Pikachu"]))
 , (["Chell"] , Choice "I've just got a volunteering position at Aperture Science. Can you help me find it? I'm not good with directions."
     [("This way." ,  Action "" (update ["Chell"] ["Chell"] []))
     ,("Not today." , Action "" (update ["Chell"] [] ["Chell"]))
    ])
 , (["Chell","Portal Gun"] , Action "This is your fault. It didn't have to be like this. I'm not kidding, now! Turn back, or I will kill you! I'm going to kill you, and all the cake is gone! You don't even care, do you? This is your last chance! ." (update ["Chell","Portal Gun"] [] [] . updateAt 4 ["Team Rocket"] ["Ash"]))
 , (["Team Rocket"] , End "Oh, prepare for trouble, that's what they should do. And make it double, we're grabbing Pikachu.")
 , (["Pikachu"] , Choice "Pika-Pika"
     [("*throw pokeball*"  , Action "" (update ["Pikachu"] ["Pikachu"] []))
     ,("Nope." ,             Action "" (update ["Pikachu"] [] ["Pikachu"]))
     ])
 , (["Ash", "Pikachu"] , Action "You win." (\_ -> Won))
 , (["Pikachu","Team Rocket"] , End "Hey, look at this! Get a load! Let's grab- ALL GLORY TO THE HYPNOTOAD")
 , (["Portal Gun"] , End "I am an inanimate object. What did you expect?")
 ]
