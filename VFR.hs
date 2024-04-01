module VFR where

import Data.List

type Agent  = String

type Prop   = String

type PropBaseClean = [Prop]

type InComp = (Prop, Prop)

type InCompProp  = [InComp]

type Value = String


data Weight =  Scale Int | Indeterminate
    deriving (Eq, Show, Ord) 


type AgentValueToWeight = (Agent, Value, Weight)



type AgentValueProfile = [AgentValueToWeight]

type AgentValuePropWeight = (Agent, Value, Prop, Weight)


type AgentPropAssessment = [AgentValuePropWeight]

{- Example - Escola vs Coca-Cola Bottling Co. -Part 1
   constructing agent's propBaseCleans
-}


-- first a list of propositions
liab :: Prop
liab = "Liab"

manPrep :: Prop
manPrep = "manPrep"

noDam :: Prop
noDam = "noDam"

defContr :: Prop
defContr = "defContr"

compensate :: Prop
compensate = "compensate"

notCompensate :: Prop
notCompensate = "notCompensate"

-- also a list of values

resp :: Value
resp = "resp"

pubGood :: Value
pubGood = "pubGood"

-- Now we construct some AgentvalueWeight/propWeight data for an agent we call Alice

alice :: Agent
alice = "Alice"

-- cycle [a,b] = [a,b,a,b,a,b,....] it is an infinite list of repeating values
-- I use it here because position matters when making comparisons
-- in the propBaseCLean function
aliceAgentValWeights :: AgentValueProfile
aliceAgentValWeights = cycle [(alice, resp, Scale 3), (alice, pubGood, Scale 1)]

aliceAgentValPropWeights :: AgentPropAssessment
aliceAgentValPropWeights = [(alice, resp, manPrep, Scale 1), (alice, pubGood, manPrep, Scale 2), 
                            (alice, resp, defContr, Scale 3), (alice, pubGood, defContr, Scale 2),
                            (alice, resp, compensate, Scale 3), (alice, pubGood, compensate, Scale 2)]

-- these functions give you access to elements of the tuples
getWeightValWeight :: AgentValueToWeight -> Weight
getWeightValWeight (a,b,c) = c

getVal1 :: AgentValueToWeight -> Value
getVal1 (x,y,z) = y

getWeightpropweight :: AgentValuePropWeight -> Weight
getWeightpropweight (a,b,c,d) = d

getVal2 :: AgentValuePropWeight -> Value
getVal2 (a,b,c,d) = b

getProp :: AgentValuePropWeight -> Prop
getProp (a,b,c,d) = c

-- this function calculates an agent's PropBaseClean using a value profile and disposition to propositions

-- it doesn't work correctly: the propbaseclean is not the same as the one in the paper
-- 

propBaseClean :: AgentPropAssessment -> AgentValueProfile -> [Prop] -> PropBaseClean
propBaseClean [] ys zs = []
propBaseClean (x:xs) (y:ys) zs = case (getWeightValWeight y) <= (getWeightpropweight x) of

                                 True ->  if (getProp x) `elem` zs
                                          then removeProp (getProp x) (nub $ propBaseClean xs ys zs)
                                          else nub $ getProp x : propBaseClean xs ys zs 
                                        
                                 False -> removeProp (getProp x) (nub $ propBaseClean xs ys (getProp x : zs))  

removeProp :: Prop -> [Prop] -> [Prop]
removeProp x ys = filter (\y -> x /= y) ys

-- And now we have everything we need to build Alice' PropBaseClean:

alicePropBaseClean :: PropBaseClean
alicePropBaseClean = propBaseClean aliceAgentValPropWeights aliceAgentValWeights []

-- now we do the same for another agent: Bob

bob :: Agent
bob = "bob"

bobValProfile :: AgentValueProfile
bobValProfile = cycle [(bob, resp, Scale 2), (bob, pubGood, Scale 3)]

bobValPropWeight :: AgentPropAssessment
bobValPropWeight = [(bob, resp, manPrep, Scale 2), (bob, pubGood, manPrep, Scale 3),
                    (bob, resp, defContr, Scale 3), (bob, pubGood, defContr, Scale 1),
                    (bob, resp, compensate, Scale 3),(bob, pubGood, compensate, Scale 3)]

-- propBaseClean currently doesn't work for bob's data.
bobPropBaseClean :: PropBaseClean
bobPropBaseClean = propBaseClean bobValPropWeight bobValProfile []