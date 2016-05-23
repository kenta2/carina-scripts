{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Main where {
import System.Environment(getArgs);
import Control.Exception(assert);
import Debug.Trace(trace);
import Data.Function((&));
import Control.Category((>>>));
import Prelude hiding((.),(>>));
import Data.List(permutations);
import Control.Monad(replicateM);
--import Data.Maybe;
--import qualified Data.Map as Map;
--import Data.Map(Map);
--import qualified Data.Set as Set;
--import Data.Set(Set);

-- to avoid the redundancy warning
trace_placeholder :: ();
trace_placeholder = (trace,assert) & (id >>> error) "trace_placeholder";

main :: IO();
main = getArgs >>= \case{
[prefix] -> do {
putStrLn "set -x";
putStrLn "set -e";
commands prefix & mapM_ putStrLn;
};
_ -> error "need prefix"
};

perms :: [[String]];
perms = permutations ["red","grn","blu"];

inverts :: [[Bool]];
inverts = replicateM 3 [False,True];

commands :: String -> [String];
commands prefix = do {
colors :: [String] <- perms;
inv :: [Bool] <- inverts;
return $ mk_command prefix (zip colors inv & map infile) (mk_suffix colors inv);
};

left :: Bool -> String;
left False = "noname";
left True = "invert";

infile :: (String,Bool) -> String;
infile (s,b) = left b ++ "." ++ s;

-- not upper lower for file systems which are case insensitive
number :: Bool -> String;
number False = "0";
number True = "1";

outfile :: (String,Bool) -> String;
outfile (s,b) = [head s] ++ number b;

mk_command :: String -> [String] -> String -> String;
-- Currently do not have enough disk space for lossless.
-- Curiously, jp2 is sometimes worse than png.
mk_command prefix ins suffix = let {
 out :: String;
 out = prefix ++ "-"++suffix++"-z.ppm";
} in ": " ++ out ++ "\n"
++ "rgb3toppm " ++ unwords ins ++ " > " ++ out;

--(zip colors inv & map outfile & concat);
mk_suffix :: [String] -> [Bool] -> String;
mk_suffix s b = map head s ++ "-" ++ concatMap number b;
} --end
