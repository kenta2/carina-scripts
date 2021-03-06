{-# LANGUAGE ScopedTypeVariables #-}
module Main  where{
import System.IO;
import System.Environment;

main :: IO(());
main = (do{
  (hPutStrLn stderr rcs_code);
  (getArgs >>= (\lambda_case_var ->case lambda_case_var of {
  ["nothing"]-> (return ());
  ["cut-points", (x), (y)]-> (putStrLn(unwords((map show)((cut_points (read x) (read y))))));
  ["double-cut-points", (x), (y)]-> (putStrLn(unwords((map show)((double_cut_points (read x) (read y))))));
  ["test", (x), (y)]-> (test (read x) (read y));
  (_)-> undefined
}));
});
powers_of_two :: [](Int);
powers_of_two = ((:) 1 (map ((*) 2) powers_of_two));
rcs_code :: String;
rcs_code = "$Id: cut-width.ll,v 1.6 2016/06/06 17:00:46 kenta Exp $";
div_rounding_up :: Int -> Int -> Int;
div_rounding_up x y = (case (divMod x y) of {
  ((d), (0))-> d;
  ((d), (_))-> (succ d)
});
cut_points :: Int -> Int -> [](Int);
cut_points big small = (case (compare small big) of {
  (GT)-> [];
  (EQ)-> [0];
  (LT)-> (let {
numpieces :: Int;
numpieces = (pred (div_rounding_up big small));
last_one :: Int;
last_one = ((-) big small);
f :: Int -> Int;
f i = (div ((*) i last_one) numpieces)
}
 in ((map f)((enumFromTo 0 numpieces))))
});
double_cut_points :: Int -> Int -> [](Int);
double_cut_points big small = (case (compare small big) of {
  (GT)-> [];
  (EQ)-> [0];
  (LT)-> (let {
last_one :: Int;
last_one = ((-) big small);
n1 :: Int;
n1 = (div last_one small);
n2 :: Int;
n2 = (case n1 of {
  (0)-> 1;
  (_)-> ((*) 2 n1)
});
f :: Int -> Int;
f i = (div ((*) i last_one) n2)
}
 in ((map f)((enumFromTo 0 n2))))
});
one_pnmcut :: Int -> Int -> String;
one_pnmcut height vcut = ("pnmcut 0 " ++ (show vcut) ++ " 0 " ++ (show height) ++ " a ");
one_pipeline :: Rescale -> [](String) -> Int -> Int -> Int -> String;
one_pipeline rescale rotations hcut height vcut = (do{
  rot :: String <- rotations;
  ((one_pnmcut height vcut) ++ (do_rescale rescale) ++ (do_rotation rot) ++ "| cjpeg > x-" ++ (show rescale) ++ "-" ++ (show hcut) ++ "-" ++ (show vcut) ++ rot ++ ".jpg\n");
});
data Rescale  = Rescale(Int);
do_rescale :: Rescale -> String;
do_rescale rscale = (case rscale of {
  (Rescale(1))-> [];
  (Rescale(x))-> ("| pnmscale " ++ (show ((/) (1.0 :: Double) (fromIntegral x))) ++ " ")
});
do_rotation :: String -> String;
do_rotation rot = (case rot of {
  ([])-> [];
  (_)-> ("| pnmflip " ++ rot ++ " ")
});
instance Show (Rescale) where {
show x = (case x of {
  (Rescale(y))-> ((show y))
})
}
;
big_column :: Int -> Rescale -> [](String) -> Int -> Int -> Int -> String;
big_column big_height rescale rotations width height hcut = ("pnmcut " ++ (show hcut) ++ " 0 " ++ (show width) ++ " 0 original >| a\n" ++ (concatMap (one_pipeline rescale rotations hcut height) (cut_points big_height height)));
big_big :: Int -> Int -> Rescale -> [](String) -> Int -> Int -> String;
big_big big_width big_height rescale rotations width height = (concatMap (big_column big_height rescale rotations width height) (cut_points big_width width));
double_size :: (Int, Int) -> (Int, Int);
double_size x = (((*) 2 (fst x)), ((*) 2 (snd x)));
fits :: Int -> Int -> Int -> Int -> Bool;
fits width height x y = ((&&) ((<) x width) ((<) y height));
size_series :: Int -> Int -> Int -> Int -> []((Int, (Int, Int)));
size_series width height monitor_width monitor_height = (zip powers_of_two ((takeWhile (uncurry (fits width height)))((iterate double_size)((monitor_width, monitor_height)))));
run_big_big :: Int -> Int -> [](String) -> (Int, (Int, Int)) -> String;
run_big_big big_width big_height rotations rwh = (case rwh of {
  ((rescale), ((width), (height)))-> (big_big big_width big_height (Rescale rescale) rotations width height)
});
run_rescales :: Int -> Int -> Int -> Int -> String;
run_rescales big_width big_height width height = ((++) (concatMap (run_big_big big_width big_height ["-r90", "-r270"]) (size_series big_width big_height height width)) (concatMap (run_big_big big_width big_height ["", "-r180"]) (size_series big_width big_height width height)));
test :: Int -> Int -> IO(());
test width height = (do{
  (putStrLn "set -x");
  (putStrLn "set -C");
  (putStrLn "set -e");
  (putStr (run_rescales 29566 14321 width height));
})
}

