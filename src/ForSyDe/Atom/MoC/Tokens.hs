module ForSyDe.Atom.MoC.Tokens where

data Tokens a = Tokens { num :: Int, toks :: [a] }

instance Show a => Show (Tokens a) where
  show = show . toks

ntokens :: Int -> [a] -> Tokens a
ntokens n l | length l == n = Tokens n l
            | otherwise = error "ntokens: number of tokens does not correspond to the length of the list"

tokens :: [a] -> Tokens a
tokens l = Tokens (length l) l

unsafeCheck :: Tokens a -> Tokens a
unsafeCheck ts | num ts == length (toks ts) = ts
               | otherwise = error "unsafeCheck: wrong number of tokens"

safeCheck :: Tokens a -> Maybe (Tokens a)
safeCheck ts | num ts == length (toks ts) = Just ts
             | otherwise = Nothing


-- consume :: (Tokens a -> b) -> [a] -> (b, [a])
-- consume f


-- f (Tokens _ a) = tokens a
