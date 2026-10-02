module SnocList
  ( SnocList,
    toList,
    fromList,
    snoc,
    lastSnoc,
  )
where

-- A list optimized for adding elements at the end.
-- Elements are stored in reverse order.
newtype SnocList a = SnocList {unSnocList :: [a]}
  deriving (Eq, Show)

toList :: SnocList a -> [a]
toList (SnocList xs) =
  reverse xs

fromList :: [a] -> SnocList a
fromList xs =
  SnocList (reverse xs)

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x =
  SnocList (x : xs)

lastSnoc :: SnocList a -> Maybe a
lastSnoc (SnocList []) = Nothing
lastSnoc (SnocList (x : _)) = Just x

instance Semigroup (SnocList a) where
  SnocList xs <> SnocList ys = SnocList (ys ++ xs)

instance Monoid (SnocList a) where
  mempty = SnocList []
