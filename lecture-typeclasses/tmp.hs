divide 0 = Nothing
divide x = Just (2 / x)


anyThingBut5 :: Maybe Int -> Maybe Int
anyThingBut5 Nothing = Nothing
anyThingBut5 (Just 5) = Nothing
anyThingBut5 (Just x) = Just x
