type CourseID = Int
type Capacity = Int
type StudentID = Int
data CourseInfo = Course CourseID Capacity [StudentID]
    deriving (Show)

-- enroll student in the course if
-- possible and return updated course
-- otherwise, Nothing
enroll' :: CourseInfo -> StudentID ->
        Either String CourseInfo
enroll' (Course cid cap rs) sid
  | sid `elem` rs =
      Left "student already registered"
  | length rs >= cap =
      Left "course full"
  | otherwise =
      Right $ Course cid cap (sid:rs)

-- register student in a given course and
-- return updated registration information
-- if possible
register' :: [CourseInfo]
    -> CourseID -> StudentID
    -> Either String [CourseInfo]
register' [] _ _ = Left "no such course"
register' (c@(Course cid _ _) : cs)
        cid' sid
  | cid == cid' =
      case enroll' c sid of
        Left msg -> Left msg
        Right c' -> Right (c' : cs)
  | otherwise = 
      case register' cs cid' sid of
          Left msg -> Left msg
          Right cs' -> Right (c : cs')

reg = [Course 261216 100 [600610717],
  Course 261218 90
      [600610738, 600610747],
  Course 261497 70
      [600610754,
       600610777,
       600610783],
  Course 261406 2
      [600610752, 600610764]]
