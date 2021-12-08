type FirstName = String

type LastName = String

type MiddleName = String

type Age = Int

type Height = Int

type Weight = Int

data Sex
  = Male
  | Female

data RhType
  = Pos
  | Neg

data ABOType
  = A
  | B
  | AB
  | O

data BloodType =
  BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _               = True
canDonateTo _ (BloodType AB _)              = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _                             = False --otherwise

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l)             = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

showSex :: Sex -> String
showSex Male   = "Male"
showSex Female = "Female"

-- Patient v1
-- data Patient =
--   Patient Name Sex Age Height Weight BloodType
-- janeDoe :: Patient
-- janeDoe =
--   Patient (NameWithMiddle "Jane" "Mid" "Doe") Female 18 60 58 (BloodType O Rh)
-- Patient v2
data Patient =
  Patient
    { name      :: Name
    , sex       :: Sex
    , age       :: Age
    , height    :: Height
    , weight    :: Weight
    , bloodType :: BloodType
    }

jackieSmith :: Patient
jackieSmith =
  Patient
    { name = Name "Jackie" "Smith"
    , age = 43
    , sex = Female
    , height = 62
    , weight = 115
    , bloodType = BloodType O Neg
    }

-- Q12.1
-- Write a function similar to canDonateTo that takes two patients as
-- arguments rather than two BloodTypes.
canDonateToPatient :: Patient -> Patient -> Bool
canDonateToPatient patientA patientB =
  canDonateTo (bloodType patientA) (bloodType patientB)

-- Q12.2
-- Implement a patientSummary function that uses your final
-- Patient type. patient-Summary should output a string that
-- looks like this:
-- **************
-- Patient Name: Smith, John
-- Sex: Male
-- Age: 46
-- Height: 72 in.
-- Weight: 210 lbs.
-- Blood Type: AB+
-- **************
johnSmith :: Patient
johnSmith =
  Patient
    { name = Name "John" "Smith"
    , age = 46
    , sex = Male
    , height = 72
    , weight = 210
    , bloodType = BloodType AB Pos
    }

patientSummary :: Patient -> String
patientSummary patient =
  addLineFeed sep ++
  addLineFeed ("Patient Name: " ++ showName (name patient)) ++
  addLineFeed ("Sex: " ++ showSex (sex patient)) ++
  addLineFeed ("Age: " ++ show (age patient)) ++
  addLineFeed ("Height: " ++ show (height patient)) ++
  addLineFeed ("Weight: " ++ show (weight patient)) ++
  addLineFeed ("Blood Type: " ++ showBloodType (bloodType patient)) ++
  addLineFeed sep
  where
    sep = "**************"
    addLineFeed str = str ++ "\n"
