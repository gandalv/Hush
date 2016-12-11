module Lang where

data Program = Lit Literal
             | Inst Instruction
             | List [Program]
             deriving (Show, Eq)

data Literal = IntLit Int
             | FloatLit Float
             deriving (Show, Eq)

data Instruction = AddIntInt
                 | AddFloatFloat
                 | SubtractIntInt
                 | SubtractFloatFloat
                 | MultiplyIntInt
                 | MultiplyFloatFloat
                 | DivideIntInt
                 | DivideFloatFloat
                 | DupInt
                 | DupFloat
                 deriving (Show, Eq)

data ExecState = ExecState {
      int   :: [Int]
    , float :: [Float]
    , code  :: [Program]
    , exec  :: [Program]
} deriving (Show)

empty :: ExecState
empty = ExecState [] [] [] []

initState :: Program -> ExecState
initState ps = ExecState {
    int   = [],
    float = [],
    code  = [],
    exec  = [ps]
}



doStep :: ExecState -> Maybe ExecState
doStep s@ExecState{exec = []} = Nothing
doStep s@ExecState{exec = (e:es)} = let s' = s {exec = es} in
    Just (case e of
        Lit  l -> pushLiteral s' l
        Inst i -> processInstruction i s'
        List p -> foldr pushExec s' p)

pushExec :: Program -> ExecState -> ExecState
pushExec p s = s {exec = p : exec s}

pushLiteral :: ExecState -> Literal -> ExecState
pushLiteral s l = case l of
    IntLit   x -> s {int = x : int s}
    FloatLit x -> s {float = x : float s}

processInstruction :: Instruction -> ExecState -> ExecState
processInstruction i = case i of
    AddIntInt          -> addInt
    AddFloatFloat      -> addFloat
    SubtractIntInt     -> subtractInt
    SubtractFloatFloat -> subtractFloat
    MultiplyIntInt     -> multiplyInt
    MultiplyFloatFloat -> multiplyFloat
    DivideIntInt       -> divideInt
    DivideFloatFloat   -> divideFloat
    DupInt             -> dupInt
    DupFloat           -> dupFloat

addInt :: ExecState -> ExecState
addInt s = case s of
    ExecState{int = []}       -> s
    ExecState{int = [_]}      -> s
    ExecState{int = (x:y:ys)} -> s {int = (y + x) : ys}

addFloat :: ExecState -> ExecState
addFloat s = case s of
    ExecState{float = []}       -> s
    ExecState{float = [_]}      -> s
    ExecState{float = (x:y:ys)} -> s {float = (y + x) : ys}

subtractInt :: ExecState -> ExecState
subtractInt s = case s of
    ExecState{int = []}       -> s
    ExecState{int = [_]}      -> s
    ExecState{int = (x:y:ys)} -> s {int = (y - x) : ys}

subtractFloat :: ExecState -> ExecState
subtractFloat s = case s of
    ExecState{float = []}       -> s
    ExecState{float = [_]}      -> s
    ExecState{float = (x:y:ys)} -> s {float = (y - x) : ys}

multiplyInt :: ExecState -> ExecState
multiplyInt s = case s of
    ExecState{int = []}       -> s
    ExecState{int = [_]}      -> s
    ExecState{int = (x:y:ys)} -> s {int = (y * x) : ys}

multiplyFloat :: ExecState -> ExecState
multiplyFloat s = case s of
    ExecState{float = []}       -> s
    ExecState{float = [_]}      -> s
    ExecState{float = (x:y:ys)} -> s {float = (y * x) : ys}

divideInt :: ExecState -> ExecState
divideInt s = case s of
    ExecState{int = []}       -> s
    ExecState{int = [_]}      -> s
    ExecState{int = (x:y:ys)} -> s {int = (y `div` x) : ys}

divideFloat :: ExecState -> ExecState
divideFloat s = case s of
    ExecState{float = []}       -> s
    ExecState{float = [_]}      -> s
    ExecState{float = (x:y:ys)} -> s {float = (y / x) : ys}

dupInt :: ExecState -> ExecState
dupInt s = case s of
    ExecState{int = []}     -> s
    ExecState{int = (x:xs)} -> s {int = x:x:xs}

dupFloat :: ExecState -> ExecState
dupFloat s = case s of
    ExecState{float = []}     -> s
    ExecState{float = (x:xs)} -> s {float = x:x:xs}