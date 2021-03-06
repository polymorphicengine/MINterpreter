module ParseMINISpec where

import ParseMINI
import Test.Hspec
import Text.Parsec(parse)
import Text.Parsec.Error

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

{-| --------------------------------
          Tests: MINI Core
-}  --------------------------------

  describe "whitespace" $ do
    it "always suceeds" $ do
        (parse whitespace "" "\t  foo bar\n") `shouldBe` (Right ())
        (parse whitespace "" "foo bar\n") `shouldBe` (Right ())

  describe "number parser" $ do
    it "parses numbers, fails otherwise" $ do
        (parse numE "" "123") `shouldBe` (Right (ENum 123))
        show (parse numE "" "abc") `shouldBe` "Left (line 1, column 1):\nunexpected \"a\"\nexpecting digit"
        show (parse numE "" "x_y_a_251_ \t") `shouldBe` "Left (line 1, column 1):\nunexpected \"x\"\nexpecting digit"
        (parse numE "" "0123")  `shouldBe` (Right (ENum 0123))

  describe "varE" $ do
    it "parses variables, fails otherwise" $ do
        (parse varE "" "uzz__oi2_3") `shouldBe` (Right (EVar (Var "uzz__oi2_3")))
        (parse varE "" "uA") `shouldBe` (Right (EVar (Var "u")))
        (parse varE "" "fffffffffffffffffffffffffffffffffffffff") `shouldBe` (Right (EVar (Var "fffffffffffffffffffffffffffffffffffffff")))
        show (parse varE "" "3uzz__oi2_3") `shouldBe` "Left (line 1, column 1):\nunexpected \"3\""

  describe "expPos" $ do
    it "parses positive expressions, fails otherwise" $ do
        (parse expPos "" "3141") `shouldBe` (Right (Pos (ENum 3141)))
        (parse expPos "" "(13 + 5)") `shouldBe` (Right (Pos (Exp (Term (ENum 13) Plus (ENum 5)))))
        (parse expPos "" "(13 + 5) - 5") `shouldBe` (Right (Pos (Exp (Term (ENum 13) Plus (ENum 5)))))
        show (parse expPos "" "-x42") `shouldBe` ("Left (line 1, column 1):\nunexpected \"-\"\nexpecting \"(\" or digit")
        (parse expPos "" "(13 + (x42))") `shouldBe` (Right (Pos (Exp (Term (ENum 13) Plus (Exp (Pos (EVar (Var "x42"))))))))

  describe "expNeg" $ do
    it "parses negative expressions, fails otherwise" $ do
        (parse expNeg "" "-3141") `shouldBe` (Right (Neg (ENum 3141)))
        (parse expNeg "" "-(13 + 5)") `shouldBe` (Right (Neg (Exp (Term (ENum 13) Plus (ENum 5)))))
        (parse expNeg "" "-(13 + 5) - 5") `shouldBe` (Right (Neg (Exp (Term (ENum 13) Plus (ENum 5)))))
        show (parse expNeg "" "x42") `shouldBe` ("Left (line 1, column 1):\nunexpected \"x\"\nexpecting \"-\"")
        (parse expNeg "" "-(13 / (x42))") `shouldBe` (Right (Neg (Exp (Term (ENum 13) Divide (Exp (Pos (EVar (Var "x42"))))))))

  describe "expParse" $ do
    it "parses proper expressions, fails otherwise" $ do
        (parse expParse "" "(2)+(3)") `shouldBe` (Right (Term (Exp (Pos (ENum 2))) Plus (Exp (Pos (ENum 3)))))
        (parse expParse "" "(2)+(-3)") `shouldBe` (Right (Term (Exp (Pos (ENum 2))) Plus (Exp (Neg (ENum 3)))))
        (parse expParse "" "( (2)+(3)   ) / ( (x__3141_x___y) * (-3)) ") `shouldBe` (Right (Term (Exp (Term (Exp (Pos (ENum 2))) Plus (Exp (Pos (ENum 3))))) Divide (Exp (Term (Exp (Pos (EVar (Var "x__3141_x___y")))) Times (Exp (Neg (ENum 3)))))))
        (parse expParse "" "-123987465") `shouldBe` (Right (Neg (ENum 123987465)))
        (parse expParse "" "(-123987465)") `shouldBe` (Right (Pos (Exp (Neg (ENum 123987465)))))
        show (parse expParse "" "(-1239.87465)") `shouldBe` "Left (line 1, column 7):\nunexpected \".\"\nexpecting digit or \")\""
        (parse expParse "" "((3 -1) - (3-1)) - (3-(-1)   ) \t") `shouldBe` (Right (Term (Exp (Term (Exp (Term (ENum 3) Minus (ENum 1))) Minus (Exp (Term (ENum 3) Minus (ENum 1))))) Minus (Exp (Term (ENum 3) Minus (Exp (Neg (ENum 1)))))))
        show (parse expParse "" " ((3 -1) - (3-1)) - (3-(-1)   ) \t") `shouldBe` ("Left (line 1, column 1):\nunexpected \" \"\nexpecting \"(\", digit or \"-\"")
        (parse expParse "" "((((abc+  123) / def ) * (456 - ghi)) + (jklmnop_789__ - qrstuvw_x_y_z)) + 0") `shouldBe`(Right (Term (Exp (Term (Exp (Term (Exp (Term (Exp (Term (EVar (Var "abc")) Plus (ENum 123))) Divide (EVar (Var "def")))) Times (Exp (Term (ENum 456) Minus (EVar (Var "ghi")))))) Plus (Exp (Term (EVar (Var "jklmnop_789__")) Minus (EVar (Var "qrstuvw_x_y_z")))))) Plus (ENum 0)))

    describe "booleanParse" $ do
      it "parses boolean expressions, fails otherwise" $ do
          (parse booleanParse "" "2 < 3") `shouldBe` (Right (BExp (Pos (ENum 2)) LE (Pos (ENum 3))))
          (parse booleanParse "" "a >= 3") `shouldBe` (Right (BExp (Pos (EVar (Var "a"))) GEQ (Pos (ENum 3))))
          (parse booleanParse "" "(a+b) == 3") `shouldBe` (Right (BExp (Pos (Exp (Term (EVar (Var "a")) Plus (EVar (Var "b"))))) EQQ (Pos (ENum 3))))
          (parse booleanParse "" "(a+b)-(c+(-x2_3)) > (3+zip3format_ayy_lmao)") `shouldBe` (Right (BExp (Term (Exp (Term (EVar (Var "a")) Plus (EVar (Var "b")))) Minus (Exp (Term (EVar (Var "c")) Plus (Exp (Neg (EVar (Var "x2_3"))))))) GE (Pos (Exp (Term (ENum 3) Plus (EVar (Var "zip3format_ayy_lmao")))))))
          show (parse booleanParse "" "(1<3)>4")  `shouldBe` ("Left (line 1, column 3):\nunexpected \"<\"\nexpecting digit or \")\"")

    describe "assignParse" $ do
      it "parses variable assignments, fails otherwise" $ do
          (parse assignParse "" "x =   3141;") `shouldBe` (Right (Ass (Var "x") (Pos (ENum 3141))))
          (parse assignParse "" "x_y__z_rofl3141 =   uvw_1413 + 3;") `shouldBe` (Right (Ass (Var "x_y__z_rofl3141") (Term (EVar (Var "uvw_1413")) Plus (ENum 3))))
          (parse assignParse "" "x_y__z_rofl3141 = -a   ;") `shouldBe` (Right (Ass (Var "x_y__z_rofl3141") (Neg (EVar (Var "a")))))
          (parse assignParse "" "x_y__z_rofl3141 \t = \n -(a+3) \t   ;") `shouldBe` (Right (Ass (Var "x_y__z_rofl3141") (Neg (Exp (Term (EVar (Var "a")) Plus (ENum 3))))))
          (show (parse assignParse "" "x = (x<a)  ;")) `shouldBe` ("Left (line 1, column 7):\nunexpected \"<\"\nexpecting \")\"")

    describe "ifParse'" $ do
      it "parses if statements, fails otherwise" $ do
          (parse ifParse' "" "if (a>= b)     {x = (x+a);}") `shouldBe` (Right (If (BExp (Pos (EVar (Var "a"))) GEQ (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps)))
          (parse ifParse' "" "if (a>= b+(-3))     {if (a>= b+(-3))     {x = (x+a);}} ") `shouldBe` (Right (If (BExp (Pos (EVar (Var "a"))) GEQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) GEQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps))) Eps)))
          (show (parse ifParse' "" "if (b+(-3))     {if (a>= b+(-3))     {x = (x+a);}} ")) `shouldBe` ("Left (line 1, column 11):\nunexpected \")\"\nexpecting \">=\", \"<=\", \"==\", \"!=\", \"<\" or \">\"")
          (parse ifParse' "" "if (a>= b)  {while (ffffff_ffff_____23232_ff <  1) \n {kek = 3;}}") `shouldBe` (Right (If (BExp (Pos (EVar (Var "a"))) GEQ (Pos (EVar (Var "b")))) (St (WSt (While (BExp (Pos (EVar (Var "ffffff_ffff_____23232_ff"))) LE (Pos (ENum 1))) (St (ASt (Ass (Var "kek") (Pos (ENum 3)))) Eps))) Eps)))

    describe "elifParse" $ do
      it "parses if-else statements, fails otherwise" $ do
          (show (parse elifParse "" "if (a>= b)     {x = (x+a);}")) `shouldBe` ("Left (line 1, column 28):\nunexpected end of input\nexpecting \"else\"")
          (parse elifParse "" "if (a>= b)     {x = (x+a);} else {if (a>= b+(-3))    \n {x = (x+a);}    \t }") `shouldBe` (Right (Elif (BExp (Pos (EVar (Var "a"))) GEQ (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps) (St (ISt (If (BExp (Pos (EVar (Var "a"))) GEQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps))) Eps)))
          (parse elifParse "" "if (a>= b)     {x = (x+a);} else {if (a>= b)     {x = (x+a);} else {if (a>= b+(-3))    \n {x = (x+a);}    \t }}") `shouldBe` (Right (Elif (BExp (Pos (EVar (Var "a"))) GEQ (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps) (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) GEQ (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps) (St (ISt (If (BExp (Pos (EVar (Var "a"))) GEQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps))) Eps))) Eps)))


    describe "ifParse" $ do
      it "parses if and if-else statements, fails otherwise" $ do
          (parse ifParse "" "if (a>= b+(-3)) \n\n\t     {if (a>= b+(-3))     {x = (x+a);}} ") `shouldBe` (Right (If (BExp (Pos (EVar (Var "a"))) GEQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) GEQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps))) Eps)))
          (parse ifParse "" "if (a!= b+((-3)/\n 5)) \n\n\t   {if (a== b+(-3))     {x = (x+a);}} else {y_haha=3;}") `shouldBe` (Right (Elif (BExp (Pos (EVar (Var "a"))) NEQ (Term (EVar (Var "b")) Plus (Exp (Term (Exp (Neg (ENum 3))) Divide (ENum 5))))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) EQQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps))) Eps) (St (ASt (Ass (Var "y_haha") (Pos (ENum 3)))) Eps)))
          (show (parse ifParse "" "if (x_trulul < 3+-1) \n\n\t   {y_haha=x_trulul;}")) `shouldBe` ("Left (line 1, column 17):\nunexpected \"+\"\nexpecting digit or \")\"")
          (parse ifParse "" "if (x_trulul < 3+1) \n\n\t   {y_haha=x_trulul;}") `shouldBe` (Right (If (BExp (Pos (EVar (Var "x_trulul"))) LE (Term (ENum 3) Plus (ENum 1))) (St (ASt (Ass (Var "y_haha") (Pos (EVar (Var "x_trulul"))))) Eps)))

    describe "whileParse" $ do
      it "parses while statements, fails otherwise" $ do
          (parse whileParse "" "while (kjasdhfkj_17823___asd < 0123) {kek = 3;}") `shouldBe` (Right (While (BExp (Pos (EVar (Var "kjasdhfkj_17823___asd"))) LE (Pos (ENum 123))) (St (ASt (Ass (Var "kek") (Pos (ENum 3)))) Eps))) -- 0123 --> 123
          (parse whileParse "" "while (kjasdhfkj_17823___asd < 0123) {while (kjasdhfkj_17823___asd < 0123) {if (a>= b+(-3)) \n\n\t     {if (a>= b+(-3))     {x = (x+a);}} }}") `shouldBe` (Right (While (BExp (Pos (EVar (Var "kjasdhfkj_17823___asd"))) LE (Pos (ENum 123))) (St (WSt (While (BExp (Pos (EVar (Var "kjasdhfkj_17823___asd"))) LE (Pos (ENum 123))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) GEQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) GEQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ASt (Ass (Var "x") (Pos (Exp (Term (EVar (Var "x")) Plus (EVar (Var "a"))))))) Eps))) Eps))) Eps))) Eps)))
          (show (parse whileParse "" "while (k > 3141) {if (x<> 9) {e   =3;}}")) `shouldBe` ("Left (line 1, column 22):\nunexpected \"(\"\nexpecting \"=\"")
          (parse whileParse "" "while (k > 3141) {if (x \t> 9) {e   =3;}}") `shouldBe` (Right (While (BExp (Pos (EVar (Var "k"))) GE (Pos (ENum 3141))) (St (ISt (If (BExp (Pos (EVar (Var "x"))) GE (Pos (ENum 9))) (St (ASt (Ass (Var "e") (Pos (ENum 3)))) Eps))) Eps)))
          show (parse whileParse "" "while (k > 3141+3) {if (x< (3+1)+1)      ) {e   =3;}}") `shouldBe` ("Left (line 1, column 24):\nunexpected \"(\"\nexpecting \"=\"")

  describe "statementsParse" $ do
    it "parses statements, fails otherwise" $ do
          (parse statementsParse "" " \n \t       ") `shouldBe` (Right Eps)
          (parse statementsParse "" "if (x==3) {x=x+1;}") `shouldBe` (Right (St (ISt (If (BExp (Pos (EVar (Var "x"))) EQQ (Pos (ENum 3))) (St (ASt (Ass (Var "x") (Term (EVar (Var "x")) Plus (ENum 1)))) Eps))) Eps))
          (parse statementsParse "" "x=x+1;") `shouldBe` (Right (St (ASt (Ass (Var "x") (Term (EVar (Var "x")) Plus (ENum 1)))) Eps))
          (parse statementsParse "" "while (k > 3141) {if (x \t> 9) {e   =3;}}") `shouldBe` (Right (St (WSt (While (BExp (Pos (EVar (Var "k"))) GE (Pos (ENum 3141))) (St (ISt (If (BExp (Pos (EVar (Var "x"))) GE (Pos (ENum 9))) (St (ASt (Ass (Var "e") (Pos (ENum 3)))) Eps))) Eps))) Eps))
          (parse statementsParse "" " \n if (x <= 0) {x = 0 +0 \t;}") `shouldBe` (Right Eps)
          (parse statementsParse "" "if (x <= 0) {x = 0 +0 \t;}") `shouldBe` (Right (St (ISt (If (BExp (Pos (EVar (Var "x"))) LEQ (Pos (ENum 0))) (St (ASt (Ass (Var "x") (Term (ENum 0) Plus (ENum 0)))) Eps))) Eps))

  describe "returnParse" $ do
    it "parses return expressions, fails otherwise" $ do
          (parse returnParse "" "return xyz_123__0;") `shouldBe` (Right (Return (Var "xyz_123__0")))
          show (parse returnParse "" "r;") `shouldBe` ("Left (line 1, column 1):\nunexpected \";\"\nexpecting \"return\"")
          show (parse returnParse "" "return 3;") `shouldBe` ("Left (line 1, column 8):\nunexpected \"3\"")

  describe "argVarParse" $ do
    it "parses argument variable, fails otherwise" $ do
          (parse argVarParse "" "xyz__123, abc_9_8_7, x,y,z") `shouldBe` (Right (Args (Var "xyz__123") (Args (Var "abc_9_8_7") (Args (Var "x") (Args (Var "y") (Arg (Var "z")))))))
          (parse argVarParse "" "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y,z") `shouldBe` (Right (Args (Var "a") (Args (Var "b") (Args (Var "c") (Args (Var "d") (Args (Var "e") (Args (Var "f") (Args (Var "g") (Args (Var "h") (Args (Var "i") (Args (Var "j") (Args (Var "k") (Args (Var "l") (Args (Var "m") (Args (Var "n") (Args (Var "o") (Args (Var "p") (Args (Var "r") (Args (Var "s") (Args (Var "t") (Args (Var "u") (Args (Var "v") (Args (Var "w") (Args (Var "x") (Args (Var "y") (Arg (Var "z")))))))))))))))))))))))))))

  describe "procBodyParse" $ do
    it "parses procedure body, fails otherwise" $ do
          show (parse procBodyParse "" "if (x <= 0) {x = 0 +0 \t;}") `shouldBe` ("Left (line 1, column 27):\nunexpected end of input\nexpecting \"while\", \"if\", \"print_int\" or \"return\"")
          (parse procBodyParse "" "return x;") `shouldBe` (Right (Body Eps (Return (Var "x"))))
          (parse procBodyParse "" "if (x <= 0) {x = 0 +0 \t;} return x;") `shouldBe` (Right (Body (St (ISt (If (BExp (Pos (EVar (Var "x"))) LEQ (Pos (ENum 0))) (St (ASt (Ass (Var "x") (Term (ENum 0) Plus (ENum 0)))) Eps))) Eps) (Return (Var "x"))))
          (parse procBodyParse "" "if (c>=(b+(-3))/3)     {if (a>= b+(-3))  {a = 123987465;} else  {while (y!= 3) {x=x+1;} } } return irwas_bru;  ") `shouldBe` (Right (Body (St (ISt (If (BExp (Pos (EVar (Var "c"))) GEQ (Term (Exp (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) Divide (ENum 3))) (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) GEQ (Term (EVar (Var "b")) Plus (Exp (Neg (ENum 3))))) (St (ASt (Ass (Var "a") (Pos (ENum 123987465)))) Eps) (St (WSt (While (BExp (Pos (EVar (Var "y"))) NEQ (Pos (ENum 3))) (St (ASt (Ass (Var "x") (Term (EVar (Var "x")) Plus (ENum 1)))) Eps))) Eps))) Eps))) Eps) (Return (Var "irwas_bru"))))
          (parse procBodyParse "" "funcprog = funkdog3; return lungwog;")  `shouldBe` (Right (Body (St (ASt (Ass (Var "funcprog") (Pos (EVar (Var "funkdog3"))))) Eps) (Return (Var "lungwog"))))

  describe "mainParse" $ do
    it "parses a program that calculates the difference of two numbers" $ do
          (parse mainParse "" "procedure main ( a , b ) {c = a - b; return c ; }") `shouldBe` Right (Main (Args (Var "a") (Arg (Var "b"))) (Body (St (ASt (Ass (Var "c") (Term (EVar (Var "a")) Minus (EVar (Var "b"))))) Eps) (Return (Var "c"))))
    it "parses a program that calculates the minimum of two numbers" $ do
          (parse mainParse "" "procedure main ( a , b ) {if (a <  b) {c =  a  ;} else    {c =b  ;  }   return c ;}") `shouldBe` Right (Main (Args (Var "a") (Arg (Var "b"))) (Body (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) LE (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "c") (Pos (EVar (Var "a"))))) Eps) (St (ASt (Ass (Var "c") (Pos (EVar (Var "b"))))) Eps))) Eps) (Return (Var "c"))))
    it "parses a program that calculates the greatest common divisor for non-negative integers" $ do
          (parse mainParse "" "procedure main (a , b) {r = b ; if  ( a != 0) {while (b != 0) { if ( a <= b) {b = b - a ;} else {a = a - b ;} } r = a ; } return r ;}") `shouldBe` Right (Main (Args (Var "a") (Arg (Var "b"))) (Body (St (ASt (Ass (Var "r") (Pos (EVar (Var "b"))))) (St (ISt (If (BExp (Pos (EVar (Var "a"))) NEQ (Pos (ENum 0))) (St (WSt (While (BExp (Pos (EVar (Var "b"))) NEQ (Pos (ENum 0))) (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) LEQ (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "b") (Term (EVar (Var "b")) Minus (EVar (Var "a"))))) Eps) (St (ASt (Ass (Var "a") (Term (EVar (Var "a")) Minus (EVar (Var "b"))))) Eps))) Eps))) (St (ASt (Ass (Var "r") (Pos (EVar (Var "a"))))) Eps)))) Eps)) (Return (Var "r"))))
    it "parses a program that calculates the factorial for non-negative n" $ do
          (parse mainParse "" "procedure main (n) {i = 1; fac = 1; if (n < 0) {fac = 0;} else {while (n >= i) {fac = fac*i; i = i+1;} } return fac ; }")  `shouldBe` Right (Main (Arg (Var "n")) (Body (St (ASt (Ass (Var "i") (Pos (ENum 1)))) (St (ASt (Ass (Var "fac") (Pos (ENum 1)))) (St (ISt (Elif (BExp (Pos (EVar (Var "n"))) LE (Pos (ENum 0))) (St (ASt (Ass (Var "fac") (Pos (ENum 0)))) Eps) (St (WSt (While (BExp (Pos (EVar (Var "n"))) GEQ (Pos (EVar (Var "i")))) (St (ASt (Ass (Var "fac") (Term (EVar (Var "fac")) Times (EVar (Var "i"))))) (St (ASt (Ass (Var "i") (Term (EVar (Var "i")) Plus (ENum 1)))) Eps)))) Eps))) Eps))) (Return (Var "fac"))))
    it "parses a program that reverses a number n" $ do
          (parse mainParse "" "procedure main (n) {reverse = 0; while (n!=0 ) {\n rem = n - (10 * (n/10)); reverse =((reverse * 10) + rem); n = n/10;} return reverse;}") `shouldBe` Right (Main (Arg (Var "n")) (Body (St (ASt (Ass (Var "reverse") (Pos (ENum 0)))) (St (WSt (While (BExp (Pos (EVar (Var "n"))) NEQ (Pos (ENum 0))) (St (ASt (Ass (Var "rem") (Term (EVar (Var "n")) Minus (Exp (Term (ENum 10) Times (Exp (Term (EVar (Var "n")) Divide (ENum 10)))))))) (St (ASt (Ass (Var "reverse") (Pos (Exp (Term (Exp (Term (EVar (Var "reverse")) Times (ENum 10))) Plus (EVar (Var "rem"))))))) (St (ASt (Ass (Var "n") (Term (EVar (Var "n")) Divide (ENum 10)))) Eps))))) Eps)) (Return (Var "reverse"))))
    it "parses a program that counts the number of digits a number n" $ do
          (parse mainParse "" "procedure main (n) {counter = 0; while (n!= \t 0 ) { n = n/10; counter = counter +1; \n} return counter;}") `shouldBe` Right (Main (Arg (Var "n")) (Body (St (ASt (Ass (Var "counter") (Pos (ENum 0)))) (St (WSt (While (BExp (Pos (EVar (Var "n"))) NEQ (Pos (ENum 0))) (St (ASt (Ass (Var "n") (Term (EVar (Var "n")) Divide (ENum 10)))) (St (ASt (Ass (Var "counter") (Term (EVar (Var "counter")) Plus (ENum 1)))) Eps)))) Eps)) (Return (Var "counter"))))

{-| -----------------------------------
          Tests: MINI Extensions
-}  -----------------------------------

  describe "callE" $ do
    it "parses call expressions, fails otherwise" $ do
          (parse callE "" "xyz___1__2_3(3,1415,9265359,-1)") `shouldBe` Right (ECall (Call (Var "xyz___1__2_3") (ArgsI (Pos (ENum 3)) (ArgsI (Pos (ENum 1415)) (ArgsI (Pos (ENum 9265359)) (ArgI (Neg (ENum 1))))))))
          (parse callE "" "x      (  -1,  1  )        ") `shouldBe` Right (ECall (Call (Var "x") (ArgsI (Neg (ENum 1)) (ArgI (Pos (ENum 1))))))
          (show (parse callE "" "x      =(  -1,  1, )        ")) `shouldBe` ("Left (line 1, column 8):\nunexpected \"=\"\nexpecting \"(\"")
          (parse callE "" "x((2+3), x+1, y, proc(23))") `shouldBe` Right (ECall (Call (Var "x") (ArgsI (Pos (Exp (Term (ENum 2) Plus (ENum 3)))) (ArgsI (Term (EVar (Var "x")) Plus (ENum 1)) (ArgsI (Pos (EVar (Var "y"))) (ArgI (Pos (ECall (Call (Var "proc") (ArgI (Pos (ENum 23))))))))))))

-- just checking whether we added call_expr to int_expr_nested correctly
  describe "expParse" $ do
    it "parses proper expressions, fails otherwise" $ do
          (parse expParse "" "input(1,2,3,4,5)") `shouldBe` Right (Pos (ECall (Call (Var "input") (ArgsI (Pos (ENum 1)) (ArgsI (Pos (ENum 2)) (ArgsI (Pos (ENum 3)) (ArgsI (Pos (ENum 4)) (ArgI (Pos (ENum 5))))))))))
          (parse expParse "" "proc + func(1,23)") `shouldBe` Right (Term (EVar (Var "proc")) Plus (ECall (Call (Var "func") (ArgsI (Pos (ENum 1)) (ArgI (Pos (ENum 23)))))))
          (parse expParse "" "p(r)+ func(1,23)") `shouldBe` Right (Term (ECall (Call (Var "p") (ArgI (Pos (EVar (Var "r")))))) Plus (ECall (Call (Var "func") (ArgsI (Pos (ENum 1)) (ArgI (Pos (ENum 23)))))))

  describe "procParse" $ do
    it "parses a procedure, fails otherwise" $ do
          (parse procParse "" "procedure myproc (x,y__z) {c = x - y__z; return c ; }") `shouldBe` Right (Proc (Var "myproc") (Args (Var "x") (Arg (Var "y__z"))) (Body (St (ASt (Ass (Var "c") (Term (EVar (Var "x")) Minus (EVar (Var "y__z"))))) Eps) (Return (Var "c"))))
          (parse procParse "" "procedure myothproc (x,y,z) {return tu_nix;}") `shouldBe` Right (Proc (Var "myothproc") (Args (Var "x") (Args (Var "y") (Arg (Var "z")))) (Body Eps (Return (Var "tu_nix"))))

  describe "procsParseRec" $ do
    it "parses procedures, fails otherwise" $ do
          (parse procsParseRec "" "procedure proc (identity) {c = proc2 (identity); return c ; } procedure proc2 (c) {return c ;}") `shouldBe` Right (Procs (Proc (Var "proc") (Arg (Var "identity")) (Body (St (ASt (Ass (Var "c") (Pos (ECall (Call (Var "proc2") (ArgI (Pos (EVar (Var "identity"))))))))) Eps) (Return (Var "c")))) (Procs (Proc (Var "proc2") (Arg (Var "c")) (Body Eps (Return (Var "c")))) Nil))
          (parse procsParseRec "" "procedure proc ( a , b ) {c = a - b; return c ; } procedure proc ( a , b ) {if (a <  b) {c =  a  ;} else    {c =b  ;  }   return c ;}") `shouldBe` Right (Procs (Proc (Var "proc") (Args (Var "a") (Arg (Var "b"))) (Body (St (ASt (Ass (Var "c") (Term (EVar (Var "a")) Minus (EVar (Var "b"))))) Eps) (Return (Var "c")))) (Procs (Proc (Var "proc") (Args (Var "a") (Arg (Var "b"))) (Body (St (ISt (Elif (BExp (Pos (EVar (Var "a"))) LE (Pos (EVar (Var "b")))) (St (ASt (Ass (Var "c") (Pos (EVar (Var "a"))))) Eps) (St (ASt (Ass (Var "c") (Pos (EVar (Var "b"))))) Eps))) Eps) (Return (Var "c")))) Nil))

  describe "programParse" $ do
    it "parses a program that calculates the n-th fibonacci number" $ do
          (parse programParseEOF "" "procedure main (n) {res = fib(n); return res; } procedure fib (m) {if (m == 0) { result = 1; } if (m == 1) { result = 1; } if (m > 1) { result = fib(m - 1) + fib(m - 2); } return result;}") `shouldBe` Right (Prog (Main (Arg (Var "n")) (Body (St (ASt (Ass (Var "res") (Pos (ECall (Call (Var "fib") (ArgI (Pos (EVar (Var "n"))))))))) Eps) (Return (Var "res")))) (Procs (Proc (Var "fib") (Arg (Var "m")) (Body (St (ISt (If (BExp (Pos (EVar (Var "m"))) EQQ (Pos (ENum 0))) (St (ASt (Ass (Var "result") (Pos (ENum 1)))) Eps))) (St (ISt (If (BExp (Pos (EVar (Var "m"))) EQQ (Pos (ENum 1))) (St (ASt (Ass (Var "result") (Pos (ENum 1)))) Eps))) (St (ISt (If (BExp (Pos (EVar (Var "m"))) GE (Pos (ENum 1))) (St (ASt (Ass (Var "result") (Term (ECall (Call (Var "fib") (ArgI (Term (EVar (Var "m")) Minus (ENum 1))))) Plus (ECall (Call (Var "fib") (ArgI (Term (EVar (Var "m")) Minus (ENum 2)))))))) Eps))) Eps))) (Return (Var "result")))) Nil))
    it "parses a program that calculates the difference of two numbers by means of a procedure that calculates the difference of two numbers" $ do
          (parse programParseEOF "" "procedure main ( a , b ) {c = diff(a,b); return c ; } procedure diff (a, b) {c = a - b; return c ;} ") `shouldBe` Right (Prog (Main (Args (Var "a") (Arg (Var "b"))) (Body (St (ASt (Ass (Var "c") (Pos (ECall (Call (Var "diff") (ArgsI (Pos (EVar (Var "a"))) (ArgI (Pos (EVar (Var "b")))))))))) Eps) (Return (Var "c")))) (Procs (Proc (Var "diff") (Args (Var "a") (Arg (Var "b"))) (Body (St (ASt (Ass (Var "c") (Term (EVar (Var "a")) Minus (EVar (Var "b"))))) Eps) (Return (Var "c")))) Nil))
    it "parses a program that calculates the factorial of the n-th fibonacci number" $ do
          (parse programParseEOF "" "procedure main (n) {m = fib(n); res = fac(m); return res;} procedure fac (k) {i = 1; fac = 1; if (k < 0) {fac = 0;} else {while (k >= i) {fac = fac*i; i = i+1;} } return fac ; } procedure fib (j) {if (j == 0) { result = 1; } if (j == 1) { result = 1; } if (j > 1) { result = fib(j - 1) + fib(j - 2); } return result;}") `shouldBe` Right (Prog (Main (Arg (Var "n")) (Body (St (ASt (Ass (Var "m") (Pos (ECall (Call (Var "fib") (ArgI (Pos (EVar (Var "n"))))))))) (St (ASt (Ass (Var "res") (Pos (ECall (Call (Var "fac") (ArgI (Pos (EVar (Var "m"))))))))) Eps)) (Return (Var "res")))) (Procs (Proc (Var "fac") (Arg (Var "k")) (Body (St (ASt (Ass (Var "i") (Pos (ENum 1)))) (St (ASt (Ass (Var "fac") (Pos (ENum 1)))) (St (ISt (Elif (BExp (Pos (EVar (Var "k"))) LE (Pos (ENum 0))) (St (ASt (Ass (Var "fac") (Pos (ENum 0)))) Eps) (St (WSt (While (BExp (Pos (EVar (Var "k"))) GEQ (Pos (EVar (Var "i")))) (St (ASt (Ass (Var "fac") (Term (EVar (Var "fac")) Times (EVar (Var "i"))))) (St (ASt (Ass (Var "i") (Term (EVar (Var "i")) Plus (ENum 1)))) Eps)))) Eps))) Eps))) (Return (Var "fac")))) (Procs (Proc (Var "fib") (Arg (Var "j")) (Body (St (ISt (If (BExp (Pos (EVar (Var "j"))) EQQ (Pos (ENum 0))) (St (ASt (Ass (Var "result") (Pos (ENum 1)))) Eps))) (St (ISt (If (BExp (Pos (EVar (Var "j"))) EQQ (Pos (ENum 1))) (St (ASt (Ass (Var "result") (Pos (ENum 1)))) Eps))) (St (ISt (If (BExp (Pos (EVar (Var "j"))) GE (Pos (ENum 1))) (St (ASt (Ass (Var "result") (Term (ECall (Call (Var "fib") (ArgI (Term (EVar (Var "j")) Minus (ENum 1))))) Plus (ECall (Call (Var "fib") (ArgI (Term (EVar (Var "j")) Minus (ENum 2)))))))) Eps))) Eps))) (Return (Var "result")))) Nil)))
