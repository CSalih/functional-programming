(*Author: Christian Sternagel*)

section \<open>Solutions to Exercises of 7th PS\<close>

theory S07
  imports Exercises
begin

subsection \<open>Exercise 2\<close>

fun map :: "('a \<Rightarrow> 'b) \<Rightarrow> 'a lst \<Rightarrow> 'b lst"
  where
    "map f NIL = NIL"
  | "map f (CONS x xs) = CONS (f x) (map f xs)"

lemma map_map:
  "map f (map g xs) = map (f \<circ> g) xs"
  by (induct xs) auto

subsection \<open>Exercise 3\<close>

fun filter :: "('a \<Rightarrow> bool) \<Rightarrow> 'a lst \<Rightarrow> 'a lst"
  where
    "filter P NIL = NIL"
  | "filter P (CONS x xs) = (if P x then CONS x (filter P xs) else filter P xs)"

lemma filter_map:
  "filter P (map f xs) = map f (filter (P \<circ> f) xs)"
  by (induct xs) auto

subsection \<open>Exercise 4\<close>

lemma map_app:
  "map f (app xs ys) = app (map f xs) (map f ys)"
  by (induct xs) auto

subsection \<open>Exercise 5\<close>

text \<open>
Remark for the attentive: In Haskell functions like "take" and "drop" take an "Int" parameter.
However, the standard order on the integers is not well-founded and thus induction does not work.

But, the specific definitions of "take" and "drop" on slides satisfy (just by their defining
equations, without using induction) "take n xs = []" and "drop n xs = xs" for all "n <= 0".

Thus, the properties we have to prove trivially hold whenever "n <= 0", which justifies the usage
of natural numbers in our definitions below.

(An obvious alternative would be to stick to "int"s also in Isabelle and use induction over "xs"
instead of "n" also for the two properties below and then in the step case make a case analysis on
whether "n < 0" or not.
\<close>

fun take :: "nat \<Rightarrow> 'a lst \<Rightarrow> 'a lst"
  where
    "take (Suc n) (CONS x xs) = CONS x (take n xs)"
  | "take _ _ = NIL"

lemma take_map:
  "take n (map f xs) = map f (take n xs)"
proof (induct n arbitrary: xs)
  case 0
  then show ?case by simp
next
  case (Suc n)
  then show ?case by (cases xs) auto
qed

subsection \<open>Exercise 6\<close>

fun drop :: "nat \<Rightarrow> 'a lst \<Rightarrow> 'a lst"
  where
    "drop (Suc n) (CONS _ xs) = drop n xs"
  | "drop _ xs = xs"

lemma drop_map:
  "drop n (map f xs) = map f (drop n xs)"
proof (induct n arbitrary: xs)
  case 0
  then show ?case by simp
next
  case (Suc n)
  then show ?case by (cases xs) auto
qed

end
