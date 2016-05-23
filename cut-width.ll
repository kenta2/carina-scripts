[-*- Mode: emacs-lisp -*-]
[-----grammar
 (gz start (prog))
 (gz prog (module))
 (gz module (f decl-mark id :class language-pragma-opt exports imports topdecl-star j
             ::pr("[[language-pragma-opt]]module [[id]] [[exports]] where{\n[[imports]]\n"
                  "[[topdecl-star('',';\n','\n')]]}\n\n")))
 (gz language-pragma ( :language-pragma f id-non-star j
					::pr
					("{-# LANGUAGE [[id-non-star('',',','')]] #-}\n")))
 (gz exports (f export-star j
              ::pr("[[export-star('(',', ',')')]]")) (:export-everything ::pr("")))
 (gz export (id))
 (gz imports (f import-star j
              ::pr("[[import-star('',';\n',';\n')]]")))
 (gz import(id ::pr( "import [[id]]"))
     (f :qualified (original-name ::is id) (new-name ::is id) j
	::pr ( "import qualified [[original-name]] as [[new-name]]")))

(gz type-class (f decl-mark (class-name ::is id) :type-class context-opt  id-non-plus f type-class-decl-star j j
                 ::pr("class [[context-opt]][[class-name]] [[id-non-plus('',' ','')]] where{\n"
                      "[[type-class-decl-star('',';\n','\n')]]}")))
 (gz topdecl (decl) (data)(type-synonym)(newtype)(instance)(type-class))
 (gz type-class-decl (type-signature)(decl))
 (gz type-signature (f :tysig name ret-type-and-params j
                     ::pr("[[name]] :: [[ret-type-and-params]]")))
 (gz instance (f :instance (type ::is id) (name ::is simpletype)  decls j
               ::pr ("instance [[type]] ([[name]]) where [[decls]]")))
 [(gz instance (f :instance context-opt (type ::is id) simpletype-plus :x decls j
                ::pr ("instance [[context-opt]][[type]] [[simpletype-plus('(',')(',')')]] where [[decls]]")))]
 (gz newtype (f decl-mark id :newtype type-vars  type deriving-opt j
              ::pr("newtype [[id]] [[type-vars]] = [[type]][[deriving-opt]]")))
 (gz deriving (:deriving f id-non-plus j
               ::pr(" deriving [[id-non-plus('(',', ',')')]]")))
 (gz id-non (id))
 (gz type-synonym (f decl-mark id :type-synonym type-vars type j
                   ::pr("type [[id]] [[type-vars]] = [[type]]")))
 (gz data (f decl-mark id :data type-vars constrs deriving-opt j ::pr("data [[id]] [[type-vars]] = [[constrs]][[deriving-opt]]")))
 (gz simpletype (f id-non-plus j ::pr ("[[id-non-plus('',' ','')]]")))
 (gz type-vars (f id-non-star j ::pr ("[[id-non-star('',' ','')]]")))
 (gz constrs(f constr-star j ::pr("[[constr-star('',' | ','')]]"))

  )
 (gz field-type-and-param (f param type j
                           ::pr("[[param]] :: [[type]]")))
 (gz type-and-param ( f param type j ::pr("[[type]]")))
 (gz constr(positional-constructor) (field-label-constructor) )
 (gz field-label-constructor(f type-ctor :field f field-type-and-param-star j j
                             ::pr("[[type-ctor]][[field-type-and-param-star('{',', ','}')]]")))

 (gz decls (  decl-star  ::pr("{[[decl-star('\n',';\n','\n')]]}\n")))
 (gz context (:context f a-context-plus j ::pr
              ("[[a-context-plus('(',', ',')')]] => ")))
 (gz a-context [(f (type ::is id) id-non-plus j
		  ::pr("[[type]] [[id-non-plus('',' ','')]]"))]
     (f (class ::is id) type-plus j
	::pr("[[class]] [[type-plus('(',')(',')')]]"))
     )
 (gz forall (:forall f id-non-plus j
		     ::pr("forall [[id-non-plus('',' ','')]] . ")))
 (gz ret-type-and-params
  (forall-opt context-opt type f type-and-param-star j
   ::pr("[[forall-opt]][[context-opt]][[type-and-param-star('',' \x2d> ','')]]"
        (::c "if(my_type_and_param_star->v.size()>0)out(' \x2d> ');")
        "[[type]]")))

 (gz decl (f decl-mark name :fun ret-type-and-params  expr j
           ::pr("[[name]] :: [[ret-type-and-params]];\n"
                "[[name]]"
                (::c "for(many_trees::const_iterator pos = my_ret_type_and_params->my_type_and_param_star->v.begin();pos!= my_ret_type_and_params->my_type_and_param_star->v.end();++pos){"
                 "const tr_type_and_param* t=dynamic_cast<const tr_type_and_param*>(*pos);"
                 "assert(t);"
                 "out(' ');"
                 "t->my_param->print();" "}")
                " = [[expr]]"))
  (f decl-mark name :fun :no-sig ret-type-and-params  expr j
   ::pr("[[name]]"
        (::c "for(many_trees::const_iterator pos = my_ret_type_and_params->my_type_and_param_star->v.begin();pos!= my_ret_type_and_params->my_type_and_param_star->v.end();++pos){"
         "const tr_type_and_param* t=dynamic_cast<const tr_type_and_param*>(*pos);"
         "assert(t);"
         "out(' ');"
         "t->my_param->print();" "}")
        " = [[expr]]"))
  (f decl-mark name :simple  expr j
     ::pr("[[name]] = [[expr]]"))
  )
 (gz name (id))
(gz positional-constructor
    (type-ctor ::pr("[[type-ctor]]"))
  (f   type-ctor typepls-opt j
   ::pr("[[type-ctor]][[typepls-opt]]"))
  (f :tuple type-plus j
   ::pr("[[type-plus('(',', ',')')]]"))
  )
 (gz pattern
  (f  pattern-ctor pattern-star j
   ::pr ("([[pattern-ctor]][[pattern-star('',' ','')]])"))

  (f pattern-ctor :fpat f fpat-star j j
   ::pr ("[[pattern-ctor]][[fpat-star('{',', ','}')]]"))

  (f :ptuple pattern-plus j [pattern-plus cuz :nil exists for empty lists]
   ::pr("[[pattern-plus('(',', ',')')]]"))

  (f :plist pattern-plus j [pattern-plus cuz :nil exists for empty lists]
     ::pr("[[pattern-plus('\x5b',', ','\x5d')]]"))

  (f :pchar astring j ::pr("(\x27[[astring]]\x27)"))

  (f :pstring astring j ::pr("\x22[[astring]]\x22"))

  (f :as id pattern j ::pr("[[id]]@[[pattern]]"))
  )
 (gz pattern-ctor (id) (:cons ::pr ("(:)")) (:nil ::pr ("[]")))

 (gz fpat (f (variable ::is id) pattern j
           ::pr("[[variable]] = [[pattern]]")))

 (gz type
  (f :fn ret-type-and-params j ::pr ("([[ret-type-and-params]])")) (:unit ::pr("()"))
  (positional-constructor))
 (gz typepls (paren-type-plus))
 (gz paren-type (type ::pr( "([[type]])"))
  (f :strict type j ::pr("!([[type]])"))
  (f :generic id j ::pr (" [[id]] ")))
 (gz type-ctor(id)(:list ::pr ("[]"))(:nondet ::pr ("[]")))
 (gz param (id))
 (gz qastring  (astring ::pr("\x22[[astring]]\x22")))

 (gz expr (id) (:mcons ::pr ("(:)")) [(:nil ::pr ("[]"))]
  [(f :pipe expr-plus j ::pr[("[[expr-star('(',' $ ',')')]]")]
   (
    (::c "for(many_trees::const_iterator pos = my_expr_plus->v.begin();"
     "pos!=my_expr_plus->v.end();++pos){")
    "("
    (::c "(*pos)->print();" "}")
    (::c "for(many_trees::const_iterator pos = my_expr_plus->v.begin();"
     "pos!=my_expr_plus->v.end();++pos){")
    ")"
    (::c  "}")
    )
   )]
  (f :join expr-plus j ::pr("[[expr-plus('(',' >>= ',')')]]"))

  (f :rpipe expr-plus j ::pr[("[[expr-star('(',' $ ',')')]]")]
   (["http;//gcc.gnu.org/bugzilla/show_bug.cgi?id=11729"]
    (::c "for(many_trees::reverse_iterator pos = my_expr_plus->v.rbegin();"
     "pos!=my_expr_plus->v.rend();++pos){")
    "("
    (::c "(*pos)->print();" "}")
    (::c "for(many_trees::const_iterator pos = my_expr_plus->v.begin();"
     "pos!=my_expr_plus->v.end();++pos){")
    ")"
    (::c  "}")
    )
   )
  (f :rcompose expr-plus j ::pr
     [("[[expr-plus('(',' . ',')')]]")]
   ("("
    (::c "for(many_trees::reverse_iterator pos = my_expr_plus->v.rbegin();"
     "pos!=my_expr_plus->v.rend();++pos){")
    (::c "if(pos!=my_expr_plus->v.rbegin())")
    " . "
    (::c "(*pos)->print();" "}")
    ")"
    )
   )

  (f :cc expr-star j ::pr ("[[expr-star('(',' ++ ',')')]]"))
  (qastring)
  (f :lit astring j ::pr("[[astring]]"))

  (f :ty type expr j ::pr("([[expr]] :: [[type]])"))
  (f (fun-name ::is expr) expr-star j
   ::pr ("([[fun-name]][[expr-star(' ',' ','')]])"))
  [(f :construct ctor expr-star j
    ::pr ("([[ctor]] [[expr-star('',' ','')]])"))]
  (f :do  stmt-star  j ::pr("(do{\n[[stmt-star('  ','\n  ','\n')]]})"))
  (f :case expr  alt-star  j
   ::pr("(case [[expr]] of {\n[[alt-star('  ',';\n  ','\n')]]})"))
  (f :lcase alt-star j
     ::pr("(\x5clambda_case_var ->"
	  "case lambda_case_var of {\n"
	  "[[alt-star('  ',';\n  ','\n')]]})"))
  (f :let decl-star expr j
   ::pr("(let {[[decl-star('\n',';\n','\n')]]}\n in [[expr]])"))
  (f :cfd (type ::is id) assignments-star j
   ::pr("[[type]][[assignments-star('{',', ','}')]]"))
  [(f :compose (a ::is expr) (b ::is expr) j ::pr("((.)[[a]] [[b]])"))]
  [(:compose ::pr ("(.)"))]
  [(f :compose expr-plus j ::pr ("[[expr-plus('(',' . ',')')]]"))]
  (f :mlist expr-star j ::pr("[[expr-star('\x5b',', ','\x5d')]]"))
  (f :cons-list expr-star j ::pr("[[expr-star('(',':',')')]]"))
  (f :mtuple expr-star j ::pr("[[expr-star('(',', ',')')]]"))
  (:nothing  ::pr ("()"))

  (f :lambda name ret-type-and-params expr j
     ::pr("(let {[[name]] :: [[ret-type-and-params]];\n"
          "[[name]]"
          (::c "for(many_trees::const_iterator pos = my_ret_type_and_params->my_type_and_param_star->v.begin();pos!= my_ret_type_and_params->my_type_and_param_star->v.end();++pos){"
               "const tr_type_and_param* t=dynamic_cast<const tr_type_and_param*>(*pos);"
               "assert(t);"
               "out(' ');"
               "t->my_param->print();" "}")
          " = [[expr]]} in [[name]])"))

  )
 (gz assignments (f id expr j ::pr("[[id]] = [[expr]]")))
 (gz stmt (expr ::pr("[[expr]];"))
  (f ":=" name type expr j ::pr("[[name]] :: [[type]] <- [[expr]];"))
  (f :dlet decl-star j ::pr ("let {[[decl-star('\n',';\n','\n')]]};"))
  )
 (gz alt (f  pattern expr-or-gpat j
             ::pr("[[pattern]][[expr-or-gpat]]")))
 (gz expr-or-gpat
     (expr ::pr ("-> [[expr]]"))
     (f :where decls :gpats pred-expr-star j [silly lookahead limitation]
        ::pr ("\n[[pred-expr-star('','','')]]"
              "where "
              "[[decls]]"
              )))
 (gz pred-expr ( f (pred ::is expr) (do ::is expr) j
                ::pr ("| [[pred]]\n -> [[do]]\n")))

 (gz decl-mark (":"))
]


(: Main :class :language-pragma (ScopedTypeVariables)
		:export-everything
   (
    System.IO
    System.Environment
    [Data.Word]
    [Data.Char]
    [(:qualified System.IO.Unsafe Unsafe)]
    [Array]
    [Data.List]
    [Random]
    [Data.Ord]
    [Maybe]
    [Data.Array.IO]
    )


   (: main :fun (IO :unit)()
      (:do
       (hPutStrLn stderr rcs-code)
       (:join
	getArgs
	(:lcase
         ((:plist(:pstring "nothing"))(return :nothing))
	 [((:plist(:pstring "div")(x)(y))(print (div-rounding-up (read x)(read y))))]
	 ((:plist(:pstring "cut-points")(x)(y))(:rpipe (cut-points (read x)(read y))
						       (map show)
						       unwords
						       putStrLn
						       ))
	 ((:plist(:pstring "test")(x)(y))(test(read x)(read y)))
         ((_)undefined)
         ))
       ))

[   (: show-list :fun :context ((Show(a)))(String)((l(:list(a))))
     (unlines (map show l))
     )

   (: quiet :fun(Bool)() False)

   (: cerr :fun(a)((message (:list(String)))(x(a)))
      (:case quiet
        ((True)x)
        ((_)(Unsafe.unsafePerformIO
             (:do
              (hPutStrLn stderr (concat message))
              (return x))))))
   (: cerr0 :fun(a)((message(String))(x(a)))
      (:case quiet
        ((True)x)
        ((_)(Unsafe.unsafePerformIO
             (:do
              (hPutStrLn stderr message )
              (return x))))))

   (: cerr-x :fun(a)((_(:list(String)))(x(a)))x)

   (: peek :fun :context((Show(a)))(a)((x(a)))
     (seq x (cerr (:mlist(show x)) x)))

   [(: peek-more :fun(a)((f(:fn(:list(String))((x(a)))))(x(a)))
     (:case quiet
       ((True)x)
       ((_)(seq x (cerr  (f x) x)))))]
   (: peek-more-x :fun(a)((_(:fn(:list(String))((x(a)))))(x(a)))x)

   (: zip-map :fun(:list(:tuple(a)(b)))
     ((f(:fn(b)((x(a)))))(l(:list(a))))
     (zip l (map f l)))

   [(: zip-map-parallel :fun(:list(:tuple(a)(b)))
     ((strat(Strategy b))(f(:fn(b)((x(a)))))(l(:list(a))))
     (zip l (using (map f l)
		   (parList strat)
		   )))]

   (: show-and :fun :context ((Show(a))(Show(b)))
     (IO :unit)((f(:fn(b)((x(a)))))(l(:list(a))))
     (:rpipe (zip-map f l) show-list putStr))

   (: ntakes :fun(:list(:list(a)))((n(Int))(l(:list(a))))
     (:mcons(take n l)(ntakes n (tail l)))
     )

   (: enum-from-count :fun :context ((Enum(a))) (:list(a))
     ((start(a))(count(Int)))
     (take count (enumFrom start)))

   (: map-tuple :fun(:tuple(b)(b))((fn(:fn(b)((x(a)))))
                                 (x(:tuple(a)(a))))
     (:mtuple (fn (fst x))(fn (snd x))))

   [(: sort-by-extractor :fun :context ((Ord(b)))(:list(a))
     ((extract(:fn(b)((x(a)))))(l(:list(a))))
     (:let
      (sortBy (compare-by-extractor extract) l)
      ))
    (use sortBy (comparing extract))
    ]
   [(: compare-by-extractor :fun :context ((Ord(b)))(Ordering)
     ((extract(:fn(b)((x(a)))))(x(a))(y(a)))
     (compare (extract x)(extract y))) == comparing]

   (: reverse-comparison :fun (Ordering) ((o(Ordering)))
      (:case o
	     ((LT)GT)
	     ((EQ)EQ)
	     ((GT)LT)
	     ))
   (: reverse-comparison-function :fun
      (:fn(Ordering)((x(a))(y(a))))
      ((f(:fn(Ordering)((x(a))(y(a))))))
      (:rpipe
       (:rcompose (uncurry f) reverse-comparison)
       curry))

   (: n-chunk :fun(:list(:list(a)))((n(Int))(l(:list(a))))
     (:case l
       ((:nil)(:mlist) )
       ((_)(:mcons (take-enough n l) (n-chunk n (drop n l))))
       ))
   (: take-enough :fun (:list a) ((n Int)(l(:list a)))
      (:case (compare 0 n)
	     ((EQ)(:mlist))
	     ((LT)(:case l
			 ((:cons(head)(tail))
			  (:mcons head (take-enough (pred n)(tail))))))))

   [(: zip-check-same-length :fun(:list(:tuple(a)(b)))
     ((x1(:list(a)))(x2(:list(b))))
     (assert (==(length x1)(length x2))
             (zip x1 x2))
     )]
   (: zip-check-same-length :fun (:list(:tuple(a)(b)))
      ((x1(:list(a)))(x2(:list(b))))
      (:case (:mtuple x1 x2)
	     ((:ptuple(:nil)(:nil))(:mlist))
	     ((:ptuple(:cons(a)(arest))
		      (:cons(b)(brest)))
	      (:mcons(:mtuple a b)
		     (zip-check-same-length arest brest)))))

   (: zipWith-check-same-length :fun (:list(c))
      ((f(:fn(c)((x1(a))(x2(b)))))(x1(:list(a)))(x2(:list(b))))
      (:case (:mtuple x1 x2)
	     ((:ptuple(:nil)(:nil))(:mlist))
	     ((:ptuple(:cons(a)(arest))
		      (:cons(b)(brest)))
	      (:mcons(f a b)
		     (zipWith-check-same-length f arest brest)))))
   ]
   (: powers-of-two :fun (:list(Int))()
     (:mcons 1 (map (* 2) powers-of-two)))
   [
   (: binary-to-decimal :fun :no-sig (Int) ((bits(:list(Int))))
     (sum (zipWith * (reverse bits) powers-of-two)))

   [(: compress-rle :fun :context ((Eq a))
     (:tuple(:list(a))(:list(Int)))((l(:list(a))))
     (:let
      (:mtuple (map head g) (map length g))
      (: g :fun :no-sig(:list(:list(a)))()
        (group l)
        ))
     )]
   [(: uncompress-rle :fun(:list(a))((rle(RLE(a)))
                                  )
     (:pipe
      (assert (== (length (fst rle))(length (snd rle))))
      (concatMap (uncurry (flip replicate)) ((uncurry zip-check-same-length)
                                             rle))
      ))]

   (: apply-first :fun(:tuple(b)(c))((fn(:fn(b)((_(a)))))(x(:tuple(a)(c))))
      (:mtuple (fn (fst x))(snd x)))

   (: apply-second :fun(:tuple(c)(b))((fn(:fn(b)((_(a)))))(x(:tuple(c)(a))))
      (:mtuple (fst x)(fn (snd x))))

   (: RLE :type-synonym (a)(:tuple(:list(a))(:list(Int))))

   (: diffs :fun :no-sig (Maybe(:list(Int))) ((l(:list(Int))))
     (:case l
       ((:nil)Nothing)
       ((:cons(h)(t))
        (Just(:mcons h (zipWith - t l))))))
   (: undiff :fun :no-sig (:list(Int))((l(Maybe(:list(Int)))))
     (:case l
       ((Nothing)(:mlist))
       ((Just(l))(scanl1 + l))
       ))

   (: every-n :fun(:list(a))((n(Int))(l(:list(a))))
     (:let
      (: is-mult :fun  (Bool)((i(Int)))
        (== 0 (mod i n)))
      (map snd (filter(:rcompose fst is-mult) (zip (enumFrom 0)l)))
      ))
   (: tab-split :fun(:list(String))((s(String)))
     (:let(: br :fun(:tuple(String)(String))()(break (== (:lit "'\t'"))s))
            (:mcons (fst br)
                    (:case (snd br)
                      ((:nil)(:mlist))
                      ((:cons(:pchar "\t")(rest))(tab-split rest)))
                    )
            ))
   (: second-is-longer :fun(Bool)((small(:list(a)))(big(:list(a))))
     (:case (:mtuple small big)
       ((:ptuple(:nil)(_))True)
       ((:ptuple(_)(:nil))False)
       ((:ptuple(:cons(_)(r1))(:cons(_)(r2)))(second-is-longer r1 r2))
       ))
   (: is-prefix :fun :context ((Eq(a))) (Bool) ((prefix(:list(a)))(l(:list(a))))
     (&&(second-is-longer prefix l)
        (and (zipWith == prefix l))))

   (: is-suffix :fun :context ((Eq(a))) (Bool) ((suffix(:list(a)))(l(:list(a))))
     (is-prefix (reverse suffix) (reverse l)))

   (: concatenate-many-files :fun(IO(String))((files(:list(String))))
     (:join
      (mapM readFile files) (:rcompose concat return))
     )

   [(: glob-directory :fun(IO(:list(FilePath)))
     ((dir(FilePath))(prefix(FilePath))(suffix(FilePath)))
     (:do
      (:= files (getDirectoryContents dir))
      (:pipe
       return
       (map (++ (++ dir "/")))
       (filter (is-suffix suffix))
       (filter (is-prefix prefix))
       files)
      ))]

   [(: concatenate-glob :fun(IO(String))
     ((dir(FilePath))(prefix(FilePath))(suffix(FilePath)))
     (:do
      (:= files (glob-directory dir prefix suffix))
      (concatenate-many-files files)
      ))]

   ["sort-tuple"]
   (: put-in-order :fun :context((Ord(a)))(:tuple(a)(a))((x(:tuple(a)(a))))
     (:case (> (fst x)(snd x))
       ((True)(:mtuple (snd x)(fst x)))
       ((_)x)))

   (: punch-array :fun :context((Ix(a))) (Array(a)(Bool))
      ((bounds(:tuple(a)(a)))
       (l(:list(a))))
      (:let
       (: set-true :fun (Bool)((_(Bool))(_(Bool)))
          True
          )
       (accumArray set-true False bounds
		  (zip l (repeat undefined)))
       ))

   (: build-array-from-list :fun :context((Ix(a))) (Array(a)(:list(b)))
      ((bounds(:tuple(a)(a)))
       (l(:list(:tuple(a)(b)))))
      (accumArray snoc (:mlist) bounds l))

   (: snoc :fun (:list(a)) ((rest(:list(a)))(h(a)))
      (:mcons h rest))

   (: filter-trues :fun (:list(a))((xb(:list(:tuple(a)(Bool)))))
      (:rpipe
       xb (filter snd) (map fst)))

   (: compare-length-lists :fun (Ordering) ((x(:list(a)))(y(:list(a))))
      (:case (:mtuple x y)
        ((:ptuple(:nil)(:nil))EQ)
        ((:ptuple(:nil)(_))LT)
        ((:ptuple(_)(:nil))GT)
        ((_)(compare-length-lists (tail x)(tail y)))))

   (: ordering-to-equality :fun :forall (a)
      (:fn(Bool)((x(a))(y(a))))
      ((compare-function(:fn(Ordering)((x(a))(y(a))))))
      (:let
       (: ret-val :fun (Bool) ((x(a))(y(a)))
	  (:case (compare-function x y)
		 ((EQ)True)
		 ((_)False)
		 ))
       ret-val))
   (: sort-and-group-by :fun (:list(:list(a)))
      ((compare-function(:fn(Ordering)((x(a))(y(a)))))
       (l(:list(a))))
      (:rpipe
       l
       (sortBy compare-function)
       (groupBy ( ordering-to-equality compare-function))
       ))
   (: compare-list-length-with-unity :fun (Ordering) ((l(:list(a))))
      (:case l
        ((:nil)LT)
        ((:cons(_)(:nil))EQ)
        ((_)GT)
        ))

   (: choose-from-list-randomly :fun (IO(a)) ((l(:list(a))))
      (:case l
	     ((:nil)(error "choose-from-list-randomly called on empty list"))
	     ((_)
	      (:join
	       (randomRIO (:mtuple 0 (pred (length l))))
	       (:rcompose (!! l) return)
	       )
	      )))
   (: uniform-random-IO :fun (IO(Float)) ()
      (randomRIO (:mtuple 0 1)))
   (: random-permutation-IO :fun (IO(:list(a))) ((l(:list(a))))
      (:join
       (sequence (replicate (length l) uniform-random-IO))
       (:rcompose
	(zip-check-same-length l)
	(sortBy (comparing snd))
	(map fst)
	return))
      )

   (: combine-maybes-in-io :fun :context((Monad(io)))
      (io(Maybe(a))) ((l-to-do(:list(io(Maybe(a))))))
      (:case l-to-do
	     ((:nil)(return Nothing))
	     ((:cons(h)(rest))
	      (:join
	       h
	       (:lcase ["there's no generic way to test for failure?"]
		      ((Just(_))h)
		      ((_)(combine-maybes-in-io rest))
		      ))
	      )
	     )
      )

   (: is-singleton-list :fun (Bool)((l(:list(a))))
      (:case l
	     ((:cons(_)(:nil))True)
	     ((_)False)
	     ))
   (: first-last :fun (:tuple(a)(a))((l(:list(a))))
      (:mtuple (head l)(last l)))

   (: list-change-at-index :fun (:list(a))((l(:list(a)))(n(Int))(new-value(a)))
      (:case l
	     ((:cons(head)(tail))
	      (:case (compare 0 n)
		 ((EQ)(:mcons new-value tail))
		 ((LT)
		  (:mcons head
			  (list-change-at-index tail (pred n) new-value))))))
      [(:cc (take n l) (:mlist new-value) (drop (+ 1 n) l))
       does not fail for out of index
       ]
      )
   (: backwards-tuple :fun (:tuple(b)(a))((x(:tuple(a)(b))))
      (:case x
	     ((:ptuple(i)(j))(:mtuple j i))))

   (: mk-1-map-array :fun (Array(Int)(b))
      ((fn(:fn(b)((x(a)))))
       (la(:list(a))))
      (listArray (:mtuple 1 (length la))
		 (map fn la)))
   (: filter-justs :fun (:fn(:list a) ((ll(:list(Maybe a)))))()
      (:rcompose
       (filter isJust)
       (map fromJust)))

   (: head-only :fun a ((l(:list a)))
      (:case l
	     ((:plist(x))x)
	     ))
   [(: tail-assert :fun :context ((Eq a))(:list a)
      ((x(a))(l(:list a)))
      (assert
       (== x (head l))
       (tail l)))]

   [(: continue-through :fun b ((pred(Bool))(x(b)))
      (:case pred
	     ((True)x)))]

   (: singleton :fun (:list a) ((x a))
      (:mlist x))
   (: zero-array :fun (Array Int a)((l(:list a)))
      (listArray (:mtuple 0 (:rpipe l length pred)) l))
   ]
   (: rcs-code :fun String ()
      "$Id: cut-width.ll,v 1.3 2016/05/23 01:39:42 kenta Exp $")

   (: div-rounding-up :fun Int ((x Int)(y Int))
      (:case (divMod x y)
	     ((:ptuple(d)(0))d)
	     ((:ptuple(d)(_))(succ d))))

   (: cut-points :fun (:list Int) ((big Int)(small Int))
      (:let
       (: numpieces :fun Int () (pred(div-rounding-up big small)))
       (: last-one :fun Int () (- big small))
       (: f :fun Int ((i Int))
	  (div (* i last-one) numpieces))
       (:rpipe
	(enumFromTo 0 numpieces)
	(map f))
       ))
   (: one-pnmcut :fun String ((height Int)(vcut Int))
      (:cc "pnmcut 0 "(show vcut)" 0 "(show height)" a "))
   (: one-pipeline :fun String ((rescale Rescale)(rotations(:list String))(hcut Int)(height Int)(vcut Int))
      (:do
       (:= rot String rotations)
       (:cc
	(one-pnmcut height vcut)
	(do-rescale rescale)
	(do-rotation rot)
	"| cjpeg > x-" (show rescale)"-"(show hcut)"-"(show vcut)rot".jpg\n"))
      )
   (: Rescale :data () ((Rescale(Int))))
   (: do-rescale :fun String ((rscale Rescale))
      (:case rscale
	     ((Rescale(1))(:mlist))
	     ((Rescale(x))
	      (:cc
	       "| pnmscale "(show (/ (:ty Double 1.0) (fromIntegral x)))" "))))
   (: do-rotation :fun String ((rot String))
      (:case rot
	     ((:nil)(:mlist))
	     ((_)(:cc "| pnmflip "rot" "))))
   (:instance Show (Rescale)
	      (: show :fun :no-sig Int ((x(Rescale)))
		 (:case x
			((Rescale(y))(:cc(show y))))))
   (: big-column :fun String
      ((big-height Int)(rescale Rescale)(rotations(:list String))(width Int)(height Int)(hcut Int))
      (:cc
       "pnmcut "(show hcut)" 0 "(show width)" 0 original >| a\n"
       (concatMap
	(one-pipeline rescale rotations hcut height)
	(cut-points big-height height))))
   (: big-big :fun String
      ((big-width Int)(big-height Int)(rescale Rescale)(rotations(:list String))(width Int)(height Int))
      (concatMap
       (big-column big-height rescale rotations width height)
       (cut-points big-width width)))
   (: double-size :fun (:tuple Int Int)((x(:tuple Int Int)))
      (:mtuple (* 2 (fst x))(* 2 (snd x))))
   (: fits :fun Bool ((width Int)(height Int)(x Int)(y Int))
      (&&(< x width)(< y height)))
   (: size-series :fun (:list (:tuple Int(:tuple Int Int)))
      ((width Int)(height Int)(monitor-width Int)(monitor-height Int))
      (zip
       powers-of-two
       (:rpipe
	(:mtuple monitor-width monitor-height)
	(iterate double-size)
	(takeWhile (uncurry (fits width height))))))
   (: run-big-big :fun String
      ((big-width Int)(big-height Int)(rotations(:list String))(rwh(:tuple Int (:tuple Int Int))))
      (:case rwh
	     ((:ptuple(rescale)(:ptuple(width)(height)))
	      (big-big big-width big-height (Rescale rescale) rotations width height))))
   (: run-rescales :fun String ((big-width Int)(big-height Int)(width Int)(height Int))
      (++
       (concatMap (run-big-big big-width big-height (:mlist "-r90" "-r270"))
		  (size-series big-width big-height height width))
       (concatMap (run-big-big big-width big-height (:mlist "" "-r180"))
		  (size-series big-width big-height width height))
       ))
   (: test :fun (IO :unit)((width Int)(height Int))
      (:do
       (putStrLn"set -x")
       (putStrLn"set -C")
       (putStrLn"set -e")
      (putStr(run-rescales [29566 14321 1360 768]
			   29566 14321 width height
			   ))))
)

