(class +KVMap)
# index

(dm put> (Key Val)
  (let Node (kvmap-node Key)
    (if (idx (:: index) Node T)
        (set (caar @) Val)
        (set (car Node) Val))))

(dm get> (Key Not-found)
  (let Node (kvmap-node Key)
    (if (idx (:: index) Node)
        (val (caar @))
        Not-found)))

(dm delete> (Key Not-found)
  (let (Dying-value (get> This Key Not-found)
        Node (kvmap-node Key))
    (idx (:: index) Node NIL)
    Dying-value))

(dm for> (KV-lambda)
  (let (Keys NIL Vals NIL)
    (for Node (idx (:: index))
      (push 'Keys (cdr Node))
      (push 'Vals (val (car Node))))
    (mapc KV-lambda Keys Vals)))

(dm show> ()
  (if (idx (:: index))
      (for> This '((K V) (println K '=> V)))
      (prinl "<Empty>"))
  T)

(de kvmap-node (Key)
  (cons (char (hash Key)) Key))

(off *Class)

##--------------------------------------------------------------------
## Example Application (with tests)

`*RunTests

(let (M (new '(+KVMap)) M2 (new '(+KVMap)))
  
  # Add entries to a map.
  (test '(X X)
    (put> M "rick" 42)
    (put> M "andreas" (1 2 3))
    (put> M "alex" '(X X)))

  # Test to see that they are there.
  (test 42      (get> M "rick"))
  (test (1 2 3) (get> M "andreas"))
  (test '(X X)  (get> M "alex"))

  # Another way to see.
  # : (show> M)
  # "andreas" => (1 2 3)
  # "alex" => (X X)
  # "rick" => 42
  # -> T

  # To change an entry, use `put>` (again).
  (test "good dude"
    (put> M "andreas" "good dude")
    (get> M "andreas"))

  # Now, remove an entry.
  (test 42 (delete> M "rick"))

  # See if it's there.  Nope.
  (test NIL (get> M "rick"))

  # But, what if there is an entry with a NIL value?  How would we know
  # if NIL means "not found" or if NIL is the value?  Then, check this
  # way.
  (test "Not Found!" (get> M "rick" "Not Found!"))

  # Here is an example of using `get>` with the Not-found argument for
  # an entry that exists; it just returns the value.
  (test "good dude" (get> M "andreas" "Not Found!"))

  # Now, add an entry with a NIL value and check it.
  (test NIL
    (put> M "politician" NIL)
    (get> M "politician"))

  # Ah, but does NIL mean "not found"?  No.  It means politician's
  # value is NIL. (Of course. :)
  (test NIL
    (get> M "politician" "Could not find"))

  # The Bleed Test: when you can add entries that have the same key to
  # two different maps, then neither value trounces, or "bleeds" into,
  # the other.
  (test (42 0)
    (put> M "rick" 42)
    (put> M2 "rick" 0)
    (mapcar '((M) (get> M "rick")) (list M M2)))
  
  (t (prinl "All tests passed.")))
