GALOIS-FIELDS
=============

This library creates addition and multiplication functions for Galois fields.

The function `deffield*` returns a list containing the addition and multiplication functions for the given Galois field.

    (defun deffield* (&key p (d 1) use-tables)
        => ( #'<+ function> #'<* function>)

The Galois field of order `q` = `(expt p d)` is calculated. The value `p` must be prime.
The value `d` must be a positive integer.
If `use-tables` is true (or if `use-tables` is not specified and `q` is small), then the addition and multiplication tables are stored in arrays to speed up computations with them.
If `use-tables` is false (or if `use-tables` is not specified and `q` is large), then the addition and multiplication are calculated explicitly on each function invocation.

The Galois field elements are encoded as integers from `0` to `q-1`. The element `a0 + a1*x + a2*x^2 + ... ad*x^d` is represented by the integer obtained by evaluating that polynomial for `x` equal to `p`.

Here is an example use:

    (destructuring-bind (gf+ gf*) (deffield* :p 3 :d 2)
      (list (funcall gf+ 7 3)
            (funcall gf* 7 3)))
        => (1 4)

Because: 7 represents (1 + 2x) while 3 represent (0 + x). So, 7 + 3 represents (1 + 3x) = (1 + 0x) = 1.
Similarly, (1 + 2x) * x = (x + 2x^2) which, modulo (1 + x^2), = (1 + x) which is represnted by 4.

The returned functions behave as expected when given any number of arguments.

    (destructuring-bind (gf+ gf*) (deffield* :p 3 :d 2)
      (list (funcall gf+)
            (funcall gf*)
            (funcall gf+ 1 1 3 1)))
        => (0 1 3)

Caveat: Currently, there is no way to specify the irreducible polynomial used in the multiplication.

The library also provides a `deffield` macro for defining global functions for these operations.

    (defmacro deffield (name &key p (d 1) use-tables) ...)

This defines functions based upon the given base-name `name`.

    (deffield gf32 :p 2 :d 5)
    (list (gf32+ 12 7)
          (gf32* 12 7))
       => (11 1)
