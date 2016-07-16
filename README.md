Assoc Critbit Trie
==
An associative array based on [Crit-bit trees](https://cr.yp.to/critbit.html) in Fortran 2003.
- GFortran (>5.x) is required because full support for deferred length character is needed.
- Character string or numeric primitive data (integer/real/complex) can be used as keys and values.
- This project has been started to learn Fortran 2003's new facilities.
It may not be efficient enough to use practically.

Associative array(ASSOC) is one of the most basic data containers to record key-value pairs.
Hash-map and Btree-map are major data structures to implement it.
Each of the structures, however, has a weak point.
Hash cannot keep the order of keys.
Btree is to take a time depending on the number of keys to do search operation.
Crit-bit trees is a trie system which has a good balance against the problems.

## Build
```bash
$ make
# "example_assoc_cbtrie" is a executable file. Try it.
$ ./example_assoc_cbtrie
```
libcbtrie.a and *.mod are made at the same time.
Original sources or a library, either one can be used as usual.
```bash
$ gfortran -o TARGET YOURCODE.f03 -L. -lcbtrie
$ gfortran -o TARGET convert.f03 typack.f03 assoc_cbtrie.f03 YOURCODE.f03
# "assoc_cbtiie" depends on "typack" and "convert".
```
<!--
example_{string|deque|typack} are optional examples.
"string" and "deque" have been used in obsolete versions.
This dangling classes are left as stand-alone tools. 
-->


## Usage
Hello assoc
##### test1.f03
```FORTRAN
program main
    use class_assoc_cbtrie
    implicit none

    type(assoc) :: kvs
    
    call kvs%init                   !initialization
    call kvs%put('Hello', 'assoc')  !put key-value pair
    print *, kvs%get('Hello')       !get 'Hello' => 'assoc'
    call kvs%del('Hello')           !deletion
    call kvs%drop                   !destruction
end program main
```

```bash
$ gfortran -o test1 converter.f03 typack.f03 assoc_cbtrie.f03 test1.f03
$ ./test1
 assoc
```

## Methods of ASSOC class
### Subroutines
**init()**  
Initializer

**drop()**  
Destructor

**put(** *key* **,** *value* **)**  
Register a pair of key-value.

**del(** *key* **)**  
Delete a pair specified by the *key*.

**keys(** *ks* **)**  
Give all keys in *ks* in ascii order.
*ks* is an allocatable array of typack objects.

### Functions
**have(** *key* **)**  
Ask if the *key* is in.

**get(** *key* **)**  
Get the value binding to the *key*,
and convert it into a character string.

**get(** *key* **,** *mold* **)**  
Give the value binding to the *key*.
The result has the same type as *mold*.

**get_type(** *key* **)**  
Give a type information of value binding to the *key*.

**first()**  
Give the first key in ascii order.
The result is a typack object.

**last()**  
Give the last key in ascii order.
The result is a typack object.

**next(** *target* **)**  
Give the key following to *target* in ascii order.
*target* does not have to be in.
The result is a typack object.

**prev(** *target* **)**  
Give the key followed by *target* in ascii order.
*target* does not have to be in.
The result is a typack object.

**nelm()**  
Give the number of pairs registered in.

## Usage2
Traverse all the keys of an assoc
##### test2.f03
```FORTRAN
program main
    use class_typack
    use class_assoc_cbtrie
    implicit none

    integer :: i
    type(assoc) :: kvs
    type(typack), allocatable :: ks(:)
    type(typack) :: key
    
    call kvs%init
    call kvs%put('foo', 'value1')
    call kvs%put(1, 1d0)
    call kvs%put('bar', 'value2')
    call kvs%put(10, 2e0)
    call kvs%put('baz', 'value3')
    call kvs%put(2, 3)

    call kvs%keys(ks)
    do i=1, size(ks)
        key = ks(i)
        print *, key%get_type(),    ':  ' , &
                 key%get_str(),     ' => ', &
                 kvs%get_type(key), ':  ' , &
                 kvs%get(key)
    end do
end program main
```

```bash
$ gfortran -o test2 converter.f03 typack.f03 assoc_cbtrie.f03 test2.f03
$ ./test2
 character:  bar => character:  value2
 character:  baz => character:  value3
 character:  foo => character:  value1
 integer:  1 => double:  1.0000000000000000
 integer:  2 => integer:  3
 integer:  10 => real:  2.00000000
```
All keys and values are internally encoded into byte arrays in typack object.
By this trick, we can deal with character and numeric data without distinction at the same time.
A typack object can be used as a key to consult assoc instead of raw datum.

## Methods of TYPACK class
A typack object can encode and decode between a raw datum and a byte array packed with type information.

### Subroutines
**enpack(** *string* **)** / **enpack(** *num* **)**  
Encode a raw datum into a byte array.
It is made with big-endian style,
which has one byte type-signature at its first byte.

### Functions
**get()**  
Give the datum as a byte array.

**depack(** *mold* **)**  
Decode the byte array back to the original datum.
The result has the same type as *mold*.

**get_str()**  
Decode the byte array back to the original datum,
and convert it into a character string.
If the original is a string, this is the same as **depack()**.

**get_type()**  
Give a type information.

