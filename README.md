Assoc Critbit Trie
==
An associative array based on [Critbit-Tree Trie](https://cr.yp.to/critbit.html) in Fortran 2003.
- GFortran (>5.x) is required
- Character string or numeric primitive data (integer/real/complex) can be used as keys or values.
The way to deal with numbers is, however, bit complicated.
- This project has been started to learn Fortran 2003's new facilities.
It may not be efficient enough to use practically.

Associative array(ASSOC) is one of the most basic data containers to record key-value pairs.
Hash-map and Btree-map are major data structures to implement it.
Each of the structures has a weak point.
Hash cannot keep the order of keys.
Btree is to take a time depending on the number of keys to do search operation.
Critbit-Tree is a trie system which has a good balance against the problems.

## Usage
##### test.f03
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
$ gfortran -o test converter.f03 typack.f03 assoc_cbtrie.f03 test.f03
$ ./test
 assoc
```

## Type(assoc) class of class_assoc_cbtrie module in assoc_cbtrie.f03
### Subroutines of type(assoc) :: kvs
- call kvs%init  
Initializer
- call kvs%drop  
Destructor
- call kvs%put(key, value)  
Register a pair of key-value.
- call kvs%del(key)  
Delete a pair specified by the 'key'.
- call kvs%keys(ks)  
Give all keys.
'ks' is an array of type(typack) in class_typack module.
- call kvs%get_num(key, value)  
Give a value as a form of numeric type.
'value' must have a right type.

### Functions of type(assoc) :: kvs
- kvs%get(key)  
Give the value binding to the 'key'.
- kvs%have(key)  
Ask if the 'key' is in the assoc.
- kvs%first()  
Give the first key according to ascii order.
- kvs%last()  
Give the last key according to ascii order.
- kvs%next(str)  
Give the key following to 'str' according to ascii order.
str does not have to exist in the assoc.
- kvs%prev(str)  
Give the key followed by 'str' according to ascii order.
str does not have to exist in the assoc.
- kvs%get_type(key)  
Give a type information of value binding to the 'key'
- kvs%nelm()  
Give the number of psirs registerd in.

## Usage2
Traverse all the keys of the assoc
##### test2.f03
```FORTRAN
program main
    use class_typack
    use class_assoc_cbtrie
    implicit none

    integer :: i
    type(assoc) :: kvs
    type(typack), allocatable :: ks(:)
    
    call kvs%init
    call kvs%put('foo', 'value1')
    call kvs%put('bar', 'value2')
    call kvs%put('baz', 'value3')

    call kvs%keys(ks)
    do i=1, size(ks)
        print *, ks(i)%get_str(), ':  ', kvs%get(ks(i))
    end do
end program main
```

```bash
$ gfortran -o test2 converter.f03 typack.f03 assoc_cbtrie.f03 test2.f03
$ ./test2
 bar:  value2
 baz:  value3
 foo:  value1
```

## Type(typack) class of class_typack module in typack.f03
This class provide encoder and decoder between raw data and byte arrays packed with its type information.
All keys and values are encoded into byte arrays internally.
By this trick, we can deal with character and number data without distinction at the same time.
A type(typack) object can be used as a key to consult assoc instead of raw datum.

### Subroutines of type(typack) :: tpk
- call tpk%tpack('foo') / call tpk%tpack(3.14e0)  
Encode raw datum into a byte array.
The byte array is made with big-endian style,
which has one byte type signature at first byte.
- call tpk%tunpack(v)  
Decode the byte array to original datum.
'v' must have a right type.

### Functions of type(typack) :: tpk
- tpk%get()  
Give the datum as a byte array.
- tpk%get_str()  
Give the datum after converting to character string.
If the type is numeric, the string got is different from original one.
And, it cannot be used as a key for assoc.
- tpk%get_type()  
Give a type information.

