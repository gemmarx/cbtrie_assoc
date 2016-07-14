Assoc Critbit Trie
==
An associative array based on [Critbit-Tree Trie](https://cr.yp.to/critbit.html) in Fortran 2003.
- GFortran (>5.x) is recommended. It may work on 4.9, but it cannot on 4.8.
- Character string or numeric primitive data (integer/real/complex) can be used as keys and values.
- This project has been started to learn Fortran 2003's new facilities.
It may not be efficient enough to use practically.

Associative array(ASSOC) is one of the most basic data containers to record key-value pairs.
Hash-map and Btree-map are major data structures to implement it.
Each of the structures, however, has a weak point.
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

## ASSOC class of class_assoc_cbtrie module in assoc_cbtrie.f03
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
Give all keys in 'ks'.
'ks' is an array of typack objects which is in class_typack module.

### Functions of type(assoc) :: kvs
- kvs%have(key)  
Ask if the 'key' is in.
- kvs%get(key)  
Get the value binding to the 'key',
and convert it into a character string.
- kvs%get(key, mold)  
Give the value binding to the 'key'.
The result has the same type as 'mold'.
- kvs%get_type(key)  
Give a type information of value binding to the 'key'.
- kvs%first()  
Give the first key in ascii order.
The result is a typack object.
- kvs%last()  
Give the last key in ascii order.
The result is a typack object.
- kvs%next(str)  
Give the key following to 'str' in ascii order.
'str' does not have to be in.
The result is a typack object.
- kvs%prev(str)  
Give the key followed by 'str' in ascii order.
'str' does not have to be in.
The result is a typack object.
- kvs%nelm()  
Give the number of pairs registered in.

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
    call kvs%put(2.71828, 3.14159d0)

    call kvs%keys(ks)
    do i=1, size(ks)
        print *, ks(i)%get_str(), ' => ', kvs%get(ks(i))
    end do
end program main
```

```bash
$ gfortran -o test2 converter.f03 typack.f03 assoc_cbtrie.f03 test2.f03
$ ./test2
 bar => value2
 baz => value3
 foo => value1
 2.71828008 => 3.1415899999999999
```
All keys and values are internally encoded into byte arrays in typack object.
By this trick, we can deal with character and numeric data without distinction at the same time.
A typack object can be used as a key to consult assoc instead of raw datum.

## TYPACK class of class_typack module in typack.f03
A typack object can encode and decode between a raw datum and a byte array packed with type information.

### Subroutines of type(typack) :: tpk
- call tpk%tpack('foo') / call tpk%tpack(3.14e0)  
Encode a raw datum into a byte array.
It is made with big-endian style,
which has one byte type-signature at its first byte.

### Functions of type(typack) :: tpk
- tpk%get()  
Give the datum as a byte array.
- tpk%tunpack(mold)  
Decode the byte array back to the original datum.
The result has the same type as 'mold'.
- tpk%get_str()  
Decode the byte array back to the original datum,
and convert it into a character string.
If the original is a string, this is the same as tunpack().
- tpk%get_type()  
Give a type information.

