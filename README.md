cbtrie_assoc
--
An associative array based on [Critbit-Trie](https://cr.yp.to/critbit.html) in Fortran 2003.
- This project has been started to learn Fortran 2003's new facilities.
It may not be efficient enough to use practically.

Associative array(ASSOC) is one of the most basic data containers to record key-value pairs.
Hash-map and Btree-map are major data structures to implement it.
Each of the structures has a weak point.
Hash cannot keep the order of keys.
Btree is to take a time depending on the number of keys to do search operation.
Crit-Bit tree is a trie system which has a good balance against the problems.

#### Usage
```Fortran:test.f03
program main
    use assoc_critbit_trie
    implicit none

    type(assoc) :: kvs
    
    call kvs%init                   !initialization
    call kvs%put('Hello', 'assoc')  !put key-value pair
    print *, kvs%get('Hello')       !get 'Hello' => 'assoc'
    call kvs%del('Hello')           !deletion
    call kvs%drop                   !destruction
end program main
```

```shell
% gfortran -o test string_array.f03 assoc_critbit.f03 test.f03
% ./test
 assoc
```

#### Methods of Type(assoc) in assoc_critbit_trie module
###### Subroutines
- call init  
Initializer
- call drop  
Destructor
- call put(key, value)  
Register a pair of key-value.
- call del(key)  
Delete a pair specified by the key.
- call keys(handle)  
Give all keys.
'handle' is an array of type(strarray) in util_string_array module.

###### Functions
- get(key)  
Give the value binding to the key.
- have(key)  
Ask if the key is in the assoc.
- first()  
Give the first key according to ascii order.
- last()  
Give the last key according to ascii order.
- next(str)  
Give the key following to 'str' according to ascii order.
str does not have to exist in the assoc.
- prev(str)  
Give the key followed by 'str' according to ascii order.
str does not have to exist in the assoc.

#### Usage2
Traverse all the keys of the assoc
```Fortran:test2.f03
program main
    use util_string_array
    use assoc_critbit_trie
    implicit none

    integer :: i
    character(:), allocatable :: key
    type(assoc) :: kvs
    type(strarray), allocatable :: hd(:)
    
    call kvs%init
    call kvs%put('foo', 'value1')
    call kvs%put('bar', 'value2')
    call kvs%put('baz', 'value3')

    call kvs%keys(hd)
    do i=1, size(hd)
        key = he(i)%get()
        print *, key, ':  ', kvs%get(key)
        deallocate(key)
    end do
end program main
```

```shell
% gfortran -o test2 string_array.f03 assoc_critbit.f03 test2.f03
% ./test2
 bar:  value2
 baz:  value3
 foo:  value1
```

