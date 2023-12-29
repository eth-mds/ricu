# cli progress

    Code
      with_progress(do_stuff(pb), pb)
    Message
      * a
        ( ) hello from index 1
        ( ) hello from index 2
        ( ) hello from index 3
      * abcd abcd abcd abcd
        ( ) hello from index 4
        ( ) hello from index 5
        ( ) hello from index 6
      * abcdefg abcdefg abcdefg abcdefg abcdefg abcdefg abcdefg
        ( ) hello from index 7
        ( ) hello from index 8
        ( ) hello from index 9
      * abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij
        abcdefghij abcdefghij abcdefghij abcdefghij
        ( ) hello from index 10
      --------------------------------------------------------------------------------
    Output
      NULL

---

    Code
      with_progress(do_stuff(pb), pb)
    Message
      * a
        ( ) hello from index 1
        ( ) hello from index 2
        ( ) hello from index 3
      * abcd abcd abcd abcd
        ( ) hello from index 4
        ( ) hello from index 5
        ( ) hello from index 6
      * abcdefg abcdefg abcdefg abcdefg abcdefg abcdefg abcdefg
        ( ) hello from index 7
        ( ) hello from index 8
        ( ) hello from index 9
      * abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij abcdefghij
        abcdefghij abcdefghij abcdefghij abcdefghij
        ( ) hello from index 10
      --------------------------------------------------------------------------------
    Output
      NULL

