      program main
c---- Целые числа от 2 до 100: решето Эратосфена
      integer prime (100)
c---- Обнулим prime
      do 10 n=2, 100
 10       prime (n) = 1
c---- Вычислим prime
      do 20 n=2, 100
          if (prime(n) .eq. 0) goto 20
              maxk = 100 / n
	      if (maxk .lt. 2) goto 20
              do 30 k=2, maxk
 30               prime (n*k) = 0
 20       continue
c---- Печать prime
      do 40 n=2, 100
 40       if (prime(n) .ne. 0) print 1000, n
      stop
 1000 format (1x, i4)
      end
