'comment' Целые числа от 2 до 100: решето Эратосфена;
'begin'
    'boolean' 'array' prime [2:100];
    'integer' n, k;

    'comment' Обнулим prime;
    'for' n:=2 'step' 1 'until' 100 'do'
        prime [n] := 'true';

    'comment' Вычислим prime;
    'for' n:=2 'step' 1 'until' 100 'do'
        'if' prime[n] 'then'
            'for' k:=n+n 'step' n 'until' 100 'do'
                prime [k] := 'false';

    'comment' Печать prime;
    'for' n:=2 'step' 1 'until' 100 'do'
        'if' prime[n] 'then'
            print (n);
'end'
'eop'
