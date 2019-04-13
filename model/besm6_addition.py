#!/usr/bin/python
# -*- encoding: utf-8 -*-

import sys
from math import frexp, ldexp

if len(sys.argv) != 4:
    print "Add two numbers in BESM-6 format"
    print "Usage:"
    print "        besm6_addition op num1 num2"
    print "Parameters:"
    print "        op           -- a+x, a-x, x-a or amx"
    print "        num1, num2   -- 16-digit octal number or float number"
    print "For example:"
    print "        besm6_addition a-x 4614000000000000 4550000000000000"
    print "or:"
    print "        besmword x-a 3072.0 1024.0"
    sys.exit(1)

op = sys.argv[1]
a = sys.argv[2]
b = sys.argv[3]
normalize = 1
rounding = 1

def besm_word(input):
    if not ('.' in input or
            '-' in input or
            'e' in input or
            'E' in input or
            '8' in input or
            '9' in input):
        return int(input[:16], 8)
    (fmantissa, exponent) = frexp(float(input))
    sign = 0
    if fmantissa > 0:
        imantissa = int(fmantissa * (1<<40))
        if imantissa < (1<<39):
            raise Exception("Денормализованное число")
        if exponent > 63:
            raise Exception("Переполнение порядка")
        if exponent < -64:
            raise Exception("Опустошение порядка")
        return imantissa | (exponent+64) << 41
    elif fmantissa < 0:
        imantissa = int((fmantissa + 1.0) * (1<<40))
        if imantissa < 0:
            raise Exception("Денормализованное отрицательное число")
        if imantissa >= (1<<39):
            # Normalize.
            imantissa = int((fmantissa*2 + 1.0) * (1<<40))
            exponent -= 1
        if exponent > 63:
            raise Exception("Переполнение порядка")
        if exponent < -64:
            raise Exception("Опустошение порядка")
        return imantissa | (1<<40) | (exponent+64) << 41
    else:
        return 0

def b6_to_str(ival):
    bits = "{:048b}".format(ival)
    m = int(bits[7:],2) & 017777777777777
    sign = ord(bits[7]) - ord('0')
    e = int(bits[:7],2) & 0177
    fm = float(m) / (1 << 40)
    if sign:
        fm -= 1.0

    return "{}".format("%.14g" % ldexp(fm, e - 64))

print "Аргументы:"
a = besm_word(a)
b = besm_word(b)
print "A = {:016o} = {}".format(a, b6_to_str(a))
print "B = {:016o} = {}".format(b, b6_to_str(b))

def summation(a1, a2, trace):
    sum_bits   = [c == '1' for c in a1][::-1]
    carry_bits = [c == '1' for c in a2][::-1]

    carry_set = True
    count = 1
    while carry_set:
        carry_set = False
        if trace:
            print "  Этап суммирования: {:d}".format(count)
            print "  S  = {}".format(''.join(['1' if c else '0' for c in sum_bits[::-1]]))
            print "  Ci = {}".format(''.join(['1' if c else '0' for c in carry_bits[::-1]]))

        for i in range(len(a1)):
            temp          = sum_bits[i] != carry_bits[i]
            carry_bits[i] = sum_bits[i] &  carry_bits[i]
            sum_bits[i]   = temp
            if carry_bits[i]:
                carry_set = True

        if trace: print "  Co = {}".format(''.join(['1' if c else '0' for c in carry_bits[::-1]]))
        if carry_set:
            if trace: print "  Был перенос, сдвигаем регистр переносов"
            carry_bits.insert(0, False)
            carry_bits = carry_bits[:len(a1)]
        count += 1
    if trace:
        print "  Суммирование завершено"
        print "  S  = {}".format(''.join(['1' if c else '0' for c in sum_bits[::-1]]))
    return ''.join(['1' if c else '0' for c in sum_bits[::-1]])

print "\nРаспаковываем внутр. представление"
print "Для числа A:"
a = '{:048b}'.format(a)
ae = a[0:7]
am = a[7:]
print "\texponent = {} = {:d} = 2^{:d}".format(ae, int(ae,2), int(ae,2)-64)
print "\tmantissa = {} {}".format(am[0], am[1:])

print "Для числа B:"
b = '{:048b}'.format(b)
be = b[0:7]
bm = b[7:]
print "\texponent = {} = {:d} = 2^{:d}".format(be, int(be,2), int(be,2)-64)
print "\tmantissa = {} {}".format(bm[0], bm[1:])

def negate(e,m):
    inv = ''.join(['0' if c == '1' else '1' for c in m])
    a2  = '0'*40 + '1'
    temp = summation(inv, a2, False)
    if temp[0] == m[0]:
        print "  Требуется сдвиг мантиссы вправо"
        if m[0] == '1':
            temp = '0' + temp[:-1] # TODO теряем бит?
        else:
            temp = '1' + temp[:-1]
        e = int(e,2) + 1
        if e > 127:
            raise Exception("Переполнение порядка при отрицании") # TODO такое может быть?
        e = '{:07b}'.format(e)
    return (e,temp)

if op == 'a+x':
    print "\nВыполняем сложение A+B:"
elif op == 'a-x':
    print "\nВыполняем вычитание A-B"
    print "Для этого обращаем B"
    be, bm = negate(be, bm)
elif op == 'x-a':
    print "\nВыполняем обратное вычитание B-A"
    print "Для этого обращаем A"
    ae, am = negate(ae, am)
elif op == 'amx':
    print "\nВыполняем вычитание модулей |A|-|B|"
    if am[0] == '1':
        print "Обращаем A"
        ae, am = negate(ae, am)
    if bm[0] == '0':
        print "Обращаем B"
        be, bm = negate(be, bm)
else:
    raise Exception("Неправильная операция")

print "\nВыравниваем порядки"
rmr = '0'
need_rounding = False # True, it at least one "1" has been shifted to RMR

if (int(ae,2) < int(be,2)):
    diff = int(be,2)-int(ae,2)
    print "Порядок A < B, поэтому необходимо сдвинуть со знаком вправо мантиссу A"
    print "на количество разрядов: {}".format(diff)
    ae = be
    rmr = (am[0]*diff + am)[41:]
    need_rounding = '1' in rmr
    am  = (am[0]*diff + am)[:41]
    print "\texponent = {}".format(ae)
    print "\tmantissa = {} {}".format(am[0], am[1:])
elif (int(ae,2) > int(be,2)):
    diff = int(ae,2)-int(be,2)
    print "Порядок A > B, поэтому необходимо сдвинуть со знаком вправо мантиссу B"
    print "на количество разрядов: {}".format(diff)
    be = ae
    rmr = (bm[0]*diff + bm)[41:]
    need_rounding = '1' in rmr
    bm = (bm[0]*diff + bm)[:41]
    print "\texponent = {}".format(be)
    print "\tmantissa = {} {}".format(bm[0], bm[1:])
else:
    print "  Ничего делать не нужно, порядки равны"
rmr = rmr[0:48]


print "\nРегистр младших разрядов содержит"
print "  RMR = {}".format(rmr)

print "\nПеред сложением расширяем мантиссу со знаком до 42 бит"
am = am[0] + am
bm = bm[0] + bm
print "\tA m = {}".format(am)
print "\tB m = {}".format(bm)

print "\nВыполняем сложение мантисс"
sum_ab = summation(am ,bm, True)


print "\nВыполняем анализ 42,41-го бита: {}".format(sum_ab[:2])
if sum_ab[0] != sum_ab[1]:
    print "  Выполняем коррекцию порядка +1"
    aet = int(ae,2) + 1
    print "  exp = {:08b} = {:d}".format(aet,aet)
    rmr = sum_ab[-1]+rmr
    sum_ab = sum_ab[:-1]
    print "  S = {}".format(sum_ab)
    if aet > 127:
        print "  Произошло переполнение порядков"
        raise Exception("Произошло переполнение порядков")
    ae = '{:07b}'.format(aet)
else:
    print "  Коррекция порядков не требуется"
    sum_ab = sum_ab[1:]
    print "  S = {}".format(sum_ab)

def normalize(e,m,rmr):
    e = int(e,2)
    count = 1
    if m[0] != m[1]:
        print "  Нормализация не требуется"
    while m[0] == m[1]:
        print "  Шаг нормализации влево №{} т.к. 41,40 бит = {}".format(count, m[:2])
        e = e - 1
        m = m[1:] + rmr[0]
        rmr = rmr[1:] + '0'
        if e < 0 or ('1' not in m and '1' not in rmr):
            e = 0
            m = '0'*41
            rmr = '0' # TODO нужно ли гасить?
            print "  Произошел underflow"
            break
        print "  m = {}".format(m)
        count += 1
    e = '{:07b}'.format(e)
    return (e,m,rmr)

if normalize:
    print "\nВыполняем нормализацию результата"
    e,m,rmr = normalize(ae, sum_ab, rmr)
    print "\texponent = {} = {:d} = 2^{:d}".format(e, int(e,2), int(e,2)-64)
    print "\tmantissa = {} {}".format(m[0], m[1:])

if rounding:
    print "\nВыполняем округление"
    print "\tRMR = {}".format(rmr)
    if need_rounding:
        m = m[:-1] + '1'
    else:
        print "\tОкругление не требуется"

print "\texponent = {} = {:d} = 2^{:d}".format(e, int(e,2), int(e,2)-64)
print "\tmantissa = {} {}".format(m[0], m[1:])

r = int(e+m,2)
print "\nРезультат:"
print "  {:016o} = {}".format(r, b6_to_str(r))
