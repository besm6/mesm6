#!/usr/bin/python
# -*- encoding: utf-8 -*-

import sys
from math import frexp, ldexp

if len(sys.argv) != 2:
    print "Change a sign of BESM-6 number"
    print "Usage:"
    print "        besm6_chsign num"
    print "Parameters:"
    print "        num      -- 16-digit octal number or float number"
    print "For example:"
    print "        besm6_chsign 4020000000000000"
    print "or:"
    print "        besm6_chsign -1.0"
    sys.exit(1)

a = sys.argv[1]
normalize = 1

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

print "Аргумент:"
a = besm_word(a)
print "  {:016o} = {}".format(a, b6_to_str(a))

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
a = '{:048b}'.format(a)
ae = a[0:7]
am = a[7:]
print "  exponent = {} = {:d} = 2^{:d}".format(ae, int(ae,2), int(ae,2)-64)
print "  mantissa = {} {}".format(am[0], am[1:])

print "\nРасширяем мантиссу со знаком до 42 бит"
am = am[0] + am
print "  m = {}".format(am)

print "\nВыполняем изменение знака:"
am_inv = ''.join(['0' if c == '1' else '1' for c in am])
const1 = '0'*41 + '1'
am = summation(am_inv, const1, False)
print "  exponent = {} = {:d} = 2^{:d}".format(ae, int(ae,2), int(ae,2)-64)
print "  mantissa = {} {}".format(am[0], am[1:])

print "\nВыполняем анализ 42,41-го бита: {}".format(am[:2])
rmr = '0'
rmr = rmr[0:48]
if am[0] != am[1]:
    print "  Выполняем коррекцию порядка +1"
    aet = int(ae,2) + 1
    print "  exp = {:08b} = {:d}".format(aet,aet)
    rmr = am[-1]+rmr
    am = am[:-1]
    print "  S = {}".format(am)
    if aet > 127:
        print "  Произошло переполнение порядков"
        raise Exception("Произошло переполнение порядков")
    ae = '{:07b}'.format(aet)
else:
    print "  Коррекция порядков не требуется"
    am = am[1:]
    print "  S = {}".format(am)

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
    e,m,rmr = normalize(ae, am, rmr)
    print "  exponent = {} = {:d} = 2^{:d}".format(e, int(e,2), int(e,2)-64)
    print "  mantissa = {} {}".format(m[0], m[1:])

print "  exponent = {} = {:d} = 2^{:d}".format(e, int(e,2), int(e,2)-64)
print "  mantissa = {} {}".format(m[0], m[1:])

r = int(e+m,2)
print "\nРезультат:"
print "  {:016o} = {}".format(r, b6_to_str(r))
