#!/usr/bin/python
# -*- encoding: utf-8 -*-
#
# Convert BESM-6 code from SIMH format to BEMSH.
#
import sys, string

if len(sys.argv) != 2:
    print "Usage: simh-to-bemsh input.b6 > output.bemsh"
    sys.exit(1)
input_name = sys.argv[1]

bemsh_name = {
    "вч"   : "a-x     ",
    "вчаб" : "amx     ",
    "вчоб" : "x-a     ",
    "вчп"  : "e-x     ",
    "вчпа" : "e-n     ",
    "выпр" : "ij      ",
    "дел"  : "a/x     ",
    "знак" : "avx     ",
    "зп"   : "atx     ",
    "зпм"  : "stx     ",
    "и"    : "aax     ",
    "или"  : "aox     ",
    "мод"  : "wtc     ",
    "мода" : "utc     ",
    "нед"  : "anx     ",
    "нтж"  : "aex     ",
    "пб"   : "uj      ",
    "пв"   : "vjm     ",
    "пе"   : "u1a     ",
    "пино" : "v1m     ",
    "пио"  : "vzm     ",
    "по"   : "uza     ",
    "рег"  : "mod     ",
    "рж"   : "xtr     ",
    "ржа"  : "ntr     ",
    "рзб"  : "aux     ",
    "сбр"  : "apx     ",
    "сд"   : "asx     ",
    "сда"  : "asn     ",
    "сл"   : "a+x     ",
    "сли"  : "j+m     ",
    "слиа" : "utm     ",
    "слп"  : "e+x     ",
    "слпа" : "e+n     ",
    "слц"  : "arx     ",
    "стоп" : "стоп    ",
    "сч"   : "xta     ",
    "счи"  : "ita     ",
    "счим" : "its     ",
    "счм"  : "xts     ",
    "счмр" : "yta     ",
    "счрж" : "rte     ",
    "увв"  : "ext     ",
    "уи"   : "ati     ",
    "уиа"  : "vtm     ",
    "уии"  : "mtj     ",
    "уим"  : "sti     ",
    "умн"  : "a*x     ",
    "цикл" : "vlm     ",
    "чед"  : "acx     ",
    "э20"  : "*20     ",
    "э21"  : "*21     ",
    "э32"  : "*32     ",
    "э36"  : "*36     ",
    "э46"  : "*46     ",
    "э47"  : "*47     ",
    "э50"  : "*50     ",
    "э51"  : "*51     ",
    "э52"  : "*52     ",
    "э53"  : "*53     ",
    "э54"  : "*54     ",
    "э55"  : "*55     ",
    "э56"  : "*56     ",
    "э57"  : "*57     ",
    "э60"  : "*60     ",
    "э61"  : "*61     ",
    "э62"  : "*62     ",
    "э63"  : "*63     ",
    "э64"  : "*64     ",
    "э65"  : "*65     ",
    "э66"  : "*66     ",
    "э67"  : "*67     ",
    "э70"  : "*70     ",
    "э71"  : "*71     ",
    "э72"  : "*72     ",
    "э73"  : "*73     ",
    "э74"  : "*74     ",
    "э75"  : "*75     ",
    "э76"  : "*76     ",
    "э77"  : "*77     ",
}

#
# Put quotes around octal number.
#
def quote_num(num):
    if num[0] == '-':
        return "-'" + num[1:] + "'"
    return "'" + num + "'"

#
# Print one instruction.
#
def print_cmd(cmd):
    if ' ' in cmd:
        word = cmd.split()
        cmd = word[0]
        param = word[1]
        brace = param.find('(')
        if brace > 0:
            addr = param[:brace]
            reg = int(param[brace+1:-1], 8)
            #print "/", addr, reg, "/"
            param = quote_num(addr) + "(" + str(reg) + ")"
        elif param[0] == '(':
            reg = int(param[1:-1], 8)
            param = "(" + str(reg) + ")"
        else:
            param = quote_num(param)
        return bemsh_name[cmd] + param
    else:
        return bemsh_name[cmd].strip()

#
# Parse the input file.
#
input_file = open(input_name)
in_instr_space = -1
first_instr_addr = 0
cur_addr = 0
ilist = []
print "*"
print "* Converted from %s" % input_name
print "*"
print "m1      start   '1'"
for line in input_file.readlines():
    word = line.split()
    if len(word) < 2 or word[0][0] == ';':
        continue

    if word[0] == 'п':
        # Address of entry.
        addr = int(word[1], 8)
        print "        uj      '%o'" % addr
        ilist += [1, 1]
        continue

    if word[0] == 'в':
        if in_instr_space > 0:
            ilist += [first_instr_addr, cur_addr - 1]
        in_instr_space = 0

        # Advance the address.
        addr = int(word[1], 8)
        print "*------------------------------"
        print "m%-5o  адрес   m1-1+'%o'" % (addr, addr)
        cur_addr = addr
        continue

    if word[0] == 'с':
        if in_instr_space > 0:
            ilist += [first_instr_addr, cur_addr - 1]
        in_instr_space = 0

        # Data word.
        data = int(word[1], 8)
        print "        конд    b'%016o'" % data
        cur_addr += 1
        continue

    if word[0] == 'к':
        if in_instr_space <= 0:
            first_instr_addr = cur_addr
            in_instr_space = 1

        # Instruction word.
        word = ' '.join(word[1:]).split(',')
        #print len(word), word
        if len(word) != 2:
            print "Wrong line:", line
            sys.exit(1)

        lcmd = word[0].strip()
        rcmd = word[1].strip()
        if ';' not in word[1]:
            print "        %s" % print_cmd(lcmd)
            print "        %s" % print_cmd(rcmd)
        else:
            (rcmd, addr) = rcmd.split(';')
            rcmd = rcmd.strip()
            addr = int(addr, 8)
            if lcmd == "пв 34267(5)" and rcmd == "мода":
                # Bug in BEMSH: workaround
                (lcmd, rcmd) = (rcmd, lcmd)
            print "m%-5o  %s" % (addr, print_cmd(lcmd))
            print "        %s" % print_cmd(rcmd)
        cur_addr += 1
        continue

    print "Unknown line:", line
    sys.exit(1)

for i in range(0, len(ilist), 2):
    print "* %05o-%05o" % (ilist[i], ilist[i+1])
print "        fin"
