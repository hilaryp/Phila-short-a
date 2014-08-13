from shorta import is_tense

# setup for testing old system

from cmu import read_phoneset
from extractFormants import Phone
from plotnik_old import phila_system, cmu2plotnik_code

phoneset = read_phoneset('../cmu_phoneset.txt')

def is_tense_old(word, pron):
    try:
        i = pron.index("AE1")
    except ValueError:
        return "irrelevant"
    phones = []
    for phone in pron:
        myphone = Phone()
        myphone.label = phone
        phones.append(myphone)
    trans = word
    (code, prec_p) = cmu2plotnik_code(i, phones, trans, phoneset, None, "PHILA")
    fm = code.split(".")[1][0]
    fp = code.split(".")[1][1]
    fv = code.split(".")[1][2]
    ps = code.split(".")[1][3]
    fs = code.split(".")[1][4]
    pc = "3"
    pcode = phila_system(i, phones, trans, fm, fp, fv, ps, fs, pc, phoneset)
    if pcode == "33":
        return True
    elif pcode == "3":
        return False

if __name__ == "__main__":
    pron = {}
    for line in open("dict", "r"):
        if line.startswith(';'):
            continue
        (word, pron_string) = line.rstrip().split('  ', 1)
        pron[word] = pron_string.split()

    words = ['MADDER', 'BADNESS', 'BADDEST', 'RAN', 'SWAM', 'MATH', 'SAD',
             'HAND', 'HAM', 'HALF', 'PATH', 'PASS', 'CASH', 'BANG', 'BAT',
             'PAL', 'BAG', 'CAB', 'HAMMER', 'MANAGE', 'PLANET', 'PLANETS', 
             'MANNING', 'CLASSES', 'ASKING', 'CAN', 'BEGAN', 'ANNE',
             'ASPECT', 'CASKET', 'ASKED', 'BASKETBALL', 'BANDSTAND',
             'BACKSTROKE', 'TRANSFER', "GLADDEN", "TRANSMISSION",
             'SANTA', 'GRANDMA', 'RASCAL', "ASKIN'", "PASSIN'", 'PASSING']
#TODO two modules code different vowel in 'FANTASTIC'?
    for word in words:
        word_pron = pron[word]
        old_coding = is_tense_old(word, word_pron)
        new_coding = is_tense(word, word_pron)
        #if old_coding != new_coding:
        print word, old_coding, new_coding
