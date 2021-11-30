import ply.yacc as yacc
import ply.lex as lex

literals = ['=', '+', '-', '*', '/', '(', ')','^','{','}',';']

reserved = {
    'int': 'INTDEC',
    'float': 'FLOATDEC',
    'print': 'PRINT',
    'while': 'WHILE',
    'for': 'FOR',
    # === Bools and logical operations
    'boolean': 'BOOLEAN',
    'string': 'STRING',
    'and': 'AND',
    'or': 'OR',
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE'
}

tokens = [
            #                   Equals, !Equals, Greater or Equal to, Less or Equal to
             'INUMBER', 'FNUMBER', 'STRING_V','NAME', 'EQUAL', 'NOTEQUAL', 'GOEQUAL', 'LOEQUAL', 'GT', 'LT'
         ] + list(reserved.values())

# Tokens

t_EQUAL = r'=='
t_NOTEQUAL = r'!='
t_GOEQUAL = r'>='
t_LOEQUAL = r'<='
t_GT = r'>'
t_LT = r'<'
t_ignore = " \t"


def t_STRING_V(t):
    r'".*"'
    t.value = t.value.replace("\"", "")
    t.type = reserved.get(t.value, 'STRING_V')
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'NAME')  # Check for reserved words
    return t

def t_FNUMBER(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Build the lexer
lexer = lex.lex()

# Parsing rules

#left: left side first, right: right side first, nonassoc: no chaining operations
#position in precedence defines what goes first
precedence = (
    
    ('left', '+', '-'),
    ('left', '*', '/'),
    # Note to self: UMINUS -> Multiplying something by -1
    ('right', 'UMINUS'),
    
)

# dictionary of names
names = {}


def p_start(p):
    '''s : segment '''
    global abstractTree
    abstractTree = p[1]

def p_segment(p):
    '''segment : while segment
            | for segment
            | conditional segment
            | statement ';' segment
            | declaration ';' segment 
            | '''
    if(len(p) > 2):
        if(p[2] == ';'):
            p[2] = p[3]
        p[0] = (p[1],) + p[2]
    else:
        p[0] = ()

def p_declaration(p):
    '''declaration : declareWithVal
                |   declareVar
                |   assign'''
    p[0] = p[1]

def p_declare(p):
    '''declareWithVal : dataType NAME '=' expression '''
    p[0] = ('complete_declaration', p[1], p[2], p[4])
    names[p[2]] = p[4]

def p_declarevar(p):
    '''declareVar : dataType NAME'''
    p[0] = ('declaration', p[1], p[2]) 
    names[p[2]] = ''

def p_assign(p):
    '''assign : NAME '=' expression '''
    p[0] = ('assign',p[1],p[3])
    names[p[1]] = p[3]

def p_dataType(p):
    '''dataType : INTDEC
                | FLOATDEC
                | BOOLEAN
                | STRING'''
    p[0] = p[1]

def p_statement_print(p):
    '''statement : PRINT '(' expression ')' '''
    p[0] = ('print',p[3])

def p_expression_binop_comparison(p):
    '''expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression
                  | expression '^' expression
                  '''
    if p[2] == '+':
        p[0] = ('operation',p[1],'+',p[3])
    elif p[2] == '-':
        p[0] = ('operation',p[1],'-',p[3])
    elif p[2] == '*':
        p[0] = ('operation',p[1],'*',p[3])
    elif p[2] == '/':
        p[0] = ('operation',p[1],'/',p[3])
    elif p[2] == '^':
        p[0] = ('operation',p[1],'^',p[3]) 

def p_logic_expressions(p):
    '''expression : expression LT expression
                  | expression GT expression
                  | expression EQUAL expression
                  | expression NOTEQUAL expression
                  | expression GOEQUAL expression
                  | expression LOEQUAL expression
                  | expression AND expression
                  | expression OR expression'''
    if p[2] == '<':
        p[0] = ('comparison', p[1], '<' ,p[3])
    elif p[2] == '>':
        p[0] = ('comparison', p[1], '>' ,p[3])
    elif p[2] == '==':
        p[0] = ('comparison', p[1], '==' ,p[3])
    elif p[2] == '<=':
        p[0] = ('comparison', p[1], '<=' ,p[3])
    elif p[2] == '>=':
        p[0] = ('comparison', p[1], '>=' ,p[3])
    elif p[2] == '!=':
        p[0] = ('comparison', p[1], '!=' ,p[3])
    elif p[2] == 'and':
        p[0] = ('comparison', p[1], 'and' ,p[3])
    elif p[2] == 'or':
        p[0] = ('comparison', p[1], 'or' ,p[3])

def p_expression_uminus(p):
    "expression : '-' expression %prec UMINUS"
    p[0] = -p[2]

def p_expression_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

def p_expression_val(p):
    '''expression : INUMBER
                | FNUMBER
                | BOOLEAN
                | STRING_V  '''
    p[0] = p[1]

def p_expression_name(p):
    '''expression : NAME'''
    p[0] = names[p[1]]

def p_if(p):
    '''conditional : IF '(' expression ')' '{' segment '}' elif else '''
    p[0] = ('if', p[3], p[6], p[8], p[9])

def p_elif(p):
    '''elif : ELIF '(' expression ')' '{' segment '}' elif
        |'''

    if len(p) > 2:
        p[0] = ('elif', p[3],p[6],p[8])

def p_else(p): 
    '''else : ELSE '{' segment '}' 
        | ''' 
    if(len(p) > 1):
        p[0] = p[3]

def p_for(p):
    '''for : FOR '(' declareWithVal ';' expression ';' assign ')' '{' segment '}' '''
    p[0] = ('forloop', p[3], p[5], p[7],p[10])

def p_while(p):
    '''while : WHILE '(' expression ')' '{' segment '}' '''
    p[0] = ('whileloop', p[3], p[6])

def p_error(p):
    if p:
        print(p)
        print("Syntax error at line '%s' character '%s'" % (p.lineno,p.lexpos))
    else:
        print("Syntax error at EOF")

parser = yacc.yacc()

inputFile = open('input.txt', 'r')
lines = inputFile.read()

yacc.parse(lines)

code = []

for line in abstractTree:
    print(line)


def complete_declare(entry):
    line = "" + entry[2] + "="
    if((entry[1] == 'int') | (entry[1] == 'float')):
        if(type(entry[3]) != tuple):
            line += str(float(entry[3]))
        else:  
            line += str(expression(entry[3]))
    elif(entry[1] == 'string'):
        if(type(entry[3]) != tuple):
            line += entry[3]
        else:  
            line += str(expression(entry[3]))
    code.append(line)

def declare(entry):
    line = "" + entry[1] + " " + entry[2]
    code.append(line)

def assign(entry):
    line = " " + entry[1] + "="
    if(type(entry[2]) != tuple):
        line += str(entry[2])
    else:
        line += str(expression(entry[2]))
    code.append(line)

def expression(entry):
    line = ""
    lineResult=""


    if(entry[0] == 'comparison'):
        arg1 = ""
        arg2 = ""
       
        if(type(entry[1]) != tuple):
            line += str(entry[1])
            arg1 = (entry[1])
        else:
            line += str(expression(entry[1]))
            arg1 = (expression(entry[1]))

        line += " " + entry[2] + " "

        if(type(entry[3]) != tuple):
            line += str(entry[3])
            arg2 = (entry[3])
        else:
            line += str(expression(entry[3]))
            arg2 = (expression(entry[3]))

        res = ""

        if(entry[2] == "=="):
            res = arg1 == arg2 
        if(entry[2] == "!="):
            res = arg1 != arg2 
        elif(entry[2] == "<"):
            res = arg1 < arg2 
        elif(entry[2] == ">"):
            res = arg1 > arg2 
        elif(entry[2] == "<="):
            res = arg1 <= arg2 
        elif(entry[2] == ">="):
            res = arg1 >= arg2 
        elif(entry[2] == "and"):
            res = arg1 & arg2 
        elif(entry[2] == "or"):
            res = arg1 | arg2
        
        code.append(line)

        return res
    else:
        if(type(entry[1]) != tuple):
            if((type(entry[1]) == float) | (type(entry[1]) == int)):
                lineResult = entry[1]
            line += str(entry[1])
        else:
                lineResult = expression(entry[1])
                line += str(lineResult)

        if(entry[2] == '+'):
                line += " + "
        elif(entry[2] == '-'):
            line += " - "
        elif(entry[2] == '*'):
            line += " * "
        elif(entry[2] == '/'):
            line += " / "
        elif(entry[2] == '^'):
            line += " ^ "

        if(type(entry[3]) != tuple):
            notString = (type(entry[3]) != str)

            if((entry[2] == '+') & notString):
                if(type(lineResult != str)):
                    lineResult += entry[3]
                else:
                    lineResult = entry[3]

            if((entry[2] == '-') & notString):
                if(type(lineResult != str)):
                    lineResult -= entry[3]
                else:
                    lineResult = entry[3]

            if((entry[2] == '*') & notString):
                if(type(lineResult != str)):
                    lineResult *= entry[3]
                else:
                    lineResult = entry[3]

            if((entry[2] == '/') & notString):
                if(type(lineResult != str)):
                    lineResult /= entry[3]
                else:
                    lineResult = entry[3]

            if((entry[2] == '^') & notString):
                if(type(lineResult != str)):
                    lineResult **= entry[3]
                else:
                    lineResult = entry[3]

            line += str(entry[3])
        elif(type(entry[3]) == str):
            line += entry[3]
            code.append(line)
            return line
        else:
            result = 0
            if(entry[2] == '+'):
                lineResult += expression(entry[3])
                result = expression(entry[3])
            elif(entry[2] == '-'):
                lineResult -= expression(entry[3])
                result = expression(entry[3])
            elif(entry[2] == '*'):
                lineResult *= expression(entry[3])
                result = expression(entry[3])
            elif(entry[2] == '/'):
                lineResult /= expression(entry[3])
                result = expression(entry[3])
            elif(entry[2] == '^'):
                lineResult **= expression(entry[3])
                result = expression(entry[3])

            line += str(result)

        code.append(line)

        return lineResult

def print_func(entry):
    line = ""
    if(type(entry[1]) != tuple):
        line = entry[0] + " = " + str(entry[1])
    else:
        line = entry[0] + " = " + str(expression(entry[1]))
    code.append(line)

def if_func(entry):
    code.append("if")
    expression(entry[1])

    for segment in entry[2]:
        run(segment)

    if(entry[3]):
        elif_func(entry[3])

    if(entry[4]):
        else_func(entry[4])
    code.append("endif")

def elif_func(entry):
    code.append("elif")
    expression(entry[1])
    for segment in entry[2]:
        run(segment)

def else_func(entry):
    code.append("else")
    for segment in entry[2]:
        run(segment)

def for_func(entry):

    code.append("for")
    run(entry[1])
    run(entry[2])
    run(entry[3])

    for segment in entry[4]:
        run(segment)

    code.append("endfor")

def while_func(entry):
    code.append("while")
    expression(entry[1])
    
    for segment in entry[2]:
        run(segment)
    code.append("endwhile")

def run(entry):
    if(entry[0] == 'complete_declaration'):
        complete_declare(entry)
    elif(entry[0] == 'declaration'):
        declare(entry)
    elif(entry[0] == 'assign'):
        assign(entry)
    elif(entry[0] == 'operation'):
        expression(entry)
    elif(entry[0] == 'comparison'):
        expression(entry)
    elif(entry[0] == 'print'):
       print_func(entry)
    elif(entry[0] == 'if'):
        if_func(entry)
    elif(entry[0] == 'forloop'):
        for_func(entry)
    elif(entry[0] == 'whileloop'):
        while_func(entry)

for entry in abstractTree:
    run(entry)

print("==========================================")

for line in code:
    print(line)