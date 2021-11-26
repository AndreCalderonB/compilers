import ply.yacc as yacc
import ply.lex as lex

literals = ['=', '+', '-', '*', '/', '(', ')','^','{','}','<','>',';']

reserved = {
    'int': 'INTDEC',
    'float': 'FLOATDEC',
    'print': 'PRINT',
    # === Bools and logical operations
    'boolean': 'BOOLEAN',
    'string': 'STRING'
    'true': 'TRUE',
    'false': 'FALSE',
    'and': 'AND',
    'or': 'OR',
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE'
}

tokens = [
            #                   Equals, !Equals, Greater or Equal to, Less or Equal to
             'INUMBER', 'FNUMBER', 'NAME', 'EQUAL', 'NOTEQUAL', 'GOEQUAL', 'LOEQUAL'
         ] + list(reserved.values())

# Tokens

t_EQUAL = r'=='
t_NOTEQUAL = r'!='
t_GOEQUAL = r'>='
t_LOEQUAL = r'<='

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


t_ignore = " \t"


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
    ('left', '*', '/'),
    ('left', '+', '-'),
    # Note to self: UMINUS -> Multiplying something by -1
    ('right', 'UMINUS'),
    
)

# dictionary of names
names = {}
abstractTree = []


def p_start(p):
    '''s : segment
        | segment s'''
    p[0] = p[1]

def p_segment(p):
    '''segment : conditional
            | statement'''
    p[0] = p[1]

##  Boolean Declaration 
def p_statement_declare_bool(p):
    '''statement : BOOLEAN NAME is_assign'''
    if(type(p[3]) == bool):
        names[p[2]] = {"type": "BOOLEAN", "value": p[3]}
    else: 
        print("No se le puede asignar ese valor a un booleano")

def p_statement_declare_str(p):
    '''statement: STRING NAME is_assign'''
    if(type(p[3]) == tuple):
        names[p[2]] = {"type": "STRING", "value": p[3]}

def p_statement_declare_int(p):
    '''statement : INTDEC NAME is_assign
    '''
    if (type(p[3]) == int):
        names[p[2]] = {"type": "INT", "value": p[3]}
    else: 
        print("No se le puede asignar ese valor a un int")

def p_statement_declare_float(p):
    'statement : FLOATDEC NAME is_assign'
    names[p[2]] = {"type": "FLOAT", "value": p[3]}


def p_is_assign(p):
    '''is_assign : "=" expression
                | '''
    p[0] = 0
    if (len(p) > 2):
        p[0] = p[2]

def p_statement_print(p):
    '''statement : PRINT '(' expression ')' '''
    print(p[3])

def p_statement_assign(p):
    'statement : NAME "=" expression'
    if p[1] not in names:
        print("You must declare a variable before using it")
    else:
        names[p[1]]["value"] = p[3]

def p_expression_binop_comparison(p):
    '''expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression
                  | expression '^' expression
                  | expression '<' expression
                  | expression '>' expression
                  | expression EQUAL expression
                  | expression NOTEQUAL expression
                  | expression GOEQUAL expression
                  | expression LOEQUAL expression
                  | expression AND expression
                  | expression OR expression
                  '''
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]
    elif p[2] == '*':
        p[0] = p[1] * p[3]
    elif p[2] == '/':
        p[0] = p[1] / p[3]
    elif p[2] == '<':
        p[0] = p[1] < p[3]
    elif p[2] == '>':
        p[0] = p[1] > p[3]
    elif p[2] == '<=':
        p[0] = (p[1] <= p[3])
    elif p[2] == '>=':
        p[0] = (p[1] >= p[3])
    elif p[2] == '==':
        p[0] = (p[1] == p[3])
    elif p[2] == '!=':
        p[0] = (p[1] != p[3])
    elif p[2] == 'and':
        p[0] = (p[1] & p[3])
    elif p[2] == 'or':
        p[0] = (p[1] | p[3])   

def p_logic_expressions(p):
    '''logic_expression : expression '<' expression
                  | expression '>' expression
                  | expression EQUAL expression
                  | expression NOTEQUAL expression
                  | expression GOEQUAL expression
                  | expression LOEQUAL expression
                  | expression AND expression
                  | expression OR expression'''
    if p[2] == '<':
        p[0] = p[1] < p[3]
    elif p[2] == '>':
        p[0] = p[1] > p[3]
    elif p[2] == '==':
        p[0] = (p[1] == p[3])
    elif p[2] == '<=':
        p[0] = (p[1] <= p[3])
    elif p[2] == '>=':
        p[0] = (p[1] >= p[3])
    elif p[2] == '!=':
        p[0] = (p[1] != p[3])
    elif p[2] == 'and':
        p[0] = (p[1] & p[3])
    elif p[2] == 'or':
        p[0] = (p[1] | p[3])

def p_expression_uminus(p):
    "expression : '-' expression %prec UMINUS"
    p[0] = -p[2]

def p_expression_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

def p_expression_val(p):
    '''expression : INUMBER
                | FNUMBER
                | BOOLEAN '''
    p[0] = p[1]
def p_expression_string(p):
    '''expression : STRING'''

#========== Conditional Statements ================
def p_if(p):
    '''conditional : IF '(' logic_expression ')' '{' s '}' elif else '''
    if(p[3] == True):
        p[0] = p[6]


def p_elif(p):
    '''elif : ELIF '(' logic_expression ')' '{' s '}' else
        | '''

def p_else(p): 
    '''else : ELSE '{' s '}' 
        | ''' 
    if(len(p) > 1):
        p[0] = p[3]
    
#========================================

def p_expression_name(p):
    "expression : NAME"
    try:
        p[0] = names[p[1]]["value"]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0

def p_error(p):
    if p:
        print(p)
        print("Syntax error at line '%s' character '%s'" % (p.lineno,p.lexpos))
    else:
        print("Syntax error at EOF")


parser = yacc.yacc()

inputFile = open('input.txt', 'r')
lines = inputFile.readlines()
for line in lines:
    print(line)
    yacc.parse(line)
