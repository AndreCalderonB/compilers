
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = "left+-left*/rightUMINUSAND BOOLEAN ELIF ELSE EQUAL FLOATDEC FNUMBER FOR GOEQUAL GT IF INTDEC INUMBER LOEQUAL LT NAME NOTEQUAL OR PRINT STRING STRING_V WHILEs : segment segment : while segment\n            | for segment\n            | conditional segment\n            | statement ';' segment\n            | declaration ';' segment \n            | declaration : declareWithVal\n                |   declareVar\n                |   assigndeclareWithVal : dataType NAME '=' expression declareVar : dataType NAMEassign : NAME '=' expression dataType : INTDEC\n                | FLOATDEC\n                | BOOLEAN\n                | STRINGstatement : PRINT '(' expression ')' expression : expression '+' expression\n                  | expression '-' expression\n                  | expression '*' expression\n                  | expression '/' expression\n                  | expression '^' expression\n                  expression : expression LT expression\n                  | expression GT expression\n                  | expression EQUAL expression\n                  | expression NOTEQUAL expression\n                  | expression GOEQUAL expression\n                  | expression LOEQUAL expression\n                  | expression AND expression\n                  | expression OR expressionexpression : '-' expression %prec UMINUSexpression : '(' expression ')'expression : INUMBER\n                | FNUMBER\n                | BOOLEAN\n                | STRING_V  expression : NAMEconditional : IF '(' expression ')' '{' segment '}' elif else elif : ELIF '(' expression ')' '{' segment '}' elif\n        |else : ELSE '{' segment '}' \n        | for : FOR '(' declareWithVal ';' expression ';' assign ')' '{' segment '}' while : WHILE '(' expression ')' '{' segment '}' "
    
_lr_action_items = {'$end':([0,1,2,3,4,5,21,22,23,24,25,32,33,89,91,93,96,102,105,108,109,],[-7,0,-1,-7,-7,-7,-2,-3,-4,-7,-7,-5,-6,-45,-41,-43,-39,-44,-42,-41,-40,]),'WHILE':([0,3,4,5,24,25,70,85,89,91,93,95,96,100,102,105,106,108,109,],[8,8,8,8,8,8,8,8,-45,-41,-43,8,-39,8,-44,-42,8,-41,-40,]),'FOR':([0,3,4,5,24,25,70,85,89,91,93,95,96,100,102,105,106,108,109,],[9,9,9,9,9,9,9,9,-45,-41,-43,9,-39,9,-44,-42,9,-41,-40,]),'IF':([0,3,4,5,24,25,70,85,89,91,93,95,96,100,102,105,106,108,109,],[12,12,12,12,12,12,12,12,-45,-41,-43,12,-39,12,-44,-42,12,-41,-40,]),'PRINT':([0,3,4,5,24,25,70,85,89,91,93,95,96,100,102,105,106,108,109,],[13,13,13,13,13,13,13,13,-45,-41,-43,13,-39,13,-44,-42,13,-41,-40,]),'NAME':([0,3,4,5,15,17,18,19,20,24,25,26,28,29,31,34,36,43,46,50,51,52,53,54,55,56,57,58,59,60,61,62,64,70,85,87,89,91,93,95,96,98,100,102,105,106,108,109,],[16,16,16,16,30,-14,-15,-16,-17,16,16,41,41,41,41,41,41,65,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,16,16,16,-45,-41,-43,16,-39,41,16,-44,-42,16,-41,-40,]),'INTDEC':([0,3,4,5,24,25,27,70,85,89,91,93,95,96,100,102,105,106,108,109,],[17,17,17,17,17,17,17,17,17,-45,-41,-43,17,-39,17,-44,-42,17,-41,-40,]),'FLOATDEC':([0,3,4,5,24,25,27,70,85,89,91,93,95,96,100,102,105,106,108,109,],[18,18,18,18,18,18,18,18,18,-45,-41,-43,18,-39,18,-44,-42,18,-41,-40,]),'BOOLEAN':([0,3,4,5,24,25,26,27,28,29,31,34,36,46,50,51,52,53,54,55,56,57,58,59,60,61,62,64,70,85,89,91,93,95,96,98,100,102,105,106,108,109,],[19,19,19,19,19,19,39,19,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,19,19,-45,-41,-43,19,-39,39,19,-44,-42,19,-41,-40,]),'STRING':([0,3,4,5,24,25,27,70,85,89,91,93,95,96,100,102,105,106,108,109,],[20,20,20,20,20,20,20,20,20,-45,-41,-43,20,-39,20,-44,-42,20,-41,-40,]),'}':([3,4,5,21,22,23,24,25,32,33,70,85,86,88,89,91,93,95,96,99,100,102,103,105,106,107,108,109,],[-7,-7,-7,-2,-3,-4,-7,-7,-5,-6,-7,-7,89,91,-45,-41,-43,-7,-39,102,-7,-44,105,-42,-7,108,-41,-40,]),';':([6,7,10,11,14,30,37,38,39,40,41,42,47,63,67,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,],[24,25,-8,-10,-9,-12,-34,-35,-36,-37,-38,64,-13,-32,-18,-11,-33,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-30,-31,87,]),'(':([8,9,12,13,26,28,29,31,34,36,46,50,51,52,53,54,55,56,57,58,59,60,61,62,64,94,98,],[26,27,28,29,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,98,34,]),'=':([16,30,65,],[31,46,46,]),'-':([26,28,29,31,34,35,36,37,38,39,40,41,44,45,46,47,48,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,98,101,],[36,36,36,36,36,51,36,-34,-35,-36,-37,-38,51,51,36,51,51,36,36,36,36,36,36,36,36,36,36,36,36,36,-32,36,51,-33,-19,-20,-21,-22,51,51,51,51,51,51,51,51,51,51,36,51,]),'INUMBER':([26,28,29,31,34,36,46,50,51,52,53,54,55,56,57,58,59,60,61,62,64,98,],[37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,]),'FNUMBER':([26,28,29,31,34,36,46,50,51,52,53,54,55,56,57,58,59,60,61,62,64,98,],[38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,]),'STRING_V':([26,28,29,31,34,36,46,50,51,52,53,54,55,56,57,58,59,60,61,62,64,98,],[40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,]),')':([35,37,38,39,40,41,44,45,47,48,63,69,71,72,73,74,75,76,77,78,79,80,81,82,83,90,101,],[49,-34,-35,-36,-37,-38,66,67,-13,69,-32,-33,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-30,-31,92,104,]),'+':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[50,-34,-35,-36,-37,-38,50,50,50,50,-32,50,-33,-19,-20,-21,-22,50,50,50,50,50,50,50,50,50,50,50,]),'*':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[52,-34,-35,-36,-37,-38,52,52,52,52,-32,52,-33,52,52,-21,-22,52,52,52,52,52,52,52,52,52,52,52,]),'/':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[53,-34,-35,-36,-37,-38,53,53,53,53,-32,53,-33,53,53,-21,-22,53,53,53,53,53,53,53,53,53,53,53,]),'^':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[54,-34,-35,-36,-37,-38,54,54,54,54,-32,54,-33,-19,-20,-21,-22,54,54,54,54,54,54,54,54,54,54,54,]),'LT':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[55,-34,-35,-36,-37,-38,55,55,55,55,-32,55,-33,-19,-20,-21,-22,55,55,55,55,55,55,55,55,55,55,55,]),'GT':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[56,-34,-35,-36,-37,-38,56,56,56,56,-32,56,-33,-19,-20,-21,-22,56,56,56,56,56,56,56,56,56,56,56,]),'EQUAL':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[57,-34,-35,-36,-37,-38,57,57,57,57,-32,57,-33,-19,-20,-21,-22,57,57,57,57,57,57,57,57,57,57,57,]),'NOTEQUAL':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[58,-34,-35,-36,-37,-38,58,58,58,58,-32,58,-33,-19,-20,-21,-22,58,58,58,58,58,58,58,58,58,58,58,]),'GOEQUAL':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[59,-34,-35,-36,-37,-38,59,59,59,59,-32,59,-33,-19,-20,-21,-22,59,59,59,59,59,59,59,59,59,59,59,]),'LOEQUAL':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[60,-34,-35,-36,-37,-38,60,60,60,60,-32,60,-33,-19,-20,-21,-22,60,60,60,60,60,60,60,60,60,60,60,]),'AND':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[61,-34,-35,-36,-37,-38,61,61,61,61,-32,61,-33,-19,-20,-21,-22,61,61,61,61,61,61,61,61,61,61,61,]),'OR':([35,37,38,39,40,41,44,45,47,48,63,68,69,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,],[62,-34,-35,-36,-37,-38,62,62,62,62,-32,62,-33,-19,-20,-21,-22,62,62,62,62,62,62,62,62,62,62,62,]),'{':([49,66,92,97,104,],[70,85,95,100,106,]),'ELIF':([91,108,],[94,94,]),'ELSE':([91,93,108,109,],[-41,97,-41,-40,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'s':([0,],[1,]),'segment':([0,3,4,5,24,25,70,85,95,100,106,],[2,21,22,23,32,33,86,88,99,103,107,]),'while':([0,3,4,5,24,25,70,85,95,100,106,],[3,3,3,3,3,3,3,3,3,3,3,]),'for':([0,3,4,5,24,25,70,85,95,100,106,],[4,4,4,4,4,4,4,4,4,4,4,]),'conditional':([0,3,4,5,24,25,70,85,95,100,106,],[5,5,5,5,5,5,5,5,5,5,5,]),'statement':([0,3,4,5,24,25,70,85,95,100,106,],[6,6,6,6,6,6,6,6,6,6,6,]),'declaration':([0,3,4,5,24,25,70,85,95,100,106,],[7,7,7,7,7,7,7,7,7,7,7,]),'declareWithVal':([0,3,4,5,24,25,27,70,85,95,100,106,],[10,10,10,10,10,10,42,10,10,10,10,10,]),'assign':([0,3,4,5,24,25,70,85,87,95,100,106,],[11,11,11,11,11,11,11,11,90,11,11,11,]),'declareVar':([0,3,4,5,24,25,70,85,95,100,106,],[14,14,14,14,14,14,14,14,14,14,14,]),'dataType':([0,3,4,5,24,25,27,70,85,95,100,106,],[15,15,15,15,15,15,43,15,15,15,15,15,]),'expression':([26,28,29,31,34,36,46,50,51,52,53,54,55,56,57,58,59,60,61,62,64,98,],[35,44,45,47,48,63,68,71,72,73,74,75,76,77,78,79,80,81,82,83,84,101,]),'elif':([91,108,],[93,109,]),'else':([93,],[96,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> s","S'",1,None,None,None),
  ('s -> segment','s',1,'p_start','main.py',89),
  ('segment -> while segment','segment',2,'p_segment','main.py',94),
  ('segment -> for segment','segment',2,'p_segment','main.py',95),
  ('segment -> conditional segment','segment',2,'p_segment','main.py',96),
  ('segment -> statement ; segment','segment',3,'p_segment','main.py',97),
  ('segment -> declaration ; segment','segment',3,'p_segment','main.py',98),
  ('segment -> <empty>','segment',0,'p_segment','main.py',99),
  ('declaration -> declareWithVal','declaration',1,'p_declaration','main.py',108),
  ('declaration -> declareVar','declaration',1,'p_declaration','main.py',109),
  ('declaration -> assign','declaration',1,'p_declaration','main.py',110),
  ('declareWithVal -> dataType NAME = expression','declareWithVal',4,'p_declare','main.py',114),
  ('declareVar -> dataType NAME','declareVar',2,'p_declarevar','main.py',119),
  ('assign -> NAME = expression','assign',3,'p_assign','main.py',124),
  ('dataType -> INTDEC','dataType',1,'p_dataType','main.py',129),
  ('dataType -> FLOATDEC','dataType',1,'p_dataType','main.py',130),
  ('dataType -> BOOLEAN','dataType',1,'p_dataType','main.py',131),
  ('dataType -> STRING','dataType',1,'p_dataType','main.py',132),
  ('statement -> PRINT ( expression )','statement',4,'p_statement_print','main.py',136),
  ('expression -> expression + expression','expression',3,'p_expression_binop_comparison','main.py',140),
  ('expression -> expression - expression','expression',3,'p_expression_binop_comparison','main.py',141),
  ('expression -> expression * expression','expression',3,'p_expression_binop_comparison','main.py',142),
  ('expression -> expression / expression','expression',3,'p_expression_binop_comparison','main.py',143),
  ('expression -> expression ^ expression','expression',3,'p_expression_binop_comparison','main.py',144),
  ('expression -> expression LT expression','expression',3,'p_logic_expressions','main.py',158),
  ('expression -> expression GT expression','expression',3,'p_logic_expressions','main.py',159),
  ('expression -> expression EQUAL expression','expression',3,'p_logic_expressions','main.py',160),
  ('expression -> expression NOTEQUAL expression','expression',3,'p_logic_expressions','main.py',161),
  ('expression -> expression GOEQUAL expression','expression',3,'p_logic_expressions','main.py',162),
  ('expression -> expression LOEQUAL expression','expression',3,'p_logic_expressions','main.py',163),
  ('expression -> expression AND expression','expression',3,'p_logic_expressions','main.py',164),
  ('expression -> expression OR expression','expression',3,'p_logic_expressions','main.py',165),
  ('expression -> - expression','expression',2,'p_expression_uminus','main.py',184),
  ('expression -> ( expression )','expression',3,'p_expression_group','main.py',188),
  ('expression -> INUMBER','expression',1,'p_expression_val','main.py',192),
  ('expression -> FNUMBER','expression',1,'p_expression_val','main.py',193),
  ('expression -> BOOLEAN','expression',1,'p_expression_val','main.py',194),
  ('expression -> STRING_V','expression',1,'p_expression_val','main.py',195),
  ('expression -> NAME','expression',1,'p_expression_name','main.py',199),
  ('conditional -> IF ( expression ) { segment } elif else','conditional',9,'p_if','main.py',203),
  ('elif -> ELIF ( expression ) { segment } elif','elif',8,'p_elif','main.py',207),
  ('elif -> <empty>','elif',0,'p_elif','main.py',208),
  ('else -> ELSE { segment }','else',4,'p_else','main.py',214),
  ('else -> <empty>','else',0,'p_else','main.py',215),
  ('for -> FOR ( declareWithVal ; expression ; assign ) { segment }','for',11,'p_for','main.py',220),
  ('while -> WHILE ( expression ) { segment }','while',7,'p_while','main.py',224),
]
