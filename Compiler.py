#!/usr/bin/env python3

import sys
import string
from enum import Enum
from collections import OrderedDict
import argparse
import time

__version__ = "1.0"

#-------------------------------------------#
#-------------------------------------------#
#------------Object Definitions-------------#
#-------------------------------------------#
#-------------------------------------------#

class Token:
	# Constructor
	# tokenValue: the token itself
	# tokenType: the type of the token
	# lineNo: the line of the token in the EEL file
	# charNo: the position of the character in the EEL file
	def __init__(self, tokenValue, tokenType, lineNo, charNo):
		self.tokenValue = tokenValue
		self.tokenType = tokenType
		self.lineNo = lineNo
		self.charNo = charNo


class Quad:
	# Constructor
	# label: quad's label
	# name: quad's name
	# op1,op2,op3: quad's components
	def __init__(self, label, name, op1, op2, op3):
		self.label = label
		self.name = name
		self.op1 = op1
		self.op2 = op2
		self.op3 = op3

	def __str__(self):
		return str(self.label) + ": " + "(" + str(self.name) + "," + str(self.op1) + \
		 	   "," + str(self.op2) + "," + str(self.op3) + ")"

class Scope:
	# Constructor
	# nestingLevel: scope's nesting level
	# entities: list of scope's entities
	# previousScope: pointer to the immediate above Scope object
	# tempFrameLength: holds the frame length value of it's enclosed Scope
	def __init__(self, nestingLevel, previousScope):
		self.nestingLevel = nestingLevel
		self.entities = list()
		self.previousScope = previousScope
		self.tempFrameLength = 12 # Default value

	def __str__(self):
		return str(self.nestingLevel)

	def addEntity(self, newEntity):
		self.entities.append(newEntity)

	def setTempFrameLength(self):
		self.tempFrameLength += 4

	def getTempFrameLength(self):
		return self.tempFrameLength

	
class Entity:
	# Constructor
	# id: entity's identifier returned from lex
	# type: entity's type
	def __init__(self, id, typeOf):
		self.id = id
		self.typeOf = typeOf

	def __str__(self):
		return str(symbols.get(self.typeOf)) + ": " + self.id

	def toString(self):
		return str(symbolsForFile.get(self.typeOf)) + ": " + self.id

class Variable(Entity):
	# Constructor
	# offset: variable's offset
	def __init__(self, id, offset):
		super().__init__(id, SymbolType.VARIABLE)
		self.offset = offset

	def __str__(self):
		return super().__str__() + " || Offset = " + str(self.offset)

	def toString(self):
		return super().toString() + " || Offset = " + str(self.offset)

class Function(Entity):
	# Constructor
	# type: function or procedure
	# startQuad: function's first label
	# frameLength: function's frame length
	def __init__(self, id, funcType, startQuad):
		super().__init__(id, SymbolType.FUNCTION)
		self.funcType = funcType
		self.startQuad = startQuad
		self.args = list()
		self.frameLength = -1

	def __str__(self):
		return super().__str__() + " || Type = " + str(self.funcType) + " || Start Quad = " +\
			   str(self.startQuad) + " || Frame Length = " + str(self.frameLength)

	def toString(self):
		return super().toString() + " || Type = " + str(self.funcType) + " || Start Quad = " +\
			   str(self.startQuad) + " || Frame Length = " + str(self.frameLength)

	def setStartQuad(self, startQuad):
		self.startQuad = startQuad

	def setFrameLegth(self, frameLength):
		self.frameLength = frameLength

	def addArg(self, newArg):
		self.args.append(newArg)

class Parameter(Entity):
	# Constructor
	# offset: parameter's offset
	# parMode: cv = in || cr = inout
	def __init__(self, id, offset, parMode):
		super().__init__(id, SymbolType.PARAMETER)
		self.offset = offset
		self.parMode = parMode

	def __str__(self):
		return super().__str__() + " || Offset = " + str(self.offset) + " || parMode = " +\
			   self.parMode

	def toString(self):
		return super().toString() + " || Offset = " + str(self.offset) + " || parMode = " +\
			   self.parMode
		
class TempVariable(Entity):
	# Constructor
	# offset: tempVar's offset
	def __init__(self, id, offset):
		super().__init__(id, SymbolType.TEMPVARIABLE)
		self.offset = offset

	def __str__(self):
		return super().__str__() + " || Offset = " + str(self.offset)

	def toString(self):
		return super().toString() + " || Offset = " + str(self.offset)

class Argument():
	# Constructor
	# parMode: cv = call by value || cr = call by reference
	def __init__(self, parMode):
		self.parMode = parMode

	def __str__(self):
		return "ParMode = " + self.parMode

class SymbolType(Enum):
	FUNCTION	= 0
	VARIABLE	= 1
	PARAMETER 	= 2
	TEMPVARIABLE= 3

class TokenType(Enum):
	# Lexical analyzer products
	EOF 		= 0
	IDENTIFIER	= 1
	NUMBER		= 2
	ADDTK		= 3
	MINUSTK		= 4
	MULTITK		= 5
	DIVTK 		= 6
	LEQTK		= 7
	NEQTK		= 8
	LESSTK		= 9
	GREQTK		= 10
	GREATK		= 11
	EQTK		= 12
	ASSIGNTK 	= 13
	COLONTK		= 14
	SEMICOLONTK = 15
	COMMATK		= 16
	LPARENTK 	= 17
	RPARENTK 	= 18
	LBRACKETK	= 19
	RBRACKETK 	= 20
	PROGRAM		= 21
	ENDPROGRAM	= 22
	DECLARE		= 23
	ENDDECLARE	= 24
	IF			= 25
	THEN 		= 26
	ELSE		= 27
	ENDIF		= 28
	WHILE		= 29
	ENDWHILE	= 30
	REPEAT		= 31
	ENDREPEAT 	= 32
	EXIT		= 33
	SWITCH		= 34
	CASE 		= 35
	ENDSWITCH 	= 36
	FORCASE 	= 37
	WHEN		= 38
	ENDFORCASE 	= 39
	PROCEDURE 	= 40
	ENDPROCEDURE= 41
	FUNCTION 	= 42
	ENDFUNCTION = 43
	CALL 		= 44
	RETURN 		= 45
	IN 			= 46
	INOUT 		= 47
	AND 		= 48
	OR			= 49
	NOT 		= 50
	TRUE 		= 51
	FALSE 		= 52
	INPUT 		= 53
	PRINT 		= 54

class color:
	PURPLE = '\033[95m'
	DARKCYAN = '\033[36m'
	BLUE = '\033[94m'
	GREEN = '\033[92m'
	ORANGE = '\033[38;5;208m'
	YELLOW = '\033[93m'
	CYAN = '\033[96m'
	RED = '\033[91m'
	BOLD = '\033[1m'
	UNDERLINE = '\033[4m'
	END = '\033[0m'


#-------------------------------------------#
#-------------------------------------------#
#-------------Global Variables--------------#
#-------------------------------------------#
#-------------------------------------------#

token = Token (None,None,None,None)
file_size = 0 # the input file's size
totalLineNo = 1
totalCharNo = 0
quadList = list() # the list with all the intermediate code
labelID = 0 # the ID which is used in the creation of labels for the quads
tempVars = {} # holds all the temp vars
tempVarsID = 0 # the ID which is used in the creation of temp vars
main_name = "" # Main program's name
haltLabel = -1
repeatNests = 0 # how many repeat nests are we in currently
exitStats = OrderedDict() # Used to backpatch empty 'jump' quads generated by repeat statements
exitQuads = list() # The quads that are to be backpatched
scopesList = list() # Holds all the scopes, used in the symbol table
returnDict = {} # Prevents the use of return statements outside of a function and the omission of it
mainframe = 0 # Main program's framelength :D
symtablefile = "" # Symbol table's file output
asmfile = "" # Final code's file output
parametersEncountered = 0 # Used in checking if the passing arguments match the parameters number
parametersDict = {} # Holds the function's parameters, which is currently translated to assembly
promptDict = {} # Holds all the variables who are about to be asked for an input or output.
				# Ignores temp variables

symbols = {
	SymbolType.VARIABLE		: color.BLUE+"Variable"+color.END,
	SymbolType.FUNCTION		: color.YELLOW+"Function"+color.END,
	SymbolType.PARAMETER	: color.ORANGE+"Parameter"+color.END,
	SymbolType.TEMPVARIABLE	: color.RED+"Temp Variable"+color.END
}

symbolsForFile = {
	SymbolType.VARIABLE		: "Variable",
	SymbolType.FUNCTION		: "Function",
	SymbolType.PARAMETER	: "Parameter",
	SymbolType.TEMPVARIABLE	: "Temp Variable"
}

# Reserved tokens
tokens = {
	""				: TokenType.EOF,
	"+"				: TokenType.ADDTK,
	"-"				: TokenType.MINUSTK,
	"*"				: TokenType.MULTITK,
	"/"				: TokenType.DIVTK,
	"<="			: TokenType.LEQTK,
	"<>"			: TokenType.NEQTK,
	"<"				: TokenType.LESSTK,
	">="			: TokenType.GREQTK,
	">"				: TokenType.GREATK,
	"="				: TokenType.EQTK,
	":="			: TokenType.ASSIGNTK,
	":"				: TokenType.COLONTK,
	";"				: TokenType.SEMICOLONTK,
	","				: TokenType.COMMATK,
	"("				: TokenType.LPARENTK,
	")"				: TokenType.RPARENTK,
	"["				: TokenType.LBRACKETK,
	"]"				: TokenType.RBRACKETK,
	"program"		: TokenType.PROGRAM,
	"endprogram"	: TokenType.ENDPROGRAM,
	"declare"		: TokenType.DECLARE,
	"enddeclare"	: TokenType.ENDDECLARE,
	"if"			: TokenType.IF,
	"then"			: TokenType.THEN,
	"else"			: TokenType.ELSE,
	"endif"			: TokenType.ENDIF,
	"while"			: TokenType.WHILE,
	"endwhile"		: TokenType.ENDWHILE,
	"repeat"		: TokenType.REPEAT,
	"endrepeat"		: TokenType.ENDREPEAT,
	"exit"			: TokenType.EXIT,
	"switch"		: TokenType.SWITCH,
	"case"			: TokenType.CASE,
	"endswitch"		: TokenType.ENDSWITCH,
	"forcase"		: TokenType.FORCASE,
	"when"			: TokenType.WHEN,
	"endforcase"	: TokenType.ENDFORCASE,
	"procedure"		: TokenType.PROCEDURE,
	"endprocedure"	: TokenType.ENDPROCEDURE,
	"function"		: TokenType.FUNCTION,
	"endfunction"	: TokenType.ENDFUNCTION,
	"call"			: TokenType.CALL,
	"return"		: TokenType.RETURN,
	"in"			: TokenType.IN,
	"inout"			: TokenType.INOUT,
	"and"			: TokenType.AND,
	"or"			: TokenType.OR,
	"not"			: TokenType.NOT,
	"true"			: TokenType.TRUE,
	"false"			: TokenType.FALSE,
	"input"			: TokenType.INPUT,
	"print"			: TokenType.PRINT
	}

#-------------------------------------------#
#-------------------------------------------#
#--------------Error Printing---------------#
#-------------------------------------------#
#-------------------------------------------#

def syntaxError(errorString):
	time.sleep(1)
	print('\t'+color.DARKCYAN+color.BOLD+"Syntax Analysis                    "+color.END,end="",flush=True)
	print(color.GREEN+color.BOLD+"["+color.END+color.RED+color.BOLD+"  ERROR   "+color.END+color.GREEN+color.BOLD+"]"+color.END)
	print('\t'+color.DARKCYAN+color.BOLD+"Semantics Analysis                 "+color.END,end="",flush=True)
	print(color.GREEN+color.BOLD+"["+color.END+color.PURPLE+color.BOLD+" STOPPED  "+color.END+color.GREEN+color.BOLD+"]\n"+color.END)
	print(color.BOLD+"Error detected: "+color.END+color.RED+"Syntax Error"+color.END)
	print(color.BOLD+"Line: "+color.END+str(token.lineNo)+',  '+color.BOLD+"Character: "+color.END+str(token.charNo-len(token.tokenValue)))
	print(color.BOLD+"Message:"+color.END+errorString+"'"+str(token.tokenValue)+"'.")

def lexError(errorString, errorToken):
	time.sleep(1)
	print('\t'+color.DARKCYAN+color.BOLD+"Syntax Analysis                    "+color.END,end="",flush=True)
	print(color.GREEN+color.BOLD+"["+color.END+color.RED+color.BOLD+"  ERROR   "+color.END+color.GREEN+color.BOLD+"]\n"+color.END)
	print(color.BOLD+"Error detected: "+color.END+color.CYAN+"Lexical Error"+color.END)
	print(color.BOLD+"Line: "+color.END+str(totalLineNo)+','+color.BOLD+"Character: "+color.END+str(totalCharNo-1))
	print(color.BOLD+"Message:"+color.END+errorString+color.BOLD+" Token:"+color.END+str(errorToken))

def semanticsError(errorString):
	time.sleep(1)
	print('\t'+color.DARKCYAN+color.BOLD+"Syntax Analysis                    "+color.END,end="",flush=True)
	print(color.GREEN+color.BOLD+"["+color.END+color.PURPLE+color.BOLD+" STOPPED  "+color.END+color.GREEN+color.BOLD+"]"+color.END)
	print('\t'+color.DARKCYAN+color.BOLD+"Semantics Analysis                 "+color.END,end="",flush=True)
	print(color.GREEN+color.BOLD+"["+color.END+color.RED+color.BOLD+"  ERROR   "+color.END+color.GREEN+color.BOLD+"]\n"+color.END)
	print(color.BOLD+"Error detected: "+color.END+color.YELLOW+"Semantic Error"+color.END)
	print(color.BOLD+"Scope: "+color.END+str(scopesList[-1]))
	print(color.BOLD+"Message:"+color.END+errorString)


#-------------------------------------------#
#-------------------------------------------#
#-----------Functions used by Lex-----------#
#-------------------------------------------#
#-------------------------------------------#

# Read the next character
def getNextChar():
	global totalCharNo
	totalCharNo += 1 	# Incease the file pointer's position in the current line
	returnChar = infile.read(1)
	return returnChar

#-------------------------------------------#
#-------------------------------------------#
#-------------Lexical Analyzer--------------#
#-------------------------------------------#
#-------------------------------------------#

def getLex():
	global totalLineNo, totalCharNo, token
	tempToken = getNextChar()														# Temporary string variable
	if(tempToken != "" and infile.tell() != file_size):
		# Zero State
		while(tempToken in {" ","\n","\t"}):
			if (tempToken == "\n"):													# Add 1 to the total line number and reset the characters' counter
				totalLineNo += 1
				totalCharNo = 0
			tempToken = getNextChar()

		# First State
		if (tempToken.isalpha()):
			while(True):															# Check subsequent characters
				nextAlnum = getNextChar()
				if (nextAlnum.isalnum() == False):
					break
				tempToken += nextAlnum
				if (len(tempToken) >= 30):											# Accept max 30 letter identifiers
					break

			if (nextAlnum != "" and infile.tell() != file_size):					# Do not backtrack in case a word is at the EOF
				infile.seek(infile.tell()-1)										# Backtracking at EOF creates an infinite loop

			if (tempToken not in tokens.keys()):
				token = Token(tempToken, TokenType.IDENTIFIER, totalLineNo, totalCharNo - len(tempToken) + 1)
				return
			else:
				token = Token(tempToken, tokens[tempToken], totalLineNo, totalCharNo - len(tempToken) + 1)

		# Second State
		elif (tempToken.isdigit()):
			while(True):
				nextDigit = getNextChar()
				if (nextDigit.isdigit()):
					tempToken += nextDigit
				elif (nextDigit.isalpha()):
					tempToken += nextDigit
					errorString = "Identifiers must begin with a letter."
					lexError(errorString, tempToken)
					sys.exit()
				else:																# Found the end of the constant
					if(nextDigit != ""):											# Do not backtrack in case nextDigit is the EOF
						infile.seek(infile.tell()-1)
						totalCharNo -= 1
					break
			if(int(tempToken) > 32767):												# The limits are symmetrical so we check their absolute value
				errorString = "Found a number exceeding the absolute value of 32767."
				lexError(errorString, tempToken)
				sys.exit()
			token = Token(tempToken, TokenType.NUMBER, totalLineNo, totalCharNo - len(tempToken))	# 2 == numbertk
		
		elif (tempToken == "+"):
			token = Token(tempToken, TokenType.ADDTK, totalLineNo, totalCharNo-1)			# 3 == addtk
			return

		elif (tempToken == "-"):
			token = Token(tempToken, TokenType.MINUSTK, totalLineNo, totalCharNo-1)			# 4 == minustk
			return

		elif (tempToken == "*"):
			token = Token(tempToken, TokenType.MULTITK, totalLineNo, totalCharNo-1)			# 5 == multk
			return

		elif (tempToken == "/"):
			comment = getNextChar()
			if (comment == "*"):													# Check for the start of a comment
				comment = getNextChar()
				while(True):														# Parse through characters inside the comment
					if (comment == "\n"):
						totalLineNo += 1
						totalCharNo = 0
					if (comment == "*"):											# Until you find the end of the comment
						comment = getNextChar()
						if(comment == "/"):											# If the end of the comment is found,
							getLex()												# return the next lexical unit
							return
					elif (comment == ""):											# Found the EOF, comment did not close
						errorString = "EOF found. Comment not closed properly."		# ERROR
						lexError(errorString, 'EOF')
						sys.exit()
					else:
						comment = getNextChar()
			elif (comment == "/"):													# Start of a line comment
				while (True):														# Parse through the characters until you find a '\n' character or find the EOF
					comment = getNextChar()
					if (comment == "\n" or comment == ""):							# If you find it, return the next lexical unit
						getLex()
						return
			else:
				if (comment != ""):													# Prevent endless loop in case '/' is at the EOF
					infile.seek(infile.tell()-1)									# Backtrack one character
					totalCharNo -= 1
				token = Token(tempToken, TokenType.DIVTK, totalLineNo, totalCharNo-1)		# 6 == divtk
				return

		# Third State
		elif (tempToken == "<"):
			nextOper = getNextChar()
			if (nextOper == "="):
				token = Token("<=", TokenType.LEQTK, totalLineNo, totalCharNo-2)			# 7 == leqtk
				return
			elif (nextOper == ">"):
				token = Token("<>", TokenType.NEQTK, totalLineNo, totalCharNo-2)			# 8 == neqtk
				return
			else:
				if (nextOper != ""):														# Prevent endless loop in case '<' is at the EOF
					infile.seek(infile.tell()-1)
					totalCharNo -= 1
				token = Token("<", TokenType.LESSTK, totalLineNo, totalCharNo-1)			# 9 == lesstk	
				return

		# Fourth State
		elif (tempToken == ">"):
			nextOper = getNextChar()
			if (nextOper == "="):
				token = Token(">=", TokenType.GREQTK, totalLineNo, totalCharNo-2)			# 10 == greqtk
				return
			else:
				if (nextOper != ""):														# Prevent endless loop in case '>' is at the EOF
					infile.seek(infile.tell()-1)
					totalCharNo -= 1
				token = Token(">", TokenType.GREATK, totalLineNo, totalCharNo-1)			# 11 == greatk
				return
		elif (tempToken == "="):
			token = Token("=", TokenType.EQTK, totalLineNo, totalCharNo-1)					# 12 == eqtk
			return

		# Fifth State
		elif (tempToken == ":"):
			nextAssign = getNextChar()
			if (nextAssign == "="):
				token = Token(":=", TokenType.ASSIGNTK, totalLineNo, totalCharNo-2)			# 13 == assigntk
				return
			if (nextAssign != ""):													# Do not backtrack in case ':' is at EOF
				infile.seek(infile.tell()-1)
				totalCharNo -= 1
			token = Token(":", TokenType.COLONTK, totalLineNo, totalCharNo-1)				# 14 == colontk
			return

		# Sixth State
		elif (tempToken == ";"):
			token = Token(";", TokenType.SEMICOLONTK, totalLineNo, totalCharNo-1)			# 15 == semicolontk
			return
		elif (tempToken == ","):
			token = Token(",", TokenType.COMMATK, totalLineNo, totalCharNo-1)				# 16 == commatk
			return
		elif (tempToken == "("):
			token = Token("(", TokenType.LPARENTK, totalLineNo, totalCharNo-1)				# 17 == lparentk
			return
		elif (tempToken == ")"):
			token = Token(")", TokenType.RPARENTK, totalLineNo, totalCharNo-1)				# 18 == rparentk
			return
		elif (tempToken == "["):
			token = Token("[", TokenType.LBRACKETK, totalLineNo, totalCharNo-1)				# 19 == lbracketk
			return
		elif (tempToken == "]"):
			token = Token("]", TokenType.RBRACKETK, totalLineNo, totalCharNo-1)				# 20 == rbracketk
			return
	# EOF
	else:
		token = Token("", TokenType.EOF, totalLineNo, totalCharNo)
		return


#-------------------------------------------#
#-------------------------------------------#
#--------------Syntax Analyzer--------------#
#-------------------------------------------#
#-------------------------------------------#


def program():
	global main_name
	getLex()
	if(token.tokenType == TokenType.PROGRAM):
		getLex()
		if (token.tokenType == TokenType.IDENTIFIER):
			name = token.tokenValue
			main_name = token.tokenValue
			scopesList.append(Scope(0,None))
			getLex()
			block(name)
			if (token.tokenType != TokenType.ENDPROGRAM):
				errorString = " The keyword 'endprogram' was expected but found "
				syntaxError(errorString)
				sys.exit()
		else:
			errorString = " Program name was expected but found "
			syntaxError(errorString)
			sys.exit()
	else:
		errorString = " The keyword 'program' was expected but found "
		syntaxError(errorString)
		sys.exit()

def block(name):
	global haltLabel
	declarations()
	subprograms()
	startQuad = setFunctionStartQuad(name)
	genQuad("begin_block",name)
	statements()
	if(name == main_name):
		haltLabel = nextQuad()
		genQuad("halt")
	genQuad("end_block",name)
	setFunctionFrameLength(name, scopesList[-1].tempFrameLength)
	#print("Block name: ",name)
	for quad in quadList[startQuad:]:
		quadToAsm(quad, name)
	#printScopesToTerminal()
	printScopesToFile()
	del scopesList[-1]

def declarations():
	if (token.tokenType == TokenType.DECLARE):
		getLex()
		varlist()
		if (token.tokenType != TokenType.ENDDECLARE):
			errorString = " The keyword 'enddeclare' was expected but found "
			syntaxError(errorString)
			sys.exit()
		getLex()
	return

def varlist():
	if (token.tokenType == TokenType.IDENTIFIER):
		addVariable(token.tokenValue)
		getLex()
		while (True):
			if(token.tokenType == TokenType.COMMATK):
				getLex()
				if (token.tokenType != TokenType.IDENTIFIER):
					errorString = " Identifier expected after ',' but found "
					syntaxError(errorString)
					sys.exit()
				addVariable(token.tokenValue)
				getLex()
			else:
				break
	return

def subprograms():
	procorfunc_flag = 1
	while (procorfunc_flag == 1):
		procorfunc_flag = procorfunc()
	return

def procorfunc():
	global returnDict
	if (token.tokenType == TokenType.PROCEDURE):
		createScope()
		getLex()
		if (token.tokenType == TokenType.IDENTIFIER):
			name = token.tokenValue
			addFunction(name, 0)
			getLex()
			procorfuncbody(name)
			if (token.tokenType != TokenType.ENDPROCEDURE):
				errorString = " The keyword 'endprocedure' was expected but found "
				syntaxError(errorString)
				sys.exit()
			getLex()
			return 1
		else:
			errorString = " Name of the procedure was expected but found "
			syntaxError(errorString)
			sys.exit()
	elif (token.tokenType == TokenType.FUNCTION):
		createScope()
		getLex()
		if (token.tokenType == TokenType.IDENTIFIER):
			name = token.tokenValue
			addFunction(name, 1)
			getLex()
			procorfuncbody(name)
			lastScope = len(scopesList)
			if(returnDict.get(lastScope) != True):
				errorString = " Missing 'return' statement in function "
				semanticsError(errorString)
				sys.exit()
			else:
				del returnDict[lastScope]
			if (token.tokenType != TokenType.ENDFUNCTION):
				errorString = " The keyword 'endfunction' was expected but found "
				syntaxError(errorString)
				sys.exit()
			getLex()
			return 1
		else:
			errorString = " Name of the function was expected but found "
			syntaxError(errorString)
			sys.exit()
	return (0)

def procorfuncbody(name):
	formalpars(name)
	block(name)

def formalpars(funcId):
	if (token.tokenType == TokenType.LPARENTK):
		getLex()
		formalparlist(funcId)
		if (token.tokenType != TokenType.RPARENTK):
			errorString = " Expected right parenthesis but found "
			syntaxError(errorString)
			sys.exit()
		getLex()
	else:
		errorString = " Expected left parenthesis but found "
		syntaxError(errorString)
		sys.exit()
	return

def formalparlist(funcId):
	formalparitem(funcId)
	while (token.tokenType == TokenType.COMMATK):
		getLex()
		formalparitem(funcId)
	return

def formalparitem(funcId):
	if (token.tokenType == TokenType.IN):
		getLex()
		if (token.tokenType != TokenType.IDENTIFIER):
			errorString = " Expected an identifier after keyword 'in' but found "
			syntaxError(errorString)
			sys.exit()
		addArgument(funcId, "cv")
		addParameter(token.tokenValue, "cv")
		getLex()
	elif (token.tokenType == TokenType.INOUT):
		getLex()
		if (token.tokenType != TokenType.IDENTIFIER):
			errorString = " Expected an identifier after keyword 'inout' but found "
			syntaxError(errorString)
			sys.exit()
		addArgument(funcId, "cr")
		addParameter(token.tokenValue, "cr")
		getLex()
	return

def statements():
	statement()
	while (token.tokenType == TokenType.SEMICOLONTK):
		getLex()
		statement()
	return

def statement():
	if (token.tokenType == TokenType.IF):
		getLex()
		ifStat()
	elif (token.tokenType == TokenType.WHILE):
		getLex()
		whileStat()
	elif (token.tokenType == TokenType.REPEAT):
		getLex()
		repeatStat()
	elif (token.tokenType == TokenType.EXIT):
		getLex()
		exitStat()
	elif (token.tokenType == TokenType.SWITCH):
		getLex()
		switchStat()
	elif (token.tokenType == TokenType.FORCASE):
		getLex()
		forcaseStat()
	elif (token.tokenType == TokenType.CALL):
		getLex()
		callStat()
	elif (token.tokenType == TokenType.RETURN):
		getLex()
		returnStat()
	elif (token.tokenType == TokenType.INPUT):
		getLex()
		inputStat()
	elif (token.tokenType == TokenType.PRINT):
		getLex()
		printStat()
	elif (token.tokenType == TokenType.IDENTIFIER):
		op3 = token.tokenValue
		getLex()
		op1 = assignmentStat()
		genQuad(":=",op1,"_",op3)
	return

def assignmentStat():
	if (token.tokenType == TokenType.ASSIGNTK):
		getLex()
		op1 = expression()
	else:
		errorString = " Expected an assignment (':=') but found "
		syntaxError(errorString)
		sys.exit()
	return op1

def ifStat():
	(bTrue, bFalse) = condition()
	if (token.tokenType == TokenType.THEN):
		getLex()
		backpatch(bTrue, nextQuad())
		statements()
		skipList = makeList(nextQuad())
		genQuad("jump")
		backpatch(bFalse, nextQuad())
		elsepart()
		backpatch(skipList, nextQuad())
		if (token.tokenType != TokenType.ENDIF):
			errorString = " Expected 'endif' keyword but found "
			syntaxError(errorString)
			sys.exit()
		getLex()
	else:
		errorString = " Expected 'then' keyword but found "
		syntaxError(errorString)
		sys.exit()
	return

def elsepart():
	if (token.tokenType == TokenType.ELSE):
		getLex()
		statements()
	return

def repeatStat():
	global repeatNests
	repeatNests += 1
	exitStats[None] = None
	rQuad = nextQuad()
	statements()
	if (token.tokenType != TokenType.ENDREPEAT):
		errorString = " Expected 'endrepeat' keyword but found "
		syntaxError(errorString)
		sys.exit()
	genQuad("jump","_","_",rQuad)
	if(exitStats):
		for i, extLabel in reversed(list(enumerate(exitStats.keys()))):
			if(exitStats[extLabel] == repeatNests):
				backpatch(exitQuads[i-1],nextQuad())
				exitStats.popitem()
				exitQuads.pop(i-1)
	repeatNests -= 1
	getLex()
	return

def exitStat():
	if(repeatNests == 0):
		errorString = " Found 'exit' keyword outside of a repeat loop: "
		syntaxError(errorString)
		sys.exit()
	tempList = makeList(nextQuad())
	genQuad("jump")
	exitQuads.append(tempList)
	exitStats[tempList[0]] = repeatNests

def whileStat():
	q = nextQuad()
	(BTrue, BFalse) = condition()
	backpatch(BTrue, nextQuad())
	statements()
	genQuad("jump","_","_",q)
	backpatch(BFalse, nextQuad())
	if (token.tokenType != TokenType.ENDWHILE):
		errorString = " Expected 'endwhile' keyword but found "
		syntaxError(errorString)
		sys.exit()
	getLex()
	return

def switchStat():
	completedSwitch = emptyList()
	E1Place = expression()
	if (token.tokenType == TokenType.CASE):
		getLex()
		E2Place = expression()
		sTrue = makeList(nextQuad())
		genQuad("=",E1Place,E2Place)
		sFalse = makeList(nextQuad())
		genQuad("jump")
		if (token.tokenType == TokenType.COLONTK):
			getLex()
			backpatch(sTrue, nextQuad())
			statements()
			tempExitQuad = makeList(nextQuad())
			genQuad("jump")
			completedSwitch = mergeList(completedSwitch, tempExitQuad)
			backpatch(sFalse, nextQuad())
		else:
			errorString = " Expected colon (':') after expression in 'switch' statement but found "
			syntaxError(errorString)
			sys.exit()
	else:
		errorString = " Expected 'case' keyword in 'switch' statement but found "
		syntaxError(errorString)
		sys.exit()
	while (token.tokenType == TokenType.CASE):
		getLex()
		E2Place = expression()
		sTrue = makeList(nextQuad())
		genQuad("=",E1Place,E2Place)
		sFalse = makeList(nextQuad())
		genQuad("jump")
		if (token.tokenType == TokenType.COLONTK):
			getLex()
			backpatch(sTrue, nextQuad())
			statements()
			tempExitQuad = makeList(nextQuad())
			genQuad("jump")
			completedSwitch = mergeList(completedSwitch, tempExitQuad)
			backpatch(sFalse,nextQuad())
		else:
			errorString = " Expected colon (':') after expression in 'switch' statement but found "
			syntaxError(errorString)
			sys.exit()
	if (token.tokenType != TokenType.ENDSWITCH):
		errorString = " Expected 'endswitch' keyword after 'switch' statement but found "
		syntaxError(errorString)
		sys.exit()
	backpatch(completedSwitch,nextQuad())
	getLex()
	return

def forcaseStat():
	forFlag = newTemp()
	tempQuad = nextQuad()
	genQuad(":=",0,"_",forFlag)
	if (token.tokenType == TokenType.WHEN):
		getLex()
		(forTrue, forFalse) = condition()
		if (token.tokenType == TokenType.COLONTK):
			backpatch(forTrue, nextQuad())
			getLex()
			statements()
			genQuad(":=",1,"_",forFlag)
			backpatch(forFalse, nextQuad())
		else:
			errorString = " Expected colon (':') after condition in for-loop statement but found "
			syntaxError(errorString)
			sys.exit()
	else:
		errorString = " Expected at least one 'when' keyword in for-loop but found "
		syntaxError(errorString)
		sys.exit()
	while (token.tokenType == TokenType.WHEN):
		getLex()
		(forTrue, forFalse) = condition()
		if (token.tokenType == TokenType.COLONTK):
			backpatch(forTrue, nextQuad())
			getLex()
			statements()
			genQuad(":=",1,"_",forFlag)
			backpatch(forFalse, nextQuad())
		else:
			errorString = " Expected colon (':') after condition in for-loop but found "
			syntaxError(errorString)
			sys.exit()
	if (token.tokenType != TokenType.ENDFORCASE):
		errorString = " Expected 'endforcase' keyword at the end of the for-loop but found "
		syntaxError(errorString)
		sys.exit()
	checkFlag = makeList(nextQuad())
	genQuad("=",forFlag,0)
	genQuad("jump","_","_",tempQuad)
	backpatch(checkFlag,nextQuad())
	getLex()
	return

def callStat():
	if (token.tokenType == TokenType.IDENTIFIER):
		procorfuncID = token.tokenValue
		getLex()
		actualpars()
	else:
		errorString = " Expected a function/procedure identifier after 'call' keyword but found "
		syntaxError(errorString)
		sys.exit()
	genQuad("call",procorfuncID)
	return

def returnStat():
	global returnDict
	retStr = expression()
	lastScope = len(scopesList) - 1
	returnDict = {lastScope: True}
	genQuad("ret","_","_",retStr)
	return

def printStat():
	outStr = expression()
	genQuad("out","_","_",outStr)
	return

def inputStat():
	if (token.tokenType != TokenType.IDENTIFIER):
		errorString = " Expected an identifier after 'input' keyword but found "
		syntaxError(errorString)
		sys.exit()
	inStr = token.tokenValue
	genQuad("in","_","_",inStr)
	getLex()
	return

def actualpars():
	if (token.tokenType == TokenType.LPARENTK):
		getLex()
		actualparlist()
		if (token.tokenType != TokenType.RPARENTK):
			errorString = " Expected right parenthesis after the parameters but found "
			syntaxError(errorString)
			sys.exit()
		getLex()
		return 1

def actualparlist():
	actualparitem()
	while (token.tokenType == TokenType.COMMATK):
		getLex()
		x = actualparitem()
		if(x == None):
			errorString = " Expected parameter type after comma but found "
			syntaxError(errorString)
			sys.exit()

def actualparitem():
	if (token.tokenType == TokenType.IN):
		getLex()
		inVar = expression()
		genQuad("par",inVar,"in")
		return True
	elif (token.tokenType == TokenType.INOUT):
		getLex()
		inoutVar = token.tokenValue
		if (token.tokenType != TokenType.IDENTIFIER):
			errorString = " Expected an identifier after 'inout' but found "
			syntaxError(errorString)
			sys.exit()
		genQuad("par",inoutVar,"inout")
		getLex()
		return True

def condition():
	(bTrue, bFalse) = boolterm()
	while (token.tokenType == TokenType.OR):
		backpatch(bFalse, nextQuad())
		getLex()
		(q2True, q2False) = boolterm()
		bTrue = mergeList(bTrue, q2True)
		bFalse = q2False
	return (bTrue, bFalse)

def boolterm():
	(qTrue, qFalse) = boolfactor()
	while (token.tokenType == TokenType.AND):
		backpatch(qTrue, nextQuad())
		getLex()
		(r2True, r2False) = boolfactor()
		qFalse = mergeList(qFalse, r2False)
		qTrue = r2True
	return (qTrue,qFalse)

def boolfactor():
	if (token.tokenType == TokenType.NOT):
		getLex()
		if (token.tokenType == TokenType.LBRACKETK):
			getLex()
			BFPlace = condition()
			BFPlace = BFPlace[::-1]
			if (token.tokenType != TokenType.RBRACKETK):
				errorString = "Expected right bracket after the logical expression but found "
				syntaxError(errorString)
				sys.exit()
			getLex()
		else:
			errorString = " Expected left bracket after the logical expression but found "
			syntaxError(errorString)
			sys.exit()
	elif (token.tokenType == TokenType.LBRACKETK):
		getLex()
		BFPlace = condition()
		if (token.tokenType != TokenType.RBRACKETK):
			errorString = " Expected right bracket after the logical expression but found "
			syntaxError(errorString)
			sys.exit()
		getLex()
	elif (token.tokenType == TokenType.TRUE or token.tokenType == TokenType.FALSE):
		if(token.tokenType == TokenType.TRUE):
			BFTrue = makeList(nextQuad())
			genQuad("jump")
			BFFalse = emptyList()
		else:
			BFTrue = emptyList()
			BFFalse = makeList(nextQuad())
			genQuad("jump")
		getLex()
		return (BFTrue,BFFalse)
	else:
		E1Place = expression()
		relop = relationalOper()
		E2Place = expression()
		BFTrue = makeList(nextQuad())
		genQuad(relop,E1Place,E2Place)
		BFFalse = makeList(nextQuad())
		genQuad("jump")
		BFPlace = (BFTrue, BFFalse)
	return BFPlace

def expression():
	x = optionalSign()
	T1place = term()
	if(x != None):
		opsignTemp = newTemp()
		genQuad(x,0,str(T1place),str(opsignTemp))
		T1place = opsignTemp
	x = addOper()
	while (x != None):
		T2place = term()
		op3 = newTemp()
		genQuad(x,str(T1place),str(T2place),op3)
		T1place = op3
		x = addOper()
	return T1place

def term():
	F1place = factor()
	x = mulOper()
	while (x != None):
		F2place = factor()
		op3 = newTemp()
		genQuad(x,F1place,F2place,op3)
		F1place = op3
		x = mulOper()
	return F1place

def factor():
	if (token.tokenType == TokenType.LPARENTK):
		getLex()
		Fplace = expression()
		if (token.tokenType != TokenType.RPARENTK):
			errorString = " Expected right parenthesis after expression but found "
			syntaxError(errorString)
			sys.exit()
		getLex()
	elif (token.tokenType == TokenType.IDENTIFIER):
		Fplace = token.tokenValue
		getLex()
		tail = idtail()
		if(tail != None):
			retVar = newTemp()
			genQuad("par",retVar,"ret")
			genQuad("call",Fplace)
			Fplace = retVar
	elif (token.tokenType == TokenType.NUMBER \
	   or token.tokenType == TokenType.ADDTK  \
	   or token.tokenType == TokenType.MINUSTK):
		if (token.tokenType == TokenType.ADDTK\
		 or token.tokenType == TokenType.MINUSTK):
			x = token.tokenValue
			getLex()
			if (token.tokenType != TokenType.NUMBER):
				errorString = " A number was expected after the sign but found "
				syntaxError(errorString)
				sys.exit()
			numVal = token.tokenValue
			opsignTemp = newTemp()
			genQuad(x,0,numVal,opsignTemp)
			getLex()
			return opsignTemp
		numVal = token.tokenValue
		getLex()
		return numVal
	else:
		errorString = " Expected factor but found "
		syntaxError(errorString)
		sys.exit()
	return Fplace

def idtail():
	return actualpars()

def relationalOper():
	returnOp = token.tokenValue
	if (not(token.tokenType in \
		{TokenType.LEQTK, TokenType.NEQTK, TokenType.LESSTK,\
		 TokenType.GREQTK, TokenType.GREATK, TokenType.EQTK})):
		errorString = " Expected a relational operand in expression but found "
		syntaxError(errorString)
		sys.exit()
	getLex()
	return returnOp

def addOper():
	if (not(token.tokenType in {TokenType.ADDTK, TokenType.MINUSTK})):
		return None
	operand = token.tokenValue
	getLex()
	return operand

def mulOper():
	if (not(token.tokenType in {TokenType.MULTITK, TokenType.DIVTK})):
		return None
	operand = token.tokenValue
	getLex()
	return operand

def optionalSign():
	if(token.tokenType in {TokenType.ADDTK,TokenType.MINUSTK}):
		return addOper()

#-------------------------------------------#
#-------------------------------------------#
#-------------Intermediate Code-------------#
#-------------------------------------------#
#-------------------------------------------#

def nextQuad():
	return labelID


def genQuad(name=None, op1="_", op2="_", op3="_"):
	global labelID
	quadList.append(Quad(labelID, name, op1, op2, op3))
	labelID += 1

def newTemp():
	global tempVarsID
	newVar = "T_"+str(tempVarsID)
	tempVarsID += 1
	tempVars[newVar] = None
	offset = scopesList[-1].getTempFrameLength()
	scopesList[-1].addEntity(TempVariable(newVar, offset))
	scopesList[-1].setTempFrameLength()
	return newVar

def emptyList():
	return list()

def makeList(label):
	newList = list()
	newList.append(label)
	return newList

def mergeList(list1, list2):
	return list1 + list2

def backpatch(list1, newOp3):
	for quad in quadList:
		if quad.label in list1:
			quad.op3 = newOp3

def printIntermediateCode():
	for quad in quadList:
		intoutfile.write(quad.__str__() + '\n')

def printCequivCode():
	coutfile.write('#include <stdio.h>\n')
	coutfile.write('#include <stdlib.h>')
	coutfile.write('\n\n')	
	for quad in quadList:
		if(quad.name == 'begin_block'):
			coutfile.write('int main()')
			coutfile.write('\n')
			coutfile.write('{')
			coutfile.write('\n')
			printVars()
			coutfile.write('\n')
		else:	
			coutfile.write('L_' + str(quad.label) + ': ')
			quadToC(quad)
			coutfile.write('\n')

def findVars():
	# Using a dict here allows us to ignore duplicates
	vars = dict()
	strNums = ['0','1','2','3','4','5','6','7','8','9']
	for quad in quadList:
		if(quad.name == 'end_block'):
			break
		if(not(quad.name in {'begin_block','call'}) and not(quad.op2 in {'in','inout','ret'})):
			if(not (str(quad.op1).split()[0][0] in strNums)):
				vars[quad.op1] = 'int'
			if(not (str(quad.op2).split()[0][0] in strNums)):
				vars[quad.op2] = 'int'
			if(not (str(quad.op3).split()[0][0] in strNums)):
				vars[quad.op3] = 'int'
	if('_' in vars):
		del vars['_']
	return vars

def printVars():
	vars = findVars()
	coutfile.write('int ')
	for var in vars:
		if(var == list(vars.keys())[-1]):
			coutfile.write(str(var) + ';')
			break
		coutfile.write(str(var) + ', ')
	coutfile.write('\n')
	return


def quadToC(quad):
	if(quad.name in {'+','-','*','/'}):
		coutfile.write(str(quad.op3) + '= ' + str(quad.op1) +\
					   str(quad.name) + ' ' + str(quad.op2) + ';')
	elif(quad.name in {'<','<=','>','>=','=','<>'}):
		if(quad.name == '='):
			coutfile.write('if (' + str(quad.op1) + ' == ' +\
						   str(quad.op2) + ') goto L_'+ str(quad.op3) + ';')
		elif(quad.name == '<>'):
			coutfile.write('if (' + str(quad.op1) + ' != ' +\
						   str(quad.op2) + ') goto L_'+ str(quad.op3) + ';')
		else:
			coutfile.write('if (' + str(quad.op1) + ' ' + str(quad.name) +\
			 			   ' ' + str(quad.op2) + ') goto L_' + str(quad.op3) + ';')
	elif(quad.name == ':='):
		coutfile.write(str(quad.op3) + ' = ' + str(quad.op1) + ';')
	elif(quad.name == 'jump'):
		coutfile.write('goto L_' + str(quad.op3) + ';')
	elif(quad.name == 'ret'):
		coutfile.write('return ' + str(quad.op3) + ';')
	elif(quad.name == 'call'):
		# This won't do anything, just for aesthetic purposes, for now
		# The .c file won't compile, remove call from .eel file
		coutfile.write('call ' + str(quad.op1) + ';')
	elif(quad.name == 'out'):
		# This will print only integers placed in quad.op3
		# Or variables containing integers
		if(quad.op3.isalpha()):
			coutfile.write('printf("%s: %d\\n", "'+str(quad.op3)+"\", " + str(quad.op3) + ');')
		else:
			coutfile.write('printf("%d\\n",'+ str(quad.op3) + ');')
	elif(quad.name == 'in'):
		# Keep it pretty, user is prompted to input an integer
		coutfile.write('printf(">%s: ", "'+str(quad.op3)+'"); ')
		# Input is only integers 
		# Variables used should have been declared accordingly
		coutfile.write('scanf("%d", &' + str(quad.op3) + ');\n')
		# In case wrong input is issued
		coutfile.write('\tif('+str(quad.op3)+'> 32767 || '+str(quad.op3)+' < -32767){\n')
		coutfile.write('\t\tprintf("%d exceeds the absolute value of 32767. Exiting...\\n", '+str(quad.op3)+');\n')
		coutfile.write('\t\texit(0);\n')
		coutfile.write('\t}')
	elif(quad.name == 'end_block'):
		coutfile.write('{}\n}')
	elif(quad.name == 'halt'):
		coutfile.write('return 0;')
	return

def parser():
	program()
	token.tokenValue = ""
	getLex()
	if (token.tokenValue != ""):
		print("Stray code: "+token.tokenValue)
		print("Stray code found after endprogram. Exiting...")
		sys.exit()

#-------------------------------------------#
#-------------------------------------------#
#---------------Symbol Table----------------#
#-------------------------------------------#
#-------------------------------------------#

# Create a new scope
def createScope():
	scopesList.append(Scope(scopesList[-1].nestingLevel + 1, scopesList[-1]))

# Add a new function to the entities list
def addFunction(id, funcorproc):
	if(checkScopeForEntity(id, SymbolType.FUNCTION, scopesList[-1].previousScope.nestingLevel) == True):
		errorString = " Fuction name '" + id + "' has already been defined."
		semanticsError(errorString)
		sys.exit()
	scopesList[-2].addEntity(Function(id,funcorproc,-1))

# Add a variable entity to the entities list
def addVariable(id):
	#print("Variable: " + id)
	#print("Check: " + str(checkScopeForEntity(id, SymbolType.VARIABLE, scopesList[-1].nestingLevel)))
	if (checkScopeForEntity(id, SymbolType.VARIABLE, scopesList[-1].nestingLevel) == True):
		errorString = " Variable '" + id + "' has already been defined in this scope."
		semanticsError(errorString)
		sys.exit()
	if (checkScopeForEntity(id, SymbolType.PARAMETER ,scopesList[-1].nestingLevel) == True):
		errorString = " Variable '" + id +"' has already been defined as a parameter in this scope."
		semanticsError(errorString)
		sys.exit()
	offset = scopesList[-1].getTempFrameLength()
	scopesList[-1].addEntity(Variable(id, offset))
	scopesList[-1].setTempFrameLength()

# Add a parameter entity to the entities list
def addParameter(id, parMode):
	if (checkScopeForEntity(id, SymbolType.PARAMETER, scopesList[-1].nestingLevel) == True):
		errorString = " Parameter '" + id + "' has already been defined in this scope."
		semanticsError(errorString)
		sys.exit()
	offset = scopesList[-1].getTempFrameLength()
	scopesList[-1].addEntity(Parameter(id, offset, parMode))
	scopesList[-1].setTempFrameLength()

# Add an argument to the function 'funcId'
def addArgument(funcId, parMode):
	newArgument = Argument(parMode)
	function = findEntity(funcId, SymbolType.FUNCTION)[0]
	# This shouldn't be called, ever
	if (function == None):
		errorString = " Function '" + funcId + "' hasn't been defined."
		semanticsError(errorString)
		sys.exit()
	#print("AddArg Function Name: " + function.id)
	function.addArg(newArgument)

# Set function's startQuad
def setFunctionStartQuad(id):
	startQuad = nextQuad()
	if (id == main_name):
		return startQuad
	else:
		function = findEntityWithNoFrameLen(id, SymbolType.FUNCTION)
		if (function == None):
			errorString = " '" + function + "' has not been defined."
			semanticsError(errorString)
			sys.exit()
		function.setStartQuad(startQuad)
		return startQuad

# Set function's framelength
def setFunctionFrameLength(id, framelength):
	global mainframe
	if (id == main_name):
		# WE GOTTA UPDATE THE MAINFRAME !!!
		mainframe = framelength
		return
	else:
		function = findEntityWithNoFrameLen(id, SymbolType.FUNCTION)
		function.setFrameLegth(framelength)

#-------------------------------------------#
#-------------------------------------------#
#----------------Final code-----------------#
#-------------------------------------------#
#-------------------------------------------#

def gnlvcode(var):
	entity, nestLevel = findEntityWithNoType(var)
	if(entity == None):
		errorString = " Variable '" + var + "' has not been declared."
		semanticsError(errorString)
		sys.exit()
	if (entity.typeOf == SymbolType.FUNCTION):
		errorString = " Variable '" + var + "' has not been declared."
		semanticsError(errorString)
		sys.exit()
	asmfile.write("    lw  $t0, -4($sp)\n")
	counter = scopesList[-1].nestingLevel - nestLevel - 1 # N-1 times
	for i in range(counter, 0, -1):
		asmfile.write("	   lw  $t0, -4($t0)\n")
		# This shouldn't get triggered
		if (i < 0):
			print("Something went wrong.")
			sys.exit()
	asmfile.write("    add  $t0, $t0, -%d\n" %entity.offset)

def loadvr(v, r):
	register = "t" + str(r)
	# If v is just an integer, just load it
	if (str(v).isdigit()):
		asmfile.write("    li  $%s, %s\n" % (register, v))
		return
	entity, level = findEntityWithNoType(v)
	if(entity == None):
		errorString = " Variable '" + v + "' has not been declared."
		semanticsError(errorString)
		sys.exit()
	currScope = scopesList[-1].nestingLevel
	# 1. Global Variables
	if (level == 0 and entity.typeOf == SymbolType.VARIABLE):
		asmfile.write("    lw  $%s, -%d($s0)\n" % (register, entity.offset))
	# 2. Temp Variable
	elif (entity.typeOf == SymbolType.TEMPVARIABLE):
		asmfile.write("    lw  $%s, -%d($sp)\n" % (register, entity.offset))
	# 3. Level equal to the currScope
	elif (level == currScope and entity.typeOf != SymbolType.FUNCTION):
		# 3.1. Variable or parameter by value
		if (entity.typeOf == SymbolType.VARIABLE or\
		   (entity.typeOf == SymbolType.PARAMETER and entity.parMode == "cv")):
		   asmfile.write("    lw  $%s, -%d($sp)\n" % (register, entity.offset))
		# 3.2. Parameter by reference 
		elif (entity.typeOf == SymbolType.PARAMETER and entity.parMode == "cr"):
			asmfile.write("    lw  $t0, -%d($sp)\n" % entity.offset)
			asmfile.write("    lw  $%s, ($t0)\n" % register)	
	# 4. Level less than the currScope
	elif (level < currScope):
		# 4.1. Variable or parameter by value
		if (entity.typeOf == SymbolType.VARIABLE or\
		   (entity.typeOf == SymbolType.PARAMETER and entity.parMode == "cv")):
		   gnlvcode(v)
		   asmfile.write("    lw  $%s, ($t0)\n" % register)
		# 4.2. Parameter by reference
		elif (entity.typeOf == SymbolType.PARAMETER and entity.parMode == "cr"):
			gnlvcode(v)
			asmfile.write("    lw  $t0, ($t0)\n")
			asmfile.write("    lw  $%s, ($t0)\n" % register)
	else:
		print("Potato, po-ta-to something terrible happened...")
		sys.exit()

def storerv(r, v):
	register = "t" + str(r)
	entity, level = findEntityWithNoType(v)
	if(entity == None):
		errorString = " Variable '" + v + "' has not been declared."
		semanticsError(errorString)
		sys.exit()
	currScope = scopesList[-1].nestingLevel
	# 1. Global Variables
	if (level == 0 and entity.typeOf == SymbolType.VARIABLE):
		asmfile.write("    sw  $%s, -%d($s0)\n" % (register, entity.offset))
	# 2. Temp Variable
	elif (entity.typeOf == SymbolType.TEMPVARIABLE):
		asmfile.write("    sw  $%s, -%d($sp)\n" % (register, entity.offset))
	# 3. Level equal to the currScope
	elif (level == currScope and entity.typeOf != SymbolType.FUNCTION):
		# 3.1. Variable or parameter by value
		if (entity.typeOf == SymbolType.VARIABLE or\
		   (entity.typeOf == SymbolType.PARAMETER and entity.parMode == "cv")):
		   asmfile.write("    sw  $%s, -%d($sp)\n" % (register, entity.offset))
		# 3.2. Parameter by reference 
		elif (entity.typeOf == SymbolType.PARAMETER and entity.parMode == "cr"):
			asmfile.write("    lw  $t0, -%d($sp)\n" % entity.offset)
			asmfile.write("    sw  $%s, ($t0)\n" % register)	
	# 4. Level less than the currScope
	elif (level < currScope):
		# 4.1. Variable or parameter by value
		if (entity.typeOf == SymbolType.VARIABLE or\
		   (entity.typeOf == SymbolType.PARAMETER and entity.parMode == "cv")):
		   gnlvcode(v)
		   asmfile.write("    sw  $%s, ($t0)\n" % register)
		# 4.2. Parameter by reference
		elif (entity.typeOf == SymbolType.PARAMETER and entity.parMode == "cr"):
			gnlvcode(v)
			asmfile.write("    lw  $t0, ($t0)\n")
			asmfile.write("    sw  $%s, ($t0)\n" % register)
	else:
		print("Potato, po-ta-to something terrible happened...")
		sys.exit()

def quadToAsm(quad, name):
	global parametersEncountered, parametersDict, promptDict
	if (str(quad.label == 0)):
		# This is exactly the space that is needed. DO NOT CHANGE
		asmfile.write("                  ")
	asmfile.write('\nL_' + str(quad.label) + ':\n')
	if (quad.name in {'+','-','*','/'}):
		if(quad.name == '+'):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    add  $t1, $t1, $t2\n")
			storerv(1, quad.op3)
		elif(quad.name == '-'):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    sub  $t1, $t1, $t2\n")
			storerv(1, quad.op3)
		elif(quad.name == '*'):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    mul  $t1, $t1, $t2\n")
			storerv(1, quad.op3)
		elif(quad.name =='/'):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    div  $t1, $t1, $t2\n")
			storerv(1, quad.op3)
	elif (quad.name in {'<','<=','>','>=','=','<>'}):
		if(quad.name == '<'):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    blt  $t1, $t2, L_%s\n" % str(quad.op3))
		elif(quad.name == '<='):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    ble  $t1, $t2, L_%s\n" % str(quad.op3))
		elif(quad.name == '>'):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    bgt  $t1, $t2, L_%s\n" % str(quad.op3))
		elif(quad.name == '>='):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    bge  $t1, $t2, L_%s\n" % str(quad.op3))
		elif(quad.name == '='):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    beq  $t1, $t2, L_%s\n" % str(quad.op3))
		elif(quad.name == '<>'):
			loadvr(quad.op1, 1)
			loadvr(quad.op2, 2)
			asmfile.write("    bne  $t1, $t2, L_%s\n" % str(quad.op3))
	elif (quad.name == ':='):
		if(quad.op3 == 'forFlag'):
			loadvr(quad.op1, 8)
			storerv(8, quad.op3)
		else:
			loadvr(quad.op1, 1)
			storerv(1, quad.op3)
	elif (quad.name == 'jump'):
		asmfile.write("    j  L_%s\n" % str(quad.op3))
	elif (quad.name == 'ret'):
		loadvr(quad.op3, 1)
		asmfile.write("    lw  $t0, -8($sp)\n")
		asmfile.write("    sw  $t1, ($t0)\n")
		# Return to the caller with jr ra
		asmfile.write("    lw  $ra, ($sp)\n")
		asmfile.write("    jr  $ra\n")
	elif(quad.name == 'call'):
		# Main can not be detected by findEntity
		if(name == main_name):
			callerFramelength = mainframe
			callerScope = 0
		else:
			# Get the caller function
			callerFunction, callerScope = findEntity(name, SymbolType.FUNCTION)
			callerFramelength = callerFunction.frameLength
		# Get the callee function
		calleeFunction, calleeScope = findEntity(quad.op1, SymbolType.FUNCTION)
		if(calleeFunction == None):
			errorString = " Function or Procedure '" + quad.op1 + "' has not been defined yet. " +\
						  "(Or it is being defined later on and can't 'see' it yet.)"
			semanticsError(errorString)
			sys.exit()
		# Check parameters to abide by the semantic rules
		parameterCheck(calleeFunction)
		# If the caller and the callee are on the same scope, they have the same parent
		if (calleeScope == callerScope):
			asmfile.write("    lw  $t0, -4($sp)\n")
			asmfile.write("    sw  $t0, -4($fp)\n")
		# Else the caller is the parent of the callee
		else:
			asmfile.write("    sw  $sp, -4($fp)\n")
		# Move the stack pointer to the  callee
		asmfile.write("    addi  $sp, $sp, -%d\n" % callerFramelength)
		# Go to the callee
		asmfile.write("    jal  L_%d\n" % calleeFunction.startQuad)
		# Restore the stack pointer to the caller
		asmfile.write("    addi  $sp, $sp, %d\n" % callerFramelength)
	elif(quad.name == 'out'):
		if(str(quad.op3).isdigit()):
			# Print the integer value
			asmfile.write("    li  $v0, 1\n")
			asmfile.write("    li  $a0, %s\n" % str(quad.op3))
		else:

			if (not (quad.op3 in tempVars)):
				promptStr = quad.op3 + '_out'
				promptDict[promptStr] = 'out'
				# Print the variable's name as a prompt
				asmfile.write("    li  $v0, 4\n")
				asmfile.write("    la  $a0, %s\n" % promptStr)
				asmfile.write("    syscall\n")
			# Print the integer value thats stored in the variable
			asmfile.write("    li  $v0, 1\n")
			# Load the value of the variable to the register $t1
			loadvr(quad.op3, 1)
			asmfile.write("    add  $a0, $t1, $zero\n")
		asmfile.write("    syscall\n")
		# Print a newline, so to make it readable
		asmfile.write("    li  $v0, 4\n")
		asmfile.write("    la  $a0, newLine\n")
		asmfile.write("    syscall\n")
	elif(quad.name == 'in'):
		# Managed to get a prompt
		print("quad.op3: "+quad.op3)
		promptDict[quad.op3] = 'in'
		asmfile.write("    li  $v0, 4\n")
		asmfile.write("    la  $a0, %s\n" % quad.op3)
		asmfile.write("    syscall\n")
		asmfile.write("    li  $v0, 5\n")
		asmfile.write("    syscall\n")
		asmfile.write("    move $t1, $v0\n")
		storerv(1, quad.op3)
	elif(quad.name == 'par'):
		# Same as call
		# Main can not be detected by findEntity
		if(name == main_name):
			callerFramelength = mainframe
			callerScope = 0
		else:
			callerFunction, callerScope = findEntity(name, SymbolType.FUNCTION)
			callerFramelength = callerFunction.frameLength
		# If it's the first parameter
		if (parametersEncountered == 0):
			asmfile.write("    addi  $fp, $sp, -%d\n" % callerFramelength)
		# Ret quads will always be first or last so the above if-statement will
		# always get triggered when it needs to. But the parameter check, which 
		# takes place in the translation of the 'call' function, ignores any 'ret' 
		# arguments, so we do that here too.
		if (quad.op2 != 'ret'):
			#print("parametersEncountered: quad.op2 = " + str(parametersEncountered) + ": "  + quad.op2)
			parametersDict[parametersEncountered] = quad
			parametersEncountered += 1
		# Might be parametersEncountered -1..... and it was.... at 6:34 in the morning
		offset = 12 + 4 * (parametersEncountered-1)
		if (quad.op2 == 'in'):
			loadvr(quad.op1, 0)
			asmfile.write("    sw  $t0, -%d($fp)\n" % offset)
		elif (quad.op2 == 'inout'):
			paramEntity, paramScope = findEntityWithNoType(quad.op1)
			if (paramEntity == None):
				errorString = " Variable '" + quad.op1 + "' has not been declared."
				semanticsError(errorString)
				sys.exit()
			# If the caller and the parameter are on the same scope
			if (paramScope == callerScope):
				# If the parameter is a variable or a parameter by value on the parent
				if (paramEntity.typeOf == SymbolType.VARIABLE or \
				   (paramEntity.typeOf == SymbolType.PARAMETER and paramEntity.parMode == 'cv')):
					asmfile.write("    add  $t0, $sp, -%s\n" % paramEntity.offset)
					asmfile.write("    sw  $t0, -%d($fp)\n" % offset)
				# If the parameter is a parameter by reference on the parent
				elif (paramEntity.typeOf == SymbolType.PARAMETER and paramEntity.parMode == 'cr'):
					asmfile.write("    lw  $t0, -%s($sp)\n" % paramEntity.offset)
					asmfile.write("    sw  $t0, -%d($fp)\n" % offset)
			# If the caller and the parameter are on a different scope
			else:
				# If the parameter is a variable or a parameter by value on the parent
				if (paramEntity.typeOf == SymbolType.VARIABLE or \
				   (paramEntity.typeOf == SymbolType.PARAMETER and paramEntity.parMode == 'cv')):
				   gnlvcode(quad.op1)
				   asmfile.write("    sw  $t0, -%d($fp)\n" % offset)
				# If the parameter is a parameter by reference on the parent
				elif (paramEntity.typeOf == SymbolType.PARAMETER and paramEntity.parMode == 'cr'):
					gnlvcode(quad.op1)
					asmfile.write("    lw  $t0, ($t0)\n")
					asmfile.write("    sw  $t0, -%d($fp)\n" % offset)
		elif (quad.op2 == 'ret'):
			paramEntity, paramScope = findEntityWithNoType(quad.op1)
			if (paramEntity == None):
				errorString = " Variable '" + quad.op1 + "' has not been declared."
				semanticsError(errorString)
				sys.exit()
			asmfile.write("    add  $t0, $sp, -%s\n" % paramEntity.offset)
			asmfile.write("    sw  $t0, -8($fp)\n")
	elif (quad.name == 'begin_block'):
		asmfile.write("    sw  $ra, ($sp)\n")
		if (quad.op1 == main_name):
			asmfile.seek(0)
			asmfile.write(".text\n")
			asmfile.write("    j  L_%d\n\n" % quad.label)
			asmfile.seek(0,2)
			asmfile.write("    add  $sp, $sp, %d\n" % mainframe)
			asmfile.write("    move  $s0, $sp\n")
	elif (quad.name == 'end_block'):
		if (quad.op1 == main_name):
			asmfile.write("    j  L_%d\n" % haltLabel)
			asmfile.write("\n\n")
			asmfile.write(".data\n")
			asmfile.write("    newLine:  .asciiz  \"\\n\"\n")
			# WHATTA NICE PROMPTS! (SUCH WOW, MUCH FANCY)
			for prompt, typeOf in promptDict.items():
				if (typeOf == 'in'):
					asmfile.write("    %s:  .asciiz \"> %s : \"\n" % (prompt, prompt))
				elif (typeOf == 'out'):
					prompt1 = prompt.split('_')[0]
					asmfile.write("    %s:  .asciiz \"%s =  \"\n" % (prompt, prompt1))
		else:
			asmfile.write("    lw  $ra, ($sp)\n")
			asmfile.write("    jr  $ra\n")
	elif (quad.name == 'halt'):
		asmfile.write("    li  $v0, 10\n")
		asmfile.write("    syscall\n")

#-------------------------------------------#
#-------------------------------------------#
#------------Searching/Checking-------------#
#-------------------------------------------#
#-------------------------------------------#

# Check if an entity 'id' of type 'typeOf' exists at scope 'nestingLevel'
def checkScopeForEntity(id, typeOf, nestingLevel):
	if (nestingLevel > scopesList[-1].nestingLevel):
		print("Noooooooooooooo")
		return False
	else:
		currentScope = scopesList[nestingLevel]
		for i in range(len(currentScope.entities)):
			x = currentScope.entities[i]
			for j in range(len(currentScope.entities)):
				y = currentScope.entities[j]
				if x.id == y.id and x.typeOf == y.typeOf and x.id == id and x.typeOf == typeOf:
						return True
		return False

# Find an entity named 'id' and of type 'typeOf'
# Return that entity along with the level it was found on
def findEntity(id, typeOf):
	currentScope = scopesList[-1]
	while(currentScope != None):
		for i in range(len(currentScope.entities)):
			if(currentScope.entities[i].id == id and currentScope.entities[i].typeOf == typeOf):
				return (currentScope.entities[i], currentScope.nestingLevel)
		currentScope = currentScope.previousScope
	return (None, None)

# Find and return an entity named 'id' with the level it was found on 
def findEntityWithNoType(id):
	if (scopesList == list()):
		return
	currScope = scopesList[-1]
	while (currScope != None):
		for currEntity in currScope.entities:
			if (currEntity.id == id):
				return (currEntity, currScope.nestingLevel)
		currScope = currScope.previousScope
	return (None, None)

# Enables the naming of functions in subsequent scopes the same as
# those in their above scopes
# Returns the first entity with name 'id' and of type 'typeOf'
# which hasn't had its framelength updated yet
def findEntityWithNoFrameLen(id, typeOf):
	currentScope = scopesList[-1]
	while(currentScope != None):
		for i in range(len(currentScope.entities)):
			if (currentScope.entities[i].id == id and currentScope.entities[i].typeOf == typeOf):
				if (currentScope.entities[i].frameLength != -1):
					break
				else:
					return currentScope.entities[i]
		currentScope = currentScope.previousScope

def parameterCheck(function):
	global parametersDict, parametersEncountered
	#print("Parameters encounted: " + str(parametersEncountered))
	#print("Len(function.args): " + str(len(function.args)))
	if (parametersEncountered < len(function.args)):
		errorString = " Too few arguments in function call."
		semanticsError(errorString)
		sys.exit()
	elif (parametersEncountered > len(function.args)):
		errorString = " Too many arguments in function call."
		semanticsError(errorString)
		sys.exit()
	parametersEncountered = 0
	for i in list(parametersDict):
		#print("I: " + str(i))
		#print("quadPar.op2: " + quadPar.op2)
		#print("function.args[i]: " + function.args[i].parMode)
		if (parametersDict[i].op2 == "in" and function.args[i].parMode == 'cv'):
			del parametersDict[i]
		elif (parametersDict[i].op2 == 'inout' and function.args[i].parMode == 'cr'):
			del parametersDict[i]
		else:
			errorString = " Type missmatch for parameter " + parametersDict[i].op1 + "."
			semanticsError(errorString)
			sys.exit()

#-------------------------------------------#
#-------------------------------------------#
#-----------Printing Quads/Scopes-----------#
#-------------------------------------------#
#-------------------------------------------#

#Used in debugging
def printQuads():
	for quad in quadList:
		print(quad.__str__())

def printScopesToTerminal():
	for currScope in scopesList:
		print("    " * (currScope.nestingLevel) + "➤ " + color.UNDERLINE + color.BOLD + color.GREEN+\
			  "Scope " + color.RED + str(currScope) + color.END)
		for currEntity in currScope.entities:
			print("    " * (currScope.nestingLevel) + " • " + str(currEntity))
			if (isinstance(currEntity, Function)):
				for i,currArg in enumerate(currEntity.args):
					print("    " * (currScope.nestingLevel+1) + " " + str(i+1) + ". " + str(currArg))
	print("\n")
	print(color.BOLD + color.CYAN + ">> Main Program's Framelength: " + color.END + str(mainframe))
	print('{:-^100}'.format(''))
	print('{:-^100}'.format(''))
	print("\n")

def printScopesToFile():
	for currScope in scopesList:
		symtablefile.write("    " * (currScope.nestingLevel) + "➤ Scope " + str(currScope) + "\n")
		for currEntity in currScope.entities:
			symtablefile.write("    " * (currScope.nestingLevel) + " • " + str(currEntity.toString()) + "\n")
			if (isinstance(currEntity, Function)):
				for i,currArg in enumerate(currEntity.args):
					symtablefile.write("    " * (currScope.nestingLevel+1) + " " + str(i+1) + ". " + str(currArg) + "\n")
	symtablefile.write("\n")
	symtablefile.write(">> Main Program's Framelength: " + str(mainframe) + "\n")
	symtablefile.write('{:-^100}'.format('') + "\n")
	symtablefile.write('{:-^100}'.format('') + "\n")
	symtablefile.write("\n")

#-------------------------------------------#
#-------------------------------------------#
#---------------File Handling---------------#
#-------------------------------------------#
#-------------------------------------------#

def open_files(inputFile, intermOutputFile, cOutputFile, symtableOutputFile, asmOutputFile):
	global infile, file_size, intoutfile, coutfile, symtablefile, asmfile
	checkInputFile = inputFile.split('.')
	if(str(checkInputFile[1]) != 'eel'):
		print("Input file is not an EEL file.\nCan not parse.\nNow exiting...")
		sys.exit()
	with open(inputFile, mode= 'r', encoding='utf-8') as infile:
		file_size = infile.seek(0,2)
		file_size = infile.tell()
		infile.seek(0)
		# Symbol table file
		if (symtableOutputFile == None):
			symtableOutputFile = checkInputFile[0]+'.txt'
		else:
			symtableOutputFile = symtableOutputFile.split('.')[0]+'.txt'
		# Assembly file
		symtablefile = open(symtableOutputFile, mode='w', encoding= 'utf-8')
		if (asmOutputFile == None):
			asmOutputFile = checkInputFile[0]+'.asm'
		else:
			asmOutputFile = asmOutputFile.split('.')[0]+'.asm'
		asmfile = open(asmOutputFile, mode='w', encoding='utf-8')
		# Starting the parser

		print('\t'+color.DARKCYAN+color.BOLD+"Syntax n' Semantic Analysis         "+color.END,end="",flush=True)
		print(color.GREEN+color.BOLD+"["+color.END+color.YELLOW+color.BOLD+" PARSING "+color.END+color.GREEN+color.BOLD+"]"+color.END,end='\r')
		parser()	
		time.sleep(1)
		print('\t'+color.DARKCYAN+color.BOLD+"Syntax Analysis                    "+color.END,end="",flush=True)
		print(color.GREEN+color.BOLD+"["+color.END+color.GREEN+color.BOLD+"SUCCESSFUL"+color.END+color.GREEN+color.BOLD+"]"+color.END)
		print('\t'+color.DARKCYAN+color.BOLD+"Semantics Analysis                 "+color.END,end="",flush=True)
		print(color.GREEN+color.BOLD+"["+color.END+color.GREEN+color.BOLD+"SUCCESSFUL"+color.END+color.GREEN+color.BOLD+"]"+color.END)
		time.sleep(1)
		print('\t'+color.DARKCYAN+color.BOLD+"Generating Files                   "+color.END,end="",flush=True)
		print(color.GREEN+color.BOLD+"["+color.END+color.PURPLE+color.BOLD+" CREATING "+color.END+color.GREEN+color.BOLD+"]"+color.END,end='\r')
		time.sleep(1)
		if(intermOutputFile == None):
			intermOutputFile = checkInputFile[0]+'.int'
		else:
			intermOutputFile = intermOutputFile.split('.')[0]+'.int'
		intoutfile = open(intermOutputFile, mode= 'w', encoding = 'utf-8')
		if(cOutputFile == None):
			cOutputFile = checkInputFile[0]+'.c'
		else:
			cOutputFile = cOutputFile.split('.')[0]+'.c'
		coutfile = open(cOutputFile, mode='w', encoding= 'utf-8')

def close_files():
	intoutfile.close()
	coutfile.close()
	symtablefile.close()
	asmfile.close()

#-------------------------------------------#
#-------------------------------------------#
#----------------Fancy Stuff----------------#
#-------------------------------------------#
#-------------------------------------------#


info = color.BOLD+color.RED+'{:-^100}'.format('INFO')+color.END+'\n\n'+\
	   color.UNDERLINE+color.BOLD+color.DARKCYAN+'{:^100}'.format('EEL Compiler v.1.0')+color.END+"\n\n"+\
	   color.CYAN+color.BOLD+'Usage\n'+color.END+\
	   color.BOLD+'1. '+'.\\Compiler.py -h:'+color.END+' Display help message and exit.\n'+\
	   color.BOLD+'2. '+'.\\Compiler.py -v:'+color.END+' Display program\'s version and exit.\n'+\
	   color.BOLD+'3. '+'.\\Compiler.py -i:'+color.END+' Display info message and exit.\n'+\
	   color.BOLD+'4. '+'.\\Compiler.py oysters.eel:'+color.END+' Compile \'oysters\' eel file.'+\
					  'The intermediate and c code files will be named \'oysters.int\''+\
					  ' and \'oysters.c\'\n   respectively.\n'+\
	   color.BOLD+'5. '+'.\\Compiler.py oysters.eel -int clams: '+color.END+\
					  'Compile \'oysters\' eel file and name the intermediate code file'+\
					  ' \'clams.int\'.\n'+\
	   color.BOLD+'6. '+'.\\Compiler.py oysters.eel -c cockles: '+color.END+\
					  'Compile \'oysters\' eel file and name the c code file'+\
					  ' \'cockles.c\'.\n'+\
	   color.BOLD+'7. '+'.\\Compiler.py oysters.eel -sym potatoes: '+color.END+\
					  'Compile \'oysters\' eel file and name the symbol table output file'+\
					  ' \'potatoes.txt\'.\n'+\
	   color.BOLD+'8. '+'.\\Compiler.py oysters.eel -asm tomatoes: '+color.END+\
					  'Compile \'oysters\' eel file and name the assembly output file'+\
					  ' \'tomatoes.asm\'.\n'+\
	   color.BOLD+'9. '+'You can combine any of the above ^^.\n\n'+\
	   color.UNDERLINE+color.BOLD+color.GREEN+'Version 1.0\n'+color.END+\
	   color.BOLD+'•'+color.END+' Added the symbol table and the final code.\n'+\
	   color.BOLD+'•'+color.END+' Added two more optional arguments for'+\
	   							' renaming the symbol table and assembly files.\n'+\
	   color.BOLD+'•'+color.END+' Added semanticsError, aiming at presenting a new'+\
	   							' kind of error.\n'+\
	   color.BOLD+'•'+color.END+' Added printing of the symbol table to an output file.'+\
	   							' The user can choose the file\'s name (optional).\n'+\
	   color.BOLD+'•'+color.END+' Added printing of the final code to an output file.'+\
	   							' The user can choose the file\'s name (optional).\n'+\
	   color.BOLD+'•'+color.END+' Added intemediate-to-assembly code convertion.\n'+\
	   color.BOLD+'•'+color.END+' Also added pretty colors and improved slightly the error'+\
	   							' printing system. (still work to do)\n\n'+\
	   color.BOLD+color.RED+'{:-^100}'.format('END')+color.END+'\n'
		

class printInfo(argparse.Action):

	def __init__(self, option_strings, dest='==SUPPRESS==',\
				 default='==SUPPRESS==',help="show program's version number and exit"):
		super(printInfo, self).__init__(option_strings=option_strings,\
										dest= dest, default=default,nargs=0,help=help)

	def __call__(self, parser, args, values, option_string=None):
		print(info)
		parser.exit()

#-------------------------------------------#
#-------------------------------------------#
#-------------------Main--------------------#
#-------------------------------------------#
#-------------------------------------------#

def main():
	argParser = argparse.ArgumentParser(description="Compiler for Early Experimental Language")
	argParser.add_argument('--version','-v', action = 'version',\
						   version =color.BOLD+color.GREEN+'EEL Compiler v.'+color.END+color.BOLD+color.PURPLE+'{version} '.format(version =__version__)+color.END)
	argParser.add_argument('--info','-i',action = printInfo,\
						   help = 'show info about the version, compiler usage and exit')
	argParser.add_argument('inputF', metavar = '<inputFileName>', help = 'the .eel input file')
	argParser.add_argument('--interm','-int',metavar='', nargs = "?", help = 'intermediate code\'s output file name')
	argParser.add_argument('-c',metavar='', nargs='?', help = 'c code\'s output file name')
	argParser.add_argument('-sym',metavar='', nargs='?', help = 'final code\'s output file name')
	argParser.add_argument('-asm',metavar='', nargs='?', help = 'final code\'s output file name')
	args = vars(argParser.parse_args())
	# Open files
	open_files(args['inputF'],args['interm'],args['c'],args['sym'],args['asm'])
	
	print('\t'+color.DARKCYAN+color.BOLD+"Generating Files                   "+color.END,end="",flush=True)
	print(color.GREEN+color.BOLD+"["+color.END+color.YELLOW+color.BOLD+" WRITING  "+color.END+color.GREEN+color.BOLD+"]"+color.END,end='\r')
	time.sleep(1)
	# Print code to files
	printIntermediateCode()
	printCequivCode()
	
	print('\t'+color.DARKCYAN+color.BOLD+"Generating Files                   "+color.END,end="",flush=True)
	print(color.GREEN+color.BOLD+"["+color.END+color.GREEN+color.BOLD+"SUCCESSFUL"+color.END+color.GREEN+color.BOLD+"]"+color.END)
	time.sleep(1)
	#printQuads()
	print('\t'+color.DARKCYAN+color.BOLD+"Closing Files                      "+color.END,end="",flush=True)
	print(color.GREEN+color.BOLD+"["+color.END+color.YELLOW+color.BOLD+" CLOSING  "+color.END+color.GREEN+color.BOLD+"]"+color.END,end='\r')
	# Close files
	close_files()
	
	time.sleep(1)
	if(infile.closed and intoutfile.closed and coutfile.closed and symtablefile.closed and asmfile.closed):
		print('\t'+color.DARKCYAN+color.BOLD+"Closing Files                      "+color.END,end="",flush=True)
		print(color.GREEN+color.BOLD+"["+color.END+color.GREEN+color.BOLD+"SUCCESSFUL"+color.END+color.GREEN+color.BOLD+"]"+color.END)
	else:
		print('\t'+color.DARKCYAN+color.BOLD+"Closing Files                      "+color.END,end="",flush=True)
		print(color.GREEN+color.BOLD+"["+color.END+color.RED+color.BOLD+"  ERROR   "+color.END+color.GREEN+color.BOLD+"]"+color.END)
	
	print(color.RED+color.BOLD+'\t-----------------------------------------------'+color.END)
	print('\t'+color.BOLD+"Type	   "+color.YELLOW+"'./Compiler.py -h'    "+color.END+color.BOLD+"for more help!"+color.END)
	#printScopes()


if __name__ == "__main__":
	main()