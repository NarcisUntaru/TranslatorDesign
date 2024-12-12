package cup.example;
import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;
import java_cup.runtime.Symbol;
import java.lang.*;
import java.io.InputStreamReader;

%%

%class Lexer
%implements sym
%public
%caseless
%unicode
%line
%column
%cup
%char

%{
	StringBuilder string = new StringBuilder();
	
    public Lexer(ComplexSymbolFactory sf, java.io.InputStream is){
		this(is);
        symbolFactory = sf;
    }
	public Lexer(ComplexSymbolFactory sf, java.io.Reader reader){
		this(reader);
        symbolFactory = sf;
    }
    
    private StringBuffer sb;
    private ComplexSymbolFactory symbolFactory;
    private int csline,cscolumn;

    public Symbol symbol(String name, int code){
		return symbolFactory.newSymbol(name, code,
						new Location(yyline+1,yycolumn+1, yychar), // -yylength()
						new Location(yyline+1,yycolumn+yylength(), yychar+yylength())
				);
    }
    public Symbol symbol(String name, int code, String lexem){
	return symbolFactory.newSymbol(name, code, 
						new Location(yyline+1, yycolumn +1, yychar), 
						new Location(yyline+1,yycolumn+yylength(), yychar+yylength()), lexem);
    }
    
    protected void emit_warning(String message){
    	System.out.println("scanner warning: " + message + " at : 2 "+ 
    			(yyline+1) + " " + (yycolumn+1) + " " + yychar);
    }
    
    protected void emit_error(String message){
    	System.out.println("scanner error: " + message + " at : 2" + 
    			(yyline+1) + " " + (yycolumn+1) + " " + yychar);
    }
    
%}

/* main character classes */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment} | 
          {DocumentationComment}

TraditionalComment = "/*" [^*] ~"*/" | "/*" "*"+ "/"
EndOfLineComment = "//" {InputCharacter}* {LineTerminator}?
DocumentationComment = "/*" "*"+ [^/*] ~"*/"

/* identifiers */
Identifier = [:jletter:][:jletterdigit:]*

/* integer literals */
DecIntegerLiteral = 0 | [1-9][0-9]*
DecLongLiteral    = {DecIntegerLiteral} [lL]

HexIntegerLiteral = 0 [xX] 0* {HexDigit} {1,8}
HexLongLiteral    = 0 [xX] 0* {HexDigit} {1,16} [lL]
HexDigit          = [0-9a-fA-F]

OctIntegerLiteral = 0+ [1-3]? {OctDigit} {1,15}
OctLongLiteral    = 0+ 1? {OctDigit} {1,21} [lL]
OctDigit          = [0-7]
    
/* floating point literals */        
FloatLiteral  = ({FLit1}|{FLit2}|{FLit3}) {Exponent}? [fF]
DoubleLiteral = ({FLit1}|{FLit2}|{FLit3}) {Exponent}?

FLit1    = [0-9]+ \. [0-9]* 
FLit2    = \. [0-9]+ 
FLit3    = [0-9]+ 
Exponent = [eE] [+-]? [0-9]+

/* string and character literals */
StringCharacter = [^\r\n\"\\]
CharactersList = {StringCharacter}+
SingleCharacter = [^\r\n\'\\]
Newline    = \r | \n | \r\n
Whitespace = [ \t\f] | {Newline}

%%  
<YYINITIAL> {
	[ \t\n\r]+           	{ /* ignore whitespace */ }
  {Whitespace} 		    {						  }
  "<"                              { return symbolFactory.newSymbol("START_TAG", START_TAG); }
  ">"                              { return symbolFactory.newSymbol("END_TAG", END_TAG); }
  "</"                             { return symbolFactory.newSymbol("CLOSE_TAG", CLOSE_TAG); }
  "H1"                             { return symbolFactory.newSymbol("H1", H1); }
  "H2"                             { return symbolFactory.newSymbol("H2", H2); }
  "H3"                             { return symbolFactory.newSymbol("H3", H3); }
  "H4"                             { return symbolFactory.newSymbol("H4", H4); }
  "H5"                             { return symbolFactory.newSymbol("H5", H5); }
  "H6"                             { return symbolFactory.newSymbol("H6", H6); }
  "UL"                             { return symbolFactory.newSymbol("UL", UL); }
  "LI"                             { return symbolFactory.newSymbol("LI", LI); }
  "TABLE"                          { return symbolFactory.newSymbol("TABLE", TABLE); }
  "TR"                             { return symbolFactory.newSymbol("TR", TR); }
  "A"                              { return symbolFactory.newSymbol("A", A); }
  "ABBE"                           { return symbolFactory.newSymbol("ABBRTAG", ABBRTAG); }
  "ACRONYM"                        { return symbolFactory.newSymbol("ACRONYM", ACRONYM); }
  "ADDRESS"                        { return symbolFactory.newSymbol("ADDRESS", ADDRESS); }
  "APPLET"                         { return symbolFactory.newSymbol("APPLET", APPLET); }
  "B"                              { return symbolFactory.newSymbol("B", B); }
  "BASEFONT"                       { return symbolFactory.newSymbol("BASEFONT", BASEFONT); }
  "BDO"                            { return symbolFactory.newSymbol("BDO", BDO); }
  "BIG"                            { return symbolFactory.newSymbol("BIG", BIG); }
  "BLINK"                          { return symbolFactory.newSymbol("BLINK", BLINK); }
  "BLOCKQUOTE"                     { return symbolFactory.newSymbol("BLOCKQUOTE", BLOCKQUOTE); }
  "BODY"                           { return symbolFactory.newSymbol("BODY", BODY); }
  "CAPTION"                        { return symbolFactory.newSymbol("CAPTION", CAPTION); }
  "CENTER"                         { return symbolFactory.newSymbol("CENTER", CENTER); }
  "CITE"                           { return symbolFactory.newSymbol("CITE", CITE); }
  "COLGROUP"                       { return symbolFactory.newSymbol("COLGROUP", COLGROUP); }
  "DD"                             { return symbolFactory.newSymbol("DD", DD); }
  "DFN"                            { return symbolFactory.newSymbol("DFN", DFN); }
  "DIR"                            { return symbolFactory.newSymbol("DIR", DIR); }
  "DIV"                            { return symbolFactory.newSymbol("DIV", DIV); }
  "DL"                             { return symbolFactory.newSymbol("DL", DL); }
  "DT"                             { return symbolFactory.newSymbol("DT", DT); }
  "EM"                             { return symbolFactory.newSymbol("EM", EM); }
  "FIELDSET"                       { return symbolFactory.newSymbol("FIELDSET", FIELDSET); }
  "FONT"                           { return symbolFactory.newSymbol("FONT", FONT); }
  "FORM"                           { return symbolFactory.newSymbol("FORM", FORM); }
  "FRAMESET"                       { return symbolFactory.newSymbol("FRAMESET", FRAMESET); }
  "HEAD"                           { return symbolFactory.newSymbol("HEAD", HEAD); }
  "HTML"                       		{ return symbolFactory.newSymbol("HTML_TAG", HTML_TAG); }
  "ISINDEX"                        { return symbolFactory.newSymbol("ISINDEX", ISINDEX); }
  "I"                              { return symbolFactory.newSymbol("I", I); }
  "ILAYER"                         { return symbolFactory.newSymbol("ILAYER", ILAYER); }
  "INS"                            { return symbolFactory.newSymbol("INS", INS); }
  "KBD"                            { return symbolFactory.newSymbol("KBD", KBD); }
  "LABEL"                          { return symbolFactory.newSymbol("LABEL", LABEL); }
  "LAYER"                          { return symbolFactory.newSymbol("LAYER", LAYER); }
  "LEGEND"                         { return symbolFactory.newSymbol("LEGEND", LEGEND); }
  "LISTING"                        { return symbolFactory.newSymbol("LISTING", LISTING); }
  "MAP"                            { return symbolFactory.newSymbol("MAP", MAP); }
  "MARQUEE"                        { return symbolFactory.newSymbol("MARQUEE", MARQUEE); }
  "MENU"                           { return symbolFactory.newSymbol("MENU", MENU); }
  "MULTICOL"                       { return symbolFactory.newSymbol("MULTICOL", MULTICOL); }
  "NOBR"                           { return symbolFactory.newSymbol("NOBR", NOBR); }
  "NEXTID"                         { return symbolFactory.newSymbol("NEXTID", NEXTID); }
  "NOEMBED"                        { return symbolFactory.newSymbol("NOEMBED", NOEMBED); }
  "NOFRAMES"                       { return symbolFactory.newSymbol("NOFRAMES", NOFRAMES); }
  "NOSCRIPT"                       { return symbolFactory.newSymbol("NOSCRIPT", NOSCRIPT); }
  "OBJECT"                         { return symbolFactory.newSymbol("OBJECT", OBJECT); }
  "OL"                             { return symbolFactory.newSymbol("OL", OL); }
  "OPTGROUP"                       { return symbolFactory.newSymbol("OPTGROUP", OPTGROUP); }
  "OPTION"                         { return symbolFactory.newSymbol("OPTION", OPTION); }
  "P"                              { return symbolFactory.newSymbol("P", P); }
  "PRE"                            { return symbolFactory.newSymbol("PRE", PRE); }
  "Q"                              { return symbolFactory.newSymbol("Q", Q); }
  "S"                              { return symbolFactory.newSymbol("S", S); }
  "SAMP"                           { return symbolFactory.newSymbol("SAMP", SAMP); }
  "SCRIPT"                         { return symbolFactory.newSymbol("SCRIPT", SCRIPT); }
  "SELECT"                         { return symbolFactory.newSymbol("SELECT", SELECT); }
  "SEVER"                          { return symbolFactory.newSymbol("SEVER", SEVER); }
  "SMALL"                          { return symbolFactory.newSymbol("SMALL", SMALL); }
  "SPAN"                           { return symbolFactory.newSymbol("SPAN", SPAN); }
  "STRIKE"                         { return symbolFactory.newSymbol("STRIKE", STRIKE); }
  "STRONG"                         { return symbolFactory.newSymbol("STRONG", STRONG); }
  "STYLE"                          { return symbolFactory.newSymbol("STYLE", STYLE); }
  "SUB"                            { return symbolFactory.newSymbol("SUB", SUB); }
  "SUP"                            { return symbolFactory.newSymbol("SUP", SUP); }
  "TD"                             { return symbolFactory.newSymbol("TD", TD); }
  "TEXTAREA"                       { return symbolFactory.newSymbol("TEXTAREA", TEXTAREA); }
  "TH"                             { return symbolFactory.newSymbol("TH", TH); }
  "TITLE"                          { return symbolFactory.newSymbol("TITLE", TITLE); }
  "IMG"                            { return symbolFactory.newSymbol("IMG", IMG); }
  "WBR"                            { return symbolFactory.newSymbol("WBR", WBR); }
  "ABBR"                           { return symbolFactory.newSymbol("ABBR", ABBR); }
  "CODE"                           { return symbolFactory.newSymbol("CODE", CODE); }
  "TR"                             { return symbolFactory.newSymbol("TR", TR); }
  "TT"                             { return symbolFactory.newSymbol("TT", TT); }
  "HR"                             { return symbolFactory.newSymbol("HR", HR); }
  "BR"                             { return symbolFactory.newSymbol("BR", BR); }
  "U"                              { return symbolFactory.newSymbol("U", U); }
  "VAR"                            { return symbolFactory.newSymbol("VAR", VAR); }
  "XMP"                            { return symbolFactory.newSymbol("XMP", XMP); }
  "BGSOUND"                        { return symbolFactory.newSymbol("BGSOUND", BGSOUND); }
  
		}			
	
		/* identifiers */ 
// error fallback
.|\n          { emit_warning("Unrecognized character '" +yytext()+"' -- ignored"); }
 {CharactersList}             { return symbolFactory.newSymbol("TEXT_CONTENT", TEXT_CONTENT, yytext()); }
