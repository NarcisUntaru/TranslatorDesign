package cup.example;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;

import java_cup.runtime.*;

class Driver {

	public static void main(String[] args) throws Exception {
	/*	Parser parser = new Parser();
		parser.parse(); */
        ComplexSymbolFactory symbolFactory = new ComplexSymbolFactory();

        FileInputStream fis = new FileInputStream("input.txt");
        Lexer lexer = new Lexer(symbolFactory, new InputStreamReader(fis));

        Symbol currentSymbol;
        
        try {
            while ((currentSymbol = lexer.next_token()).sym != sym.EOF) {
                System.out.println("Symbol: " + sym.terminalNames[currentSymbol.sym]);
                
                if (currentSymbol.value != null) {
                    System.out.println("Value: " + currentSymbol.value);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        System.out.println("End of File (EOF) reached.");
    }
	
}