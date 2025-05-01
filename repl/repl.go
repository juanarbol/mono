package repl

import (
	"bufio"
	"fmt"
	"io"
	"mono/evaluator"
	"mono/lexer"
	"mono/parser"
)

// TODO: add version as python does
const PROMPT = "(mono, write '.exit' to exit) >> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Fprintf(out, PROMPT)
		scanned := scanner.Scan()

		// Whoops
		if !scanned {
			return
		}

		// Read line from stdin
		line := scanner.Text()
		if line == ".exit" {
			break
		}
		// Lex that line from stdin
		l := lexer.New(line)
		p := parser.New(l)

		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			printParserErrors(out, p.Errors())
			continue
		}

		evaluated := evaluator.Eval(program)
		if evaluated != nil {
			io.WriteString(out, evaluated.Inspect())
			io.WriteString(out, "\n")
		}
	}
}

func printParserErrors(out io.Writer, errors []string) {
	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
