package repl

import (
	"bufio"
	"fmt"
	"io"
	"mono/lexer"
	"mono/token"
)

// TODO: add version as python does
const PROMPT = "(mono) >>"

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
		// Lex that line from stdin
		l := lexer.New(line)

		// Parse til EOF
		for tok := l.NextToken(); tok.Type != token.EOF; tok = l.NextToken() {
			// Print the token
			fmt.Fprintf(out, "%+v\n", tok)
		}
	}
}
