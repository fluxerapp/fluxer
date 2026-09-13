// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseMarkdownAstWithWasm} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import type {Node} from '@app/features/messaging/utils/markdown/parser/Nodes';

export class Parser {
	private readonly input: string;
	private readonly parserFlags: number;

	constructor(input: string, flags: number) {
		this.input = input;
		this.parserFlags = flags;
	}

	parse(): {
		nodes: Array<Node>;
	} {
		return parseMarkdownAstWithWasm(this.input, this.parserFlags);
	}
}
