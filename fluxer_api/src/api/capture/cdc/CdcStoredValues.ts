// SPDX-License-Identifier: AGPL-3.0-or-later

type Parsed = null | {num: string} | string | Array<Parsed> | {obj: Array<[string, Parsed]>};

const SIMPLE_ESCAPES: Readonly<Record<string, string>> = {
	n: '\n',
	t: '\t',
	r: '\r',
	b: '\b',
	f: '\f',
	'0': '\0',
	'\\': '\\',
	"'": "'",
	'"': '"',
	'/': '/',
};

const HEX_ESCAPE_WIDTHS: Readonly<Record<string, number>> = {x: 2, u: 4, U: 8};

const LIST_CALLS = new Set(['SortedSet', 'set', 'frozenset', 'list', 'tuple']);
const MAP_CALLS = new Set(['OrderedMapSerializedKey', 'OrderedMap', 'dict']);

class ReprParser {
	private readonly text: string;
	private at = 0;

	constructor(text: string) {
		this.text = text;
	}

	parseAll(): Parsed {
		const value = this.value();
		this.space();
		if (this.at !== this.text.length) this.fail('trailing text');
		return value;
	}

	private fail(reason: string): never {
		throw new Error(`cannot parse stored value at ${this.at}: ${reason}`);
	}

	private space(): void {
		while (/\s/.test(this.text[this.at] ?? '')) this.at++;
	}

	private take(token: string): boolean {
		this.space();
		if (!this.text.startsWith(token, this.at)) return false;
		this.at += token.length;
		return true;
	}

	private expect(token: string): void {
		if (!this.take(token)) this.fail(`expected ${token}`);
	}

	private value(): Parsed {
		this.space();
		const char = this.text[this.at] ?? '';
		if (char === '"' || char === "'") return this.string(char);
		if (char === '[') return this.sequence('[', ']');
		if (char === '(') return this.sequence('(', ')');
		if (char === '{') return this.braces();
		if (/[-+\d.]/.test(char)) return this.number();
		const name = /^[A-Za-z_][\w.]*/.exec(this.text.slice(this.at))?.[0];
		if (name === undefined) this.fail('unexpected character');
		this.at += name.length;
		if (name === 'None' || name === 'null') return null;
		if (name === 'True' || name === 'true') return {num: '1'};
		if (name === 'False' || name === 'false') return {num: '0'};
		if (name === 'NaN' || name === 'Infinity') return {num: name};
		return this.call(name.split('.').pop() ?? name);
	}

	private number(): Parsed {
		const text = /^[-+]?(?:\d+\.?\d*(?:[eE][-+]?\d+)?|\.\d+(?:[eE][-+]?\d+)?|Infinity)/.exec(
			this.text.slice(this.at),
		)?.[0];
		if (text === undefined) this.fail('bad number');
		this.at += text.length;
		return {num: /^-?\d+$/.test(text) ? text : String(Number(text))};
	}

	private string(quote: string): string {
		let out = '';
		this.at++;
		for (;;) {
			const char = this.text[this.at++];
			if (char === undefined) this.fail('unterminated string');
			if (char === quote) return out;
			if (char !== '\\') {
				out += char;
				continue;
			}
			const code = this.text[this.at++] ?? '';
			const width = HEX_ESCAPE_WIDTHS[code];
			if (width !== undefined) {
				out += String.fromCodePoint(Number.parseInt(this.text.slice(this.at, this.at + width), 16));
				this.at += width;
			} else {
				out += SIMPLE_ESCAPES[code] ?? `\\${code}`;
			}
		}
	}

	private items(close: string, item: () => void): void {
		while (!this.take(close)) {
			item();
			if (!this.take(',')) {
				this.expect(close);
				return;
			}
		}
	}

	private sequence(open: string, close: string): Array<Parsed> {
		this.expect(open);
		const values: Array<Parsed> = [];
		this.items(close, () => values.push(this.value()));
		return values;
	}

	private braces(): Parsed {
		this.expect('{');
		const values: Array<Parsed> = [];
		const entries: Array<[string, Parsed]> = [];
		this.items('}', () => {
			const value = this.value();
			if (this.take(':')) entries.push([keyText(value), this.value()]);
			else values.push(value);
		});
		return values.length > 0 ? values : {obj: entries};
	}

	private call(name: string): Parsed {
		this.expect('(');
		const positional: Array<Parsed> = [];
		const keywords: Array<[string, Parsed]> = [];
		this.items(')', () => {
			const field = /^\s*([A-Za-z_]\w*)\s*=(?!=)/.exec(this.text.slice(this.at));
			if (field?.[1] !== undefined) {
				this.at += field[0].length;
				keywords.push([field[1], this.value()]);
			} else {
				positional.push(this.value());
			}
		});
		if (keywords.length > 0) return {obj: keywords};
		const [first] = positional;
		if (LIST_CALLS.has(name)) return Array.isArray(first) ? first : [];
		if (MAP_CALLS.has(name)) {
			if (first !== undefined && first !== null && typeof first === 'object' && 'obj' in first) return first;
			const pairs = Array.isArray(first) ? first : [];
			return {
				obj: pairs.map((pair) => (Array.isArray(pair) ? [keyText(pair[0] ?? null), pair[1] ?? null] : ['', pair])),
			};
		}
		if (name === 'datetime') return datetimeText(positional);
		if (name === 'date') return datetimeText(positional).slice(0, 10);
		return positional.length === 1 && typeof first === 'string' ? first : positional;
	}
}

function numberAt(values: ReadonlyArray<Parsed>, index: number): number {
	const value = values[index];
	return value !== null && typeof value === 'object' && 'num' in value ? Number(value.num) : 0;
}

function datetimeText(parts: ReadonlyArray<Parsed>): string {
	const ms = Date.UTC(
		numberAt(parts, 0),
		numberAt(parts, 1) - 1,
		numberAt(parts, 2),
		numberAt(parts, 3),
		numberAt(parts, 4),
		numberAt(parts, 5),
		Math.floor(numberAt(parts, 6) / 1000),
	);
	return new Date(ms).toISOString().slice(0, 23).replace('T', ' ');
}

function keyText(value: Parsed): string {
	return typeof value === 'string' ? value : canonical(value);
}

function canonical(value: Parsed): string {
	if (value === null) return 'null';
	if (typeof value === 'string') return JSON.stringify(value);
	if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
	if ('num' in value) return value.num;
	const entries = [...value.obj].sort(([a], [b]) => Number(a > b) - Number(a < b));
	return `{${entries.map(([key, entry]) => `${JSON.stringify(key)}:${canonical(entry)}`).join(',')}}`;
}

export function canonicalStoredValue(text: string): string {
	if (text === '') return 'null';
	return canonical(new ReprParser(text).parseAll());
}
