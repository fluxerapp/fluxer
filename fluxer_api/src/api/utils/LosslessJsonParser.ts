// SPDX-License-Identifier: AGPL-3.0-or-later

const MAX_SAFE_INTEGER_DECIMAL = Number.MAX_SAFE_INTEGER.toString();
const UNSAFE_INTEGER_DIGIT_RUN = new RegExp(String.raw`\d{${MAX_SAFE_INTEGER_DECIMAL.length}}`);

function isDigit(char: string): boolean {
	return char >= '0' && char <= '9';
}

function isValidJsonIntegerToken(token: string): boolean {
	if (!/^-?\d+$/.test(token)) return false;
	if (token === '0' || token === '-0') return true;
	const digits = token[0] === '-' ? token.slice(1) : token;
	return digits.length > 0 && digits[0] !== '0';
}

function isUnsafeIntegerToken(token: string): boolean {
	const digits = token[0] === '-' ? token.slice(1) : token;
	if (digits === '0') return false;
	if (digits.length < MAX_SAFE_INTEGER_DECIMAL.length) return false;
	if (digits.length > MAX_SAFE_INTEGER_DECIMAL.length) return true;
	return digits > MAX_SAFE_INTEGER_DECIMAL;
}

export function coerceUnsafeIntegersToStrings(jsonText: string): string {
	let inString = false;
	let escaped = false;
	let i = 0;
	let lastCopyIndex = 0;
	let outputParts: Array<string> | null = null;
	while (i < jsonText.length) {
		const char = jsonText[i]!;
		if (inString) {
			if (escaped) {
				escaped = false;
			} else if (char === '\\') {
				escaped = true;
			} else if (char === '"') {
				inString = false;
			}
			i++;
			continue;
		}
		if (char === '"') {
			inString = true;
			i++;
			continue;
		}
		if (char === '-' || isDigit(char)) {
			const start = i;
			i++;
			while (i < jsonText.length) {
				const c = jsonText[i]!;
				if (isDigit(c) || c === '.' || c === 'e' || c === 'E' || c === '+' || c === '-') {
					i++;
					continue;
				}
				break;
			}
			const token = jsonText.slice(start, i);
			if (isValidJsonIntegerToken(token) && isUnsafeIntegerToken(token)) {
				if (!outputParts) {
					outputParts = [];
				}
				outputParts.push(jsonText.slice(lastCopyIndex, start), `"${token}"`);
				lastCopyIndex = i;
			}
			continue;
		}
		i++;
	}
	if (!outputParts) {
		return jsonText;
	}
	outputParts.push(jsonText.slice(lastCopyIndex));
	return outputParts.join('');
}

export function parseJsonPreservingLargeIntegers(jsonText: string): unknown {
	const processed = UNSAFE_INTEGER_DIGIT_RUN.test(jsonText) ? coerceUnsafeIntegersToStrings(jsonText) : jsonText;
	const parsed: unknown = JSON.parse(processed);
	return parsed;
}
