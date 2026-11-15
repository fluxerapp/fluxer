// SPDX-License-Identifier: AGPL-3.0-or-later

import {writeFileSync} from 'node:fs';
import {dirname, join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';

const UNICODE_VERSION = '17.0.0';
const SOURCE = `https://www.unicode.org/Public/${UNICODE_VERSION}/security`;
const OUTPUT = join(
	resolve(dirname(fileURLToPath(import.meta.url)), '..'),
	'src/api/operator_pass/OperatorPassNameUnicodeData.ts',
);
const RESTRICTED_TYPES = new Set(['Technical', 'Obsolete', 'Deprecated', 'Not_NFKC', 'Not_XID', 'Default_Ignorable']);

async function fetchRows(file) {
	const response = await fetch(`${SOURCE}/${file}`);
	if (!response.ok) {
		throw new Error(`${file}: HTTP ${response.status}`);
	}
	const text = await response.text();
	if (!text.includes(`Version: ${UNICODE_VERSION}`)) {
		throw new Error(`${file} is not version ${UNICODE_VERSION}`);
	}
	return text
		.split('\n')
		.map((line) => line.split('#')[0].trim())
		.filter((line) => line.length > 0)
		.map((line) => line.split(';').map((field) => field.trim()));
}

function wrap(values, indent = '\t') {
	const lines = [];
	let line = '';
	for (const value of values) {
		const next = `${value},`;
		if (line.length + next.length + 1 > 110) {
			lines.push(line);
			line = '';
		}
		line = line.length === 0 ? next : `${line} ${next}`;
	}
	if (line.length > 0) {
		lines.push(line);
	}
	return lines.map((entry) => `${indent}${entry}`).join('\n');
}

const restrictedRows = (await fetchRows('IdentifierType.txt'))
	.filter(([, types]) => types.split(/\s+/).some((type) => RESTRICTED_TYPES.has(type)))
	.map(([range]) => range.split('..').map((hex) => Number.parseInt(hex, 16)))
	.sort((left, right) => left[0] - right[0]);
const restricted = [];
for (const [start, end = start] of restrictedRows) {
	const last = restricted.at(-1);
	if (last && last[1] + 1 === start) {
		last[1] = end;
	} else {
		restricted.push([start, end]);
	}
}

const confusables = [];
for (const [source, target] of await fetchRows('confusables.txt')) {
	const prototype = target
		.split(/\s+/)
		.map((hex) => String.fromCodePoint(Number.parseInt(hex, 16)))
		.join('')
		.normalize('NFD')
		.replace(/\p{M}+/gu, '')
		.toLowerCase();
	if (/^[a-z0-9]+$/.test(prototype)) {
		confusables.push([Number.parseInt(source, 16), prototype]);
	}
}
confusables.sort((left, right) => left[0] - right[0]);

const hex = (value) => `0x${value.toString(16)}`;
writeFileSync(
	OUTPUT,
	[
		'// SPDX-License-Identifier: AGPL-3.0-or-later',
		'',
		'export const RESTRICTED_IDENTIFIER_RANGES: ReadonlyArray<number> = [',
		wrap(restricted.flatMap(([start, end]) => [hex(start), hex(end)])),
		'];',
		'',
		'export const CONFUSABLE_SOURCES: ReadonlyArray<number> = [',
		wrap(confusables.map(([source]) => hex(source))),
		'];',
		'',
		'export const CONFUSABLE_PROTOTYPES: ReadonlyArray<string> = [',
		wrap(confusables.map(([, prototype]) => `'${prototype}'`)),
		'];',
		'',
	].join('\n'),
);
console.log(`${restricted.length} restricted ranges, ${confusables.length} confusables`);
