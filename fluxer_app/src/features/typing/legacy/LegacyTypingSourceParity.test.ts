// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {readdirSync, readFileSync} from 'node:fs';
import {join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

const APP_SRC = fileURLToPath(new URL('../../../', import.meta.url));

const LEGACY_SPECIFIER_PAIRS: ReadonlyArray<readonly [relocated: string, head: string]> = [
	['@app/features/typing/legacy/LegacyTypingCommands', '@app/features/typing/commands/TypingCommands'],
	['@app/features/typing/legacy/LegacyTypingIndicator', '@app/features/typing/state/TypingIndicator'],
	['@app/features/typing/legacy/LegacyTypingUtils', '@app/features/typing/utils/TypingUtils'],
];

const HEAD_DIGESTS = {
	typingIndicator: 'b1e497ae31f83beff6d3c4cc42f2c103c599541c93aee9db9ba3abed9f316afb',
	typingUtils: '6bec349ce19186301bbe976acb82e0e4c1544e10cc848d5490d21d1379cb21e8',
	typingCommands: '12268312bfadf9d4721c72c05b660f964d3e038d424dcd3a5ee08f86ae58c5fc',
	composerTypingEffectBody: '3a4d3d2625a3bc32cfade4309d18b751685833047fb1d5e7d73ca655257b065a',
	localTypingStateMachine: 'cf56293c27bcb7bbb809efc28ff1070234a7a96e198c81f3c91f43169b1457a0',
	localTypingStateMachineTest: '90caf6ee8ac780f4b63031056c7e4eab3aeff6dcb759d1f5ba4b76fe08e56558',
} as const;

const HEAD_COMPOSER_TYPING_EFFECT_BODY = `\t\tif (!enabled || !typingEnabled) {
\t\t\tTypingUtils.clear(channelId);
\t\t\treturn;
\t\t}
\t\tconst content = value.trim();
\t\tconst isInReplaceMode = ReplaceCommandUtils.isReplaceCommand(content);
\t\tconst isSlashCommand = content.startsWith('/');
\t\tif (content && !isAutocompleteAttached && !isInReplaceMode && !isSlashCommand) {
\t\t\tTypingUtils.typing(channelId);
\t\t} else {
\t\t\tTypingUtils.clear(channelId);
\t\t}`;

const HEAD_TYPING_USERS_STYLESHEET = [
	'/* SPDX-License-Identifier: AGPL-3.0-or-later */',
	'',
	'.typing {',
	'\tmargin-right: 0;',
	'\tdisplay: flex;',
	'\talign-items: center;',
	'\tjustify-content: center;',
	'\tcolor: var(--text-primary-muted);',
	'}',
	'',
	'.username {',
	'\tfont-weight: 600;',
	'}',
	'',
	'.composerStatus {',
	'\tdisplay: inline-flex;',
	'\talign-items: center;',
	'\theight: var(--typing-pill-height, 1.125rem);',
	'\tmax-width: 100%;',
	'\tmin-width: 0;',
	'\tcolor: var(--text-primary-muted);',
	'\tpointer-events: auto;',
	'}',
	'',
].join('\n');

const TYPING_FACADE_SPECIFIERS = new Set([
	'@app/features/channel/components/TypingUsers',
	'@app/features/typing/commands/TypingCommands',
	'@app/features/typing/state/TypingIndicator',
	'@app/features/typing/state/TypingPolicy',
	'@app/features/typing/utils/TypingUtils',
]);

const IMPORT_DECLARATION = /^import [^;]+? from '([^']+)';\n/gm;
const COMPOSER_TYPING_SIGNATURE_END = '}: LegacyComposerTypingInput): void {\n';

function readSource(path: string): string {
	return readFileSync(join(APP_SRC, path), 'utf8');
}

function sha256(text: string): string {
	return createHash('sha256').update(text).digest('hex');
}

function sortImportDeclarations(source: string): string {
	const declarations = [...source.matchAll(IMPORT_DECLARATION)];
	if (declarations.length === 0) {
		return source;
	}
	const first = declarations[0]!;
	const last = declarations[declarations.length - 1]!;
	const blockStart = first.index;
	const blockEnd = last.index + last[0].length;
	expect(source.slice(blockStart, blockEnd)).toBe(declarations.map((declaration) => declaration[0]).join(''));
	const sorted = [...declarations].sort((left, right) => {
		if (left[1]! === right[1]!) return 0;
		return left[1]! < right[1]! ? -1 : 1;
	});
	return source.slice(0, blockStart) + sorted.map((declaration) => declaration[0]).join('') + source.slice(blockEnd);
}

function restoreHeadSource(source: string): string {
	let restored = source;
	for (const [relocated, head] of LEGACY_SPECIFIER_PAIRS) {
		restored = restored.replaceAll(`'${relocated}'`, `'${head}'`);
	}
	return sortImportDeclarations(restored);
}

function importSpecifiers(source: string): Array<string> {
	return [...source.matchAll(/ from '([^']+)'/g)].map((match) => match[1]!);
}

function extractComposerTypingEffectBody(source: string): string {
	const signatureEnd = source.indexOf(COMPOSER_TYPING_SIGNATURE_END);
	expect(signatureEnd).toBeGreaterThan(-1);
	const body = source.slice(signatureEnd + COMPOSER_TYPING_SIGNATURE_END.length, source.lastIndexOf('\n}\n'));
	return body
		.split('\n')
		.map((line) => `\t${line}`)
		.join('\n')
		.replaceAll('LegacyTypingUtils.', 'TypingUtils.');
}

describe('legacy typing source parity', () => {
	it('keeps the relocated typing store identical to the recorded head source', () => {
		const source = readSource('features/typing/legacy/LegacyTypingIndicator.ts');

		expect(sha256(source)).toBe(HEAD_DIGESTS.typingIndicator);
	});

	it('keeps the relocated typing utils identical to the recorded head source apart from its legacy import paths', () => {
		const source = readSource('features/typing/legacy/LegacyTypingUtils.ts');

		expect(importSpecifiers(source)).toEqual(
			expect.arrayContaining([
				'@app/features/typing/legacy/LegacyTypingCommands',
				'@app/features/typing/legacy/LegacyTypingIndicator',
				'@app/features/typing/state/LocalTypingStateMachine',
				'@app/features/auth/state/Authentication',
			]),
		);
		expect(sha256(restoreHeadSource(source))).toBe(HEAD_DIGESTS.typingUtils);
	});

	it('keeps the relocated typing commands identical to the recorded head source apart from its legacy import path', () => {
		const source = readSource('features/typing/legacy/LegacyTypingCommands.ts');

		expect(importSpecifiers(source)).toContain('@app/features/typing/legacy/LegacyTypingIndicator');
		expect(sha256(restoreHeadSource(source))).toBe(HEAD_DIGESTS.typingCommands);
	});

	it('keeps the extracted composer typing effect identical to the recorded head effect body', () => {
		const source = readSource('features/typing/legacy/LegacyComposerTyping.ts');

		expect(sha256(HEAD_COMPOSER_TYPING_EFFECT_BODY)).toBe(HEAD_DIGESTS.composerTypingEffectBody);
		expect(extractComposerTypingEffectBody(source)).toBe(HEAD_COMPOSER_TYPING_EFFECT_BODY);
	});

	it('keeps the local typing state machine and its test frozen in place', () => {
		expect(sha256(readSource('features/typing/state/LocalTypingStateMachine.ts'))).toBe(
			HEAD_DIGESTS.localTypingStateMachine,
		);
		expect(sha256(readSource('features/typing/state/LocalTypingStateMachine.test.ts'))).toBe(
			HEAD_DIGESTS.localTypingStateMachineTest,
		);
	});

	it('keeps the typing users stylesheet frozen in place', () => {
		expect(readSource('features/channel/components/TypingUsers.module.css')).toBe(HEAD_TYPING_USERS_STYLESHEET);
	});

	it('never imports a typing facade from a legacy module', () => {
		const legacyModules = [
			...readdirSync(join(APP_SRC, 'features/typing/legacy'))
				.filter((name) => !name.includes('.test.'))
				.map((name) => `features/typing/legacy/${name}`),
			'features/channel/components/LegacyTypingUsers.tsx',
		];
		expect(legacyModules).toHaveLength(5);

		const facadeImports = legacyModules.flatMap((path) =>
			importSpecifiers(readSource(path))
				.filter((specifier) => TYPING_FACADE_SPECIFIERS.has(specifier))
				.map((specifier) => `${path} imports ${specifier}`),
		);

		expect(facadeImports).toEqual([]);
	});
});
