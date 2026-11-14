// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import {createRequire} from 'node:module';
import * as path from 'node:path';
import {fileURLToPath} from 'node:url';

interface MjmlError {
	formattedMessage: string;
}

interface MjmlResult {
	html: string;
	errors: Array<MjmlError>;
}

type Mjml2Html = (input: string, options: Record<string, unknown>) => Promise<MjmlResult>;

interface MjmlCoreModule {
	default: Mjml2Html;
	components: Record<string, unknown>;
	assignComponents(target: Record<string, unknown>, source: Array<unknown>): void;
}

interface MjmlValidatorModule {
	dependencies: Record<string, unknown>;
	assignDependencies(target: Record<string, unknown>, source: Record<string, unknown>): void;
}

interface MjmlPreset {
	components: Array<unknown>;
	dependencies: Record<string, unknown>;
}

const require = createRequire(import.meta.url);
const mjmlCore = require('mjml-core') as MjmlCoreModule;
const mjmlValidator = require('mjml-validator') as MjmlValidatorModule;
const presetCore = require('mjml-preset-core') as MjmlPreset;
mjmlCore.assignComponents(mjmlCore.components, presetCore.components);
mjmlValidator.assignDependencies(mjmlValidator.dependencies, presetCore.dependencies);

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const PACKAGE_ROOT = path.resolve(__dirname, '..');
const LAYOUTS_DIR = path.join(PACKAGE_ROOT, 'src/email_html/layouts');
const OUTPUT_FILE = path.join(PACKAGE_ROOT, 'src/email_html/EmailHtmlLayouts.generated.ts');
const GENERATED_HEADER = '// SPDX-License-Identifier: AGPL-3.0-or-later\n\n';
const INCLUDE_PATTERN = /<mj-include path="\.\/partials\/([a-z-]+)\.mjml"((?:\s+[a-z_]+="[a-z0-9_#-]*")*)\s*\/>/g;
const INCLUDE_PARAM_PATTERN = /([a-z_]+)="([a-z0-9_#-]*)"/g;
const SLOT_PATTERN = /%%([a-z0-9_]+)%%/g;
const BLOCK_MARKER_PATTERN = /%%([?/])([a-z0-9_]+)%%/g;
const MAX_STYLE_BYTES = 16_000;

type Direction = 'ltr' | 'rtl';

const FONT_TOKENS: Record<string, string> = {
	font: "-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,'Noto Sans',sans-serif",
	mono: "ui-monospace,SFMono-Regular,Menlo,Consolas,'Liberation Mono',monospace",
};

const DIRECTION_TOKENS: Record<Direction, Record<string, string>> = {
	ltr: {dir: 'ltr', start: 'left', end: 'right'},
	rtl: {dir: 'rtl', start: 'right', end: 'left'},
};

function resolveIncludes(source: string, depth = 0): string {
	if (depth > 4) {
		throw new Error('email layout includes nest too deeply');
	}
	return source.replace(INCLUDE_PATTERN, (_match, name: string, params: string) => {
		let partial = fs.readFileSync(path.join(LAYOUTS_DIR, 'partials', `${name}.mjml`), 'utf8');
		for (const [, key, value] of params.matchAll(INCLUDE_PARAM_PATTERN)) {
			partial = partial.replaceAll(`[[${key}]]`, value);
		}
		return resolveIncludes(partial, depth + 1);
	});
}

function applyDirection(source: string, direction: Direction): string {
	const tokens = {...FONT_TOKENS, ...DIRECTION_TOKENS[direction]};
	return source.replace(/\[\[([a-z_]+)\]\]/g, (match, name: string) => {
		const value = tokens[name];
		if (value === undefined) {
			throw new Error(`unknown layout token ${match}`);
		}
		return value;
	});
}

function assertStyleBudget(name: string, html: string): void {
	for (const match of html.matchAll(/<style[^>]*>([\s\S]*?)<\/style>/g)) {
		const bytes = Buffer.byteLength(match[1], 'utf8');
		if (bytes > MAX_STYLE_BYTES) {
			throw new Error(`${name} has a <style> block of ${bytes} bytes, over the Gmail budget`);
		}
		if (match[1].includes('/*')) {
			throw new Error(`${name} has a CSS comment, which Gmail treats as a parse error`);
		}
	}
}

async function compileLayout(name: string, direction: Direction): Promise<string> {
	const source = applyDirection(
		resolveIncludes(fs.readFileSync(path.join(LAYOUTS_DIR, `${name}.mjml`), 'utf8')),
		direction,
	);
	const result = await mjmlCore.default(source, {
		validationLevel: 'strict',
		minify: true,
		keepComments: false,
		fonts: {},
		filePath: LAYOUTS_DIR,
		ignoreIncludes: true,
	});
	if (result.errors.length > 0) {
		throw new Error(
			`${name} (${direction}) failed MJML validation:\n${result.errors.map((error) => error.formattedMessage).join('\n')}`,
		);
	}
	const html = result.html.trim();
	if (html.includes('[[')) {
		throw new Error(`${name} (${direction}) has an unresolved layout token`);
	}
	if (!html.includes('mso-padding-alt:12px 22px')) {
		throw new Error(`${name} (${direction}) lost its Outlook button padding`);
	}
	assertStyleBudget(`${name} (${direction})`, html);
	assertBlocks(`${name} (${direction})`, html);
	return html;
}

function assertBlocks(name: string, html: string): void {
	const open: Array<string> = [];
	for (const [, kind, slot] of html.matchAll(BLOCK_MARKER_PATTERN)) {
		if (kind === '?') {
			if (open.includes(slot)) {
				throw new Error(`${name} opens the block ${slot} twice`);
			}
			open.push(slot);
		} else if (open.pop() !== slot) {
			throw new Error(`${name} closes the block ${slot} out of order`);
		}
	}
	if (open.length > 0) {
		throw new Error(`${name} leaves the block ${open.join(', ')} open`);
	}
	if (/="[^"]*%%[?/][a-z0-9_]+%%/.test(html)) {
		throw new Error(`${name} has a block marker inside an attribute`);
	}
}

function collectSlots(html: string): Array<string> {
	return [...new Set([...html.matchAll(SLOT_PATTERN)].map((match) => match[1]))].sort();
}

async function main(): Promise<void> {
	const names = fs
		.readdirSync(LAYOUTS_DIR)
		.filter((file) => file.endsWith('.mjml'))
		.map((file) => file.slice(0, -'.mjml'.length))
		.sort();
	const entries: Array<string> = [];
	for (const name of names) {
		const ltr = await compileLayout(name, 'ltr');
		const rtl = await compileLayout(name, 'rtl');
		const slots = collectSlots(ltr);
		if (JSON.stringify(slots) !== JSON.stringify(collectSlots(rtl))) {
			throw new Error(`${name} has different slots in its ltr and rtl variants`);
		}
		entries.push(
			`\t${name}: {\n\t\tslots: ${JSON.stringify(slots)},\n\t\tltr: ${JSON.stringify(ltr)},\n\t\trtl: ${JSON.stringify(rtl)},\n\t},`,
		);
	}
	const content = `${GENERATED_HEADER}export const EMAIL_HTML_LAYOUTS = {\n${entries.join('\n')}\n} as const;\n`;
	fs.writeFileSync(OUTPUT_FILE, content, 'utf8');
	console.log(`compiled ${names.length} email layouts -> ${path.relative(PACKAGE_ROOT, OUTPUT_FILE)}`);
}

await main();
