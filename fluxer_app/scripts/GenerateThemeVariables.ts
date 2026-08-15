// SPDX-License-Identifier: AGPL-3.0-or-later

import {existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync} from 'node:fs';
import {dirname, join, relative, resolve} from 'node:path';

type ThemeVariableKind = 'color' | 'font' | 'dimension' | 'number' | 'shadow' | 'transition' | 'other';

interface CssSource {
	file: string;
	label: string;
	generatedBy?: string;
}

interface VariableDefinition {
	name: string;
	kind: ThemeVariableKind;
	groupId: string;
	groupLabel: string;
	source: string;
}

const PRIORITY_CSS_SOURCES: ReadonlyArray<CssSource> = [
	{file: 'src/app/globals.css', label: 'globals'},
	{
		file: 'src/features/theme/styles/generated/color-system.css',
		label: 'color-system',
		generatedBy: 'pnpm generate:colors',
	},
	{
		file: 'src/features/theme/styles/generated/message-layout.css',
		label: 'message-layout',
		generatedBy: 'pnpm generate:message-layout',
	},
];
const PRIORITY_SOURCE_INDEX = new Map(PRIORITY_CSS_SOURCES.map((source, index) => [source.file, index]));
const IGNORED_SOURCE_PREFIXES = ['src/features/theme_studio/', 'src/theme/'];

const BUNDLED_FONT_VARIABLES_CSS = resolve(
	import.meta.dirname,
	'..',
	'..',
	'packages',
	'fonts',
	'css',
	'variables.css',
);

function bundledFontStack(name: string): string {
	const css = readFileSync(BUNDLED_FONT_VARIABLES_CSS, 'utf8');
	const declaration = new RegExp(`${name}:([^;]*);`).exec(css);
	if (!declaration) {
		throw new Error(`${name} is not declared in ${BUNDLED_FONT_VARIABLES_CSS}`);
	}
	return declaration[1].replace(/\s+/g, ' ').trim();
}

const EXTRA_GLOBAL_DEFAULTS: ReadonlyArray<{name: string; value: string; source: string}> = [
	{
		name: '--font-sans',
		value: bundledFontStack('--font-sans'),
		source: 'runtime-fonts',
	},
	{
		name: '--font-mono',
		value: bundledFontStack('--font-mono'),
		source: 'runtime-fonts',
	},
	{name: '--font-size', value: '1rem', source: 'runtime-accessibility'},
	{name: '--chat-horizontal-padding', value: '1rem', source: 'runtime-accessibility'},
	{name: '--message-group-spacing', value: '1rem', source: 'runtime-accessibility'},
	{name: '--link-decoration', value: 'none', source: 'runtime-accessibility'},
	{name: '--markup-strikethrough-color', value: 'currentColor', source: 'runtime-accessibility'},
];

const GROUP_LABELS: Record<string, string> = {
	typography: 'Typography',
	surfaces: 'Surfaces',
	headers: 'Headers',
	text: 'Text',
	brand: 'Brand & accents',
	status: 'Status indicators',
	borders: 'Borders & focus',
	alerts: 'Alerts & callouts',
	markup: 'Markup & mentions',
	buttons: 'Buttons',
	code: 'Code & terminal',
	tables: 'Tables',
	scrolling: 'Scrolling',
	layout: 'Layout',
	messages: 'Messages',
	emoji: 'Emoji',
	motion: 'Motion',
	layering: 'Layering',
	media: 'Media',
	forms: 'Forms',
	other: 'Other',
};

function stripAtRuleBlocks(css: string): string {
	let output = '';
	let index = 0;
	while (index < css.length) {
		if (css[index] !== '@') {
			output += css[index];
			index += 1;
			continue;
		}
		const nextSemicolon = css.indexOf(';', index);
		const nextBrace = css.indexOf('{', index);
		if (nextBrace === -1 || (nextSemicolon !== -1 && nextSemicolon < nextBrace)) {
			index = nextSemicolon === -1 ? css.length : nextSemicolon + 1;
			continue;
		}
		let depth = 0;
		let cursor = nextBrace;
		for (; cursor < css.length; cursor += 1) {
			if (css[cursor] === '{') depth += 1;
			if (css[cursor] === '}') {
				depth -= 1;
				if (depth === 0) {
					cursor += 1;
					break;
				}
			}
		}
		index = cursor;
	}
	return output;
}

function toPosixPath(path: string): string {
	return path.replaceAll('\\', '/');
}

function discoverCssSources(appDir: string): ReadonlyArray<CssSource> {
	const srcDir = join(appDir, 'src');
	const files: Array<string> = [];
	const visit = (directory: string) => {
		for (const entry of readdirSync(directory, {withFileTypes: true})) {
			const absolutePath = join(directory, entry.name);
			if (entry.isDirectory()) {
				visit(absolutePath);
				continue;
			}
			if (!entry.isFile() || !entry.name.endsWith('.css')) continue;
			const sourceFile = toPosixPath(relative(appDir, absolutePath));
			if (IGNORED_SOURCE_PREFIXES.some((prefix) => sourceFile.startsWith(prefix))) continue;
			files.push(sourceFile);
		}
	};
	visit(srcDir);
	const discovered = new Set(files);
	const missing = PRIORITY_CSS_SOURCES.filter((source) => !discovered.has(source.file));
	if (missing.length > 0) {
		const remedies = missing.map(
			(source) => `  ${source.file} is missing. Run \`${source.generatedBy ?? 'pnpm build'}\` first.`,
		);
		throw new Error(
			`Cannot generate the theme variable manifest from a partial input.\n${remedies.join('\n')}\n` +
				'Writing anyway would silently delete every variable those sources declare from a tracked file.',
		);
	}
	return files
		.sort((left, right) => {
			const leftPriority = PRIORITY_SOURCE_INDEX.get(left);
			const rightPriority = PRIORITY_SOURCE_INDEX.get(right);
			if (leftPriority !== undefined || rightPriority !== undefined) {
				return (leftPriority ?? Number.MAX_SAFE_INTEGER) - (rightPriority ?? Number.MAX_SAFE_INTEGER);
			}
			return left.localeCompare(right);
		})
		.map((file) => ({
			file,
			label: PRIORITY_CSS_SOURCES.find((source) => source.file === file)?.label ?? file.replace(/^src\//, ''),
		}));
}

function selectorHas(selector: string, target: string): boolean {
	return selector
		.split(',')
		.map((part) => part.trim())
		.includes(target);
}

function extractDeclarations(block: string): Array<[string, string]> {
	const declarations: Array<[string, string]> = [];
	const pattern = /(--[a-zA-Z0-9_-]+)\s*:\s*([^;]+);/g;
	let match: RegExpExecArray | null;
	while ((match = pattern.exec(block)) !== null) {
		const name = match[1] as string;
		const value = (match[2] as string).replace(/\s+/g, ' ').trim();
		declarations.push([name, value]);
	}
	return declarations;
}

function readSourceVariables(appDir: string): {
	darkDefaults: Map<string, string>;
	lightDefaults: Map<string, string>;
	sources: Map<string, string>;
} {
	const darkDefaults = new Map<string, string>();
	const lightOverrides = new Map<string, string>();
	const sources = new Map<string, string>();
	for (const source of discoverCssSources(appDir)) {
		const absolutePath = join(appDir, source.file);
		const css = stripAtRuleBlocks(readFileSync(absolutePath, 'utf8').replace(/\/\*[\s\S]*?\*\//g, ''));
		const blockPattern = /([^{}]+)\{([^{}]*)\}/g;
		let match: RegExpExecArray | null;
		while ((match = blockPattern.exec(css)) !== null) {
			const selector = (match[1] as string).trim();
			const block = match[2] as string;
			const isRoot = selectorHas(selector, ':root');
			const isLight = selectorHas(selector, '.theme-light');
			if (!isRoot && !isLight) continue;
			for (const [name, value] of extractDeclarations(block)) {
				if (isRoot) {
					darkDefaults.set(name, value);
					sources.set(name, source.label);
				}
				if (isLight) {
					lightOverrides.set(name, value);
					sources.set(name, source.label);
				}
			}
		}
	}
	for (const extra of EXTRA_GLOBAL_DEFAULTS) {
		if (!darkDefaults.has(extra.name)) {
			darkDefaults.set(extra.name, extra.value);
			sources.set(extra.name, extra.source);
		}
	}
	const lightDefaults = new Map(darkDefaults);
	for (const [name, value] of lightOverrides) {
		lightDefaults.set(name, value);
	}
	return {darkDefaults, lightDefaults, sources};
}

function findMatchingParen(text: string, openIndex: number): number {
	let depth = 0;
	for (let index = openIndex; index < text.length; index += 1) {
		const character = text[index];
		if (character === '(') depth += 1;
		if (character === ')') {
			depth -= 1;
			if (depth === 0) return index;
		}
	}
	return -1;
}

function splitVarArguments(inner: string): {dependency: string; fallback: string | null} {
	let depth = 0;
	for (let index = 0; index < inner.length; index += 1) {
		const character = inner[index];
		if (character === '(') depth += 1;
		if (character === ')') depth -= 1;
		if (character === ',' && depth === 0) {
			return {dependency: inner.slice(0, index).trim(), fallback: inner.slice(index + 1).trim()};
		}
	}
	return {dependency: inner.trim(), fallback: null};
}

function resolveVariableValue(name: string, values: ReadonlyMap<string, string>, stack = new Set<string>()): string {
	const value = values.get(name);
	if (!value) return '';
	let result = '';
	let cursor = 0;
	while (cursor < value.length) {
		const start = value.indexOf('var(', cursor);
		if (start === -1) {
			result += value.slice(cursor);
			return result;
		}
		const close = findMatchingParen(value, start + 3);
		if (close === -1) {
			result += value.slice(cursor);
			return result;
		}
		result += value.slice(cursor, start);
		const full = value.slice(start, close + 1);
		const {dependency, fallback} = splitVarArguments(value.slice(start + 4, close));
		if (dependency === '--saturation-factor') {
			result += full;
		} else if (stack.has(dependency) || !values.get(dependency)) {
			result += fallback ?? full;
		} else {
			const nextStack = new Set(stack);
			nextStack.add(name);
			result += resolveVariableValue(dependency, values, nextStack);
		}
		cursor = close + 1;
	}
	return result;
}

function getGroupId(name: string): string {
	if (name.startsWith('--font')) return 'typography';
	if (name.startsWith('--z-index')) return 'layering';
	if (name.startsWith('--transition')) return 'motion';
	if (name.startsWith('--shadow')) return 'borders';
	if (name.includes('scrollbar')) return 'scrolling';
	if (name.startsWith('--message')) return 'messages';
	if (name.includes('typing')) return 'messages';
	if (name.includes('emoji')) return 'emoji';
	if (name.includes('textarea') || name.includes('input') || name.includes('form')) return 'forms';
	if (name.includes('button') || name.includes('control-button')) return 'buttons';
	if (name.startsWith('--code') || name.startsWith('--ansi') || name === '--text-code') return 'code';
	if (name.includes('table')) return 'tables';
	if (name.startsWith('--markup') || name.includes('spoiler')) return 'markup';
	if (name.startsWith('--alert')) return 'alerts';
	if (name.startsWith('--status')) return 'status';
	if (name.startsWith('--brand') || name.startsWith('--accent') || name.startsWith('--plutonium')) return 'brand';
	if (name.startsWith('--text')) return 'text';
	if (name.includes('border') || name.includes('focus') || name.includes('radius')) return 'borders';
	if (name.includes('layout') || name.includes('spacing') || name.includes('padding') || name.includes('gap'))
		return 'layout';
	if (name.includes('width') || name.includes('height') || name.includes('size') || name.includes('gutter'))
		return 'layout';
	if (name.includes('media') || name.includes('avatar') || name.includes('guild-icon')) return 'media';
	if (name.includes('bg') || name.includes('background') || name.includes('surface') || name.includes('guild-list')) {
		return 'surfaces';
	}
	return 'other';
}

function getKind(name: string, value: string): ThemeVariableKind {
	const lowerName = name.toLowerCase();
	const lowerValue = value.toLowerCase();
	if (name === '--font-sans' || name === '--font-mono') return 'font';
	if (name.startsWith('--shadow')) return 'shadow';
	if (name.startsWith('--transition') || /\b\d+(?:\.\d+)?m?s\b/.test(lowerValue)) return 'transition';
	if (/^-?\d+(?:\.\d+)?$/.test(value)) return 'number';
	if (lowerName.includes('opacity')) return 'number';
	if (lowerValue.includes(' solid ')) return 'other';
	if (
		lowerValue === 'transparent' ||
		lowerValue === 'currentcolor' ||
		lowerValue.startsWith('#') ||
		lowerValue.startsWith('hsl') ||
		lowerValue.startsWith('rgb') ||
		lowerValue.startsWith('color-mix')
	) {
		return 'color';
	}
	if (
		/(?:^|\s)-?\d*\.?\d+(?:px|rem|em|%|vh|vw|dvh|svh|cqi)\b/.test(lowerValue) ||
		lowerValue.includes('calc(') ||
		lowerValue.includes('clamp(') ||
		lowerValue.includes('min(') ||
		lowerValue.includes('max(')
	) {
		return 'dimension';
	}
	if (
		lowerName.startsWith('--ansi') ||
		lowerName.includes('color') ||
		lowerName.startsWith('--text-') ||
		lowerName.endsWith('-text') ||
		lowerName.includes('-text-') ||
		lowerName.includes('bg') ||
		lowerName.includes('background') ||
		lowerName.includes('fill') ||
		lowerName.includes('accent') ||
		lowerName.includes('brand') ||
		lowerName.includes('status') ||
		lowerName.includes('alert') ||
		lowerName.includes('selection')
	) {
		return 'color';
	}
	return 'other';
}

function buildDefinitions(
	darkDefaults: ReadonlyMap<string, string>,
	sources: ReadonlyMap<string, string>,
): Array<VariableDefinition> {
	return [...darkDefaults.keys()]
		.sort((left, right) => left.localeCompare(right))
		.map((name) => {
			const value = resolveVariableValue(name, darkDefaults);
			const groupId = getGroupId(name);
			return {
				name,
				kind: getKind(name, value),
				groupId,
				groupLabel: GROUP_LABELS[groupId] ?? GROUP_LABELS.other,
				source: sources.get(name) ?? 'unknown',
			};
		});
}

function renderStringArray(name: string, values: ReadonlyArray<string>): string {
	const body = values.map((value) => `\t${JSON.stringify(value)},`).join('\n');
	return `export const ${name}: ReadonlyArray<string> = [\n${body}\n];`;
}

function renderValueMap(name: string, values: ReadonlyMap<string, string>): string {
	const body = [...values.keys()]
		.sort((left, right) => left.localeCompare(right))
		.map((key) => `\t${JSON.stringify(key)}: ${JSON.stringify(resolveVariableValue(key, values))},`)
		.join('\n');
	return `export const ${name}: Readonly<Record<string, string>> = {\n${body}\n};`;
}

function renderDefinitions(definitions: ReadonlyArray<VariableDefinition>): string {
	const body = definitions
		.map(
			(definition) =>
				`\t{name: ${JSON.stringify(definition.name)}, kind: ${JSON.stringify(definition.kind)}, groupId: ${JSON.stringify(definition.groupId)}, groupLabel: ${JSON.stringify(definition.groupLabel)}, source: ${JSON.stringify(definition.source)}},`,
		)
		.join('\n');
	return `export const THEME_VARIABLES: ReadonlyArray<ThemeVariableDefinition> = [\n${body}\n];`;
}

function render(appDir: string): string {
	const {darkDefaults, lightDefaults, sources} = readSourceVariables(appDir);
	const definitions = buildDefinitions(darkDefaults, sources);
	const colorVariables = definitions
		.filter((definition) => definition.kind === 'color')
		.map((definition) => definition.name);
	const fontVariables = definitions
		.filter((definition) => definition.kind === 'font')
		.map((definition) => definition.name);
	return `// SPDX-License-Identifier: AGPL-3.0-or-later
// Generated by scripts/GenerateThemeVariables.ts. Do not edit by hand.

export type ThemeVariableKind = 'color' | 'font' | 'dimension' | 'number' | 'shadow' | 'transition' | 'other';

export interface ThemeVariableDefinition {
\tname: string;
\tkind: ThemeVariableKind;
\tgroupId: string;
\tgroupLabel: string;
\tsource: string;
}

${renderDefinitions(definitions)}

${renderStringArray(
	'THEME_VARIABLE_NAMES',
	definitions.map((definition) => definition.name),
)}

${renderStringArray('THEME_COLOR_VARIABLES', colorVariables)}

${renderStringArray('THEME_FONT_VARIABLES', fontVariables)}

${renderValueMap('THEME_STUDIO_DARK_DEFAULT_VARIABLE_VALUES', darkDefaults)}

${renderValueMap('THEME_STUDIO_LIGHT_DEFAULT_VARIABLE_VALUES', lightDefaults)}
`;
}

function main(): void {
	const scriptDir = import.meta.dirname;
	const appDir = resolve(scriptDir, '..');
	const outputPath = join(appDir, 'src', 'features', 'theme', 'variables', 'ThemeVariableManifest.ts');
	const contents = render(appDir);
	if (process.argv.includes('--check')) {
		if (!existsSync(outputPath) || readFileSync(outputPath, 'utf8') !== contents) {
			throw new Error(`${relative(appDir, outputPath)} is stale. Run pnpm generate:theme-variables.`);
		}
		console.log(`Checked ${relative(appDir, outputPath)}`);
		return;
	}
	mkdirSync(dirname(outputPath), {recursive: true});
	writeFileSync(outputPath, contents);
	console.log(`Wrote ${relative(appDir, outputPath)}`);
}

main();
