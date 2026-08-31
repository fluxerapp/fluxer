// SPDX-License-Identifier: AGPL-3.0-or-later

import {existsSync, readdirSync, readFileSync, statSync, writeFileSync} from 'node:fs';
import {join, relative, resolve, sep} from 'node:path';
import {fileURLToPath} from 'node:url';
import {parse} from '@babel/parser';

const APP_DIR = fileURLToPath(new URL('..', import.meta.url));
const SOURCE_DIR = join(APP_DIR, 'src');
const JSX_EXTENSIONS = new Set(['.tsx', '.jsx']);
const SKIP_DIRS = new Set(['node_modules', 'dist', 'coverage', '.cache', '.swc']);
const GENERATED_FILES = new Set([join(SOURCE_DIR, 'features', 'ui', 'components', 'SVGMasks.tsx')]);
const NON_THEMEABLE_IDENTIFIERS = new Set([
	'Fragment',
	'I18nProvider',
	'Outlet',
	'Profiler',
	'Provider',
	'React',
	'RouterProvider',
	'StrictMode',
	'Suspense',
	'Trans',
]);
const NON_THEMEABLE_MEMBER_PROPERTIES = new Set(['Consumer', 'Fragment', 'Provider']);
const PASS_THROUGH_DOM_MEMBERS = new Set(['motion', 'm']);
const GENERIC_PATH_SEGMENTS = new Set([
	'alerts',
	'bottomsheets',
	'commands',
	'components',
	'config',
	'constants',
	'dialogs',
	'hooks',
	'layout',
	'layouts',
	'modals',
	'models',
	'pages',
	'panels',
	'popouts',
	'routes',
	'sections',
	'shared',
	'state',
	'tabs',
	'types',
	'utils',
]);
const GENERIC_SCOPE_NAMES = new Set(['children', 'component', 'props', 'render', 'root-component']);
const EVENT_ATTRIBUTE_NAMES = [
	'onClick',
	'onPress',
	'onSelect',
	'onSubmit',
	'onChange',
	'onInput',
	'onKeyDown',
	'onPointerDown',
	'onMouseDown',
	'onContextMenu',
];

function printUsage() {
	console.log(`Usage: node scripts/add-data-flx-attributes.mjs [options] [paths...]

Adds stable data-flx attributes to JSX elements for theme selectors.

Options:
  --dry-run              Report changes without writing files (default)
  --write                Write changes
  --check                Exit non-zero when any data-flx attributes are missing
  --target <all|dom>     all = JSX components and DOM nodes, dom = intrinsic DOM/SVG only (default: all)
  --summary-limit <n>    Number of changed files to list in the summary (default: 30)
  --help                 Show this message
`);
}

function parseArgs(argv) {
	const options = {
		check: false,
		write: false,
		target: 'all',
		summaryLimit: 30,
		paths: [],
	};
	for (let index = 0; index < argv.length; index++) {
		const arg = argv[index];
		if (arg === '--dry-run') {
			options.write = false;
		} else if (arg === '--write') {
			options.write = true;
		} else if (arg === '--check') {
			options.check = true;
			options.write = false;
		} else if (arg === '--target') {
			options.target = argv[++index];
		} else if (arg.startsWith('--target=')) {
			options.target = arg.slice('--target='.length);
		} else if (arg === '--summary-limit') {
			options.summaryLimit = Number(argv[++index]);
		} else if (arg.startsWith('--summary-limit=')) {
			options.summaryLimit = Number(arg.slice('--summary-limit='.length));
		} else if (arg === '--help') {
			printUsage();
			process.exit(0);
		} else if (arg === '--') {
		} else if (arg.startsWith('-')) {
			throw new Error(`Unknown option: ${arg}`);
		} else {
			options.paths.push(arg);
		}
	}
	if (!['all', 'dom'].includes(options.target)) {
		throw new Error('--target must be "all" or "dom"');
	}
	if (!Number.isInteger(options.summaryLimit) || options.summaryLimit < 0) {
		throw new Error('--summary-limit must be a non-negative integer');
	}
	return options;
}

function getExtension(path) {
	const index = path.lastIndexOf('.');
	return index === -1 ? '' : path.slice(index);
}

function walk(dir, out) {
	for (const entry of readdirSync(dir, {withFileTypes: true})) {
		if (entry.isDirectory()) {
			if (SKIP_DIRS.has(entry.name)) continue;
			walk(join(dir, entry.name), out);
			continue;
		}
		if (entry.isFile() && JSX_EXTENSIONS.has(getExtension(entry.name))) {
			out.push(join(dir, entry.name));
		}
	}
}

function collectFiles(paths) {
	const files = [];
	const roots = paths.length > 0 ? paths : [SOURCE_DIR];
	for (const input of roots) {
		const absolute = resolve(APP_DIR, input);
		if (!existsSync(absolute)) {
			throw new Error(`Path does not exist: ${input}`);
		}
		const stat = statSync(absolute);
		if (stat.isDirectory()) {
			walk(absolute, files);
		} else if (stat.isFile() && JSX_EXTENSIONS.has(getExtension(absolute))) {
			files.push(absolute);
		}
	}
	return Array.from(new Set(files))
		.filter((file) => !GENERATED_FILES.has(file))
		.sort();
}

function parseSource(source, filePath) {
	return parse(source, {
		sourceFilename: filePath,
		sourceType: 'module',
		errorRecovery: false,
		plugins: [
			'jsx',
			'typescript',
			['decorators', {decoratorsBeforeExport: true}],
			'importAttributes',
			'explicitResourceManagement',
		],
	});
}

function kebabCase(value) {
	return String(value)
		.replace(/([A-Z]+)([A-Z][a-z])/g, '$1-$2')
		.replace(/([a-z0-9])([A-Z])/g, '$1-$2')
		.replace(/[\s_.:/\\]+/g, '-')
		.replace(/[^A-Za-z0-9-]+/g, '-')
		.replace(/-{2,}/g, '-')
		.replace(/^-|-$/g, '')
		.toLowerCase();
}

function uniqueItems(values) {
	const seen = new Set();
	const out = [];
	for (const value of values) {
		if (!value || seen.has(value)) continue;
		seen.add(value);
		out.push(value);
	}
	return out;
}

function relativeFileScope(filePath) {
	const relativePath = relative(SOURCE_DIR, filePath).split(sep).join('/');
	const withoutExtension = relativePath.replace(/\.[tj]sx$/, '');
	const parts = withoutExtension.split('/').filter(Boolean);
	const normalized = [];
	for (let index = 0; index < parts.length; index++) {
		const part = parts[index];
		if (index === 0 && part === 'features') continue;
		const token = kebabCase(part);
		if (!token) continue;
		if (GENERIC_PATH_SEGMENTS.has(token) && index !== parts.length - 1) continue;
		normalized.push(token);
	}
	if (normalized.at(-1) === 'index' && normalized.length > 1) normalized.pop();
	return normalized.join('.');
}

function getNodeName(node) {
	if (!node) return '';
	switch (node.type) {
		case 'JSXIdentifier':
			return node.name;
		case 'JSXMemberExpression':
			return `${getNodeName(node.object)}.${getNodeName(node.property)}`;
		case 'JSXNamespacedName':
			return `${getNodeName(node.namespace)}:${getNodeName(node.name)}`;
		default:
			return '';
	}
}

function getNameTail(name) {
	const parts = name.split(/[.:]/).filter(Boolean);
	if (parts.length === 0) return '';
	if (parts.length >= 2 && PASS_THROUGH_DOM_MEMBERS.has(parts[0]) && /^[a-z]/.test(parts[1])) {
		return parts[1];
	}
	return parts.join('-');
}

function isIntrinsicElementName(name) {
	if (!name) return false;
	if (/^[a-z]/.test(name)) return true;
	const parts = name.split('.');
	return parts.length === 2 && PASS_THROUGH_DOM_MEMBERS.has(parts[0]) && /^[a-z]/.test(parts[1]);
}

function isNonThemeableOpeningElement(opening) {
	const name = getNodeName(opening.name);
	if (!name) return true;
	if (NON_THEMEABLE_IDENTIFIERS.has(name)) return true;
	const parts = name.split('.');
	const property = parts.at(-1);
	return property ? NON_THEMEABLE_MEMBER_PROPERTIES.has(property) : false;
}

function shouldTagOpeningElement(opening, target) {
	if (opening.type !== 'JSXOpeningElement') return false;
	if (isNonThemeableOpeningElement(opening)) return false;
	const name = getNodeName(opening.name);
	if (target === 'dom') return isIntrinsicElementName(name);
	return true;
}

function getAttributeName(attribute) {
	if (!attribute || attribute.type !== 'JSXAttribute') return '';
	return getNodeName(attribute.name);
}

function hasAttribute(opening, name) {
	return opening.attributes.some((attribute) => getAttributeName(attribute) === name);
}

function getAttribute(opening, names) {
	const wanted = Array.isArray(names) ? new Set(names) : new Set([names]);
	return opening.attributes.find((attribute) => wanted.has(getAttributeName(attribute))) ?? null;
}

function stringLiteralAttribute(opening, names) {
	const attribute = getAttribute(opening, names);
	if (!attribute?.value) return '';
	if (attribute.value.type === 'StringLiteral') return attribute.value.value;
	return '';
}

function expressionFromAttribute(attribute) {
	if (!attribute?.value) return null;
	if (attribute.value.type === 'JSXExpressionContainer') return attribute.value.expression;
	return attribute.value;
}

function memberPropertyToken(node) {
	if (!node) return '';
	if (node.type === 'MemberExpression' || node.type === 'OptionalMemberExpression') {
		const objectName = node.object?.type === 'Identifier' ? node.object.name : '';
		if (!/(^|[A-Z])styles?$/.test(objectName)) return '';
		if (!node.computed && node.property?.type === 'Identifier') return kebabCase(node.property.name);
		if (node.computed && node.property?.type === 'StringLiteral') return kebabCase(node.property.value);
		return '';
	}
	return '';
}

function collectClassTokens(node, out = []) {
	if (!node) return out;
	const memberToken = memberPropertyToken(node);
	if (memberToken) out.push(memberToken);
	switch (node.type) {
		case 'StringLiteral':
			out.push(...node.value.split(/\s+/).map(kebabCase));
			break;
		case 'TemplateLiteral':
			for (const quasi of node.quasis) out.push(...quasi.value.cooked.split(/\s+/).map(kebabCase));
			for (const expression of node.expressions) collectClassTokens(expression, out);
			break;
		case 'ArrayExpression':
			for (const element of node.elements) collectClassTokens(element, out);
			break;
		case 'ObjectExpression':
			for (const property of node.properties) {
				if (property.type === 'ObjectProperty') collectClassTokens(property.key, out);
			}
			break;
		case 'CallExpression':
		case 'OptionalCallExpression':
			for (const arg of node.arguments) collectClassTokens(arg, out);
			break;
		case 'ConditionalExpression':
			collectClassTokens(node.consequent, out);
			collectClassTokens(node.alternate, out);
			break;
		case 'LogicalExpression':
			collectClassTokens(node.left, out);
			collectClassTokens(node.right, out);
			break;
		case 'SequenceExpression':
			for (const expression of node.expressions) collectClassTokens(expression, out);
			break;
		case 'TSAsExpression':
		case 'TSSatisfiesExpression':
		case 'TSTypeAssertion':
		case 'TSNonNullExpression':
			collectClassTokens(node.expression, out);
			break;
		default:
			break;
	}
	return out;
}

function commonSegmentPrefix(tokens) {
	if (tokens.length < 2) return '';
	const segments = tokens.map((token) => token.split('-').filter(Boolean));
	const prefix = [];
	for (let index = 0; index < Math.min(...segments.map((segment) => segment.length)); index++) {
		const candidate = segments[0][index];
		if (segments.every((segment) => segment[index] === candidate)) {
			prefix.push(candidate);
		} else {
			break;
		}
	}
	return prefix.length >= 2 ? prefix.join('-') : '';
}

function classTokenForOpening(opening) {
	const classAttribute = getAttribute(opening, ['className', 'class']);
	if (!classAttribute) return '';
	const tokens = uniqueItems(collectClassTokens(expressionFromAttribute(classAttribute)).filter(Boolean));
	if (tokens.length === 0) return '';
	return commonSegmentPrefix(tokens) || tokens[0];
}

function identifierToken(node) {
	if (!node) return '';
	switch (node.type) {
		case 'Identifier':
			return node.name;
		case 'MemberExpression':
		case 'OptionalMemberExpression':
			return identifierToken(node.property);
		case 'CallExpression':
		case 'OptionalCallExpression':
			return identifierToken(node.callee);
		default:
			return '';
	}
}

function firstCallTokenFromExpression(node) {
	if (!node) return '';
	switch (node.type) {
		case 'CallExpression':
		case 'OptionalCallExpression':
			return identifierToken(node.callee);
		case 'ArrowFunctionExpression':
		case 'FunctionExpression':
			return firstCallTokenFromExpression(node.body);
		case 'BlockStatement':
			for (const statement of node.body) {
				const token = firstCallTokenFromExpression(statement);
				if (token) return token;
			}
			return '';
		case 'ExpressionStatement':
			return firstCallTokenFromExpression(node.expression);
		case 'ReturnStatement':
			return firstCallTokenFromExpression(node.argument);
		case 'ConditionalExpression':
			return firstCallTokenFromExpression(node.consequent) || firstCallTokenFromExpression(node.alternate);
		default:
			return identifierToken(node);
	}
}

function normalizeHandlerToken(token) {
	const kebab = kebabCase(token)
		.replace(/^handle-/, '')
		.replace(/^on-/, '')
		.replace(/-(handler|callback)$/, '');
	return GENERIC_SCOPE_NAMES.has(kebab) ? '' : kebab;
}

function handlerTokenForOpening(opening) {
	for (const name of EVENT_ATTRIBUTE_NAMES) {
		const attribute = getAttribute(opening, name);
		const token = normalizeHandlerToken(firstCallTokenFromExpression(expressionFromAttribute(attribute)));
		if (token) return token;
	}
	return '';
}

function isInteractiveOpening(opening) {
	const name = getNodeName(opening.name);
	const tail = kebabCase(getNameTail(name));
	if (['a', 'button', 'input', 'select', 'textarea', 'summary'].includes(tail)) return true;
	if (stringLiteralAttribute(opening, 'role') === 'button') return true;
	return EVENT_ATTRIBUTE_NAMES.some((eventName) => getAttribute(opening, eventName));
}

function semanticTokenForOpening(opening) {
	const name = getNodeName(opening.name);
	const nameToken = kebabCase(getNameTail(name)) || 'element';
	const dataRole = kebabCase(stringLiteralAttribute(opening, ['data-role', 'data-testid', 'data-test-id']));
	const id = kebabCase(stringLiteralAttribute(opening, 'id'));
	const classToken = classTokenForOpening(opening);
	const role = kebabCase(stringLiteralAttribute(opening, 'role'));
	const ariaLabel = kebabCase(stringLiteralAttribute(opening, 'aria-label'));
	const type = kebabCase(stringLiteralAttribute(opening, 'type'));
	const handler = isInteractiveOpening(opening) ? handlerTokenForOpening(opening) : '';
	const parts = [];
	if (dataRole) parts.push(dataRole);
	else if (id) parts.push(id);
	else if (classToken) parts.push(classToken);
	else if (role) parts.push(role);
	else if (ariaLabel) parts.push(ariaLabel);
	else parts.push(nameToken);
	if (handler && !parts.some((part) => part.includes(handler) || handler.includes(part))) parts.push(handler);
	if (type && !parts.some((part) => part.includes(type))) parts.push(type);
	return uniqueItems(parts).join('.');
}

function inferFunctionScopeName(node, ancestors) {
	if (node.type === 'FunctionDeclaration' && node.id?.name) return node.id.name;
	if (node.type === 'FunctionExpression' && node.id?.name) return node.id.name;
	const parent = ancestors.at(-1);
	const grandparent = ancestors.at(-2);
	if (parent?.type === 'VariableDeclarator' && parent.id?.type === 'Identifier') return parent.id.name;
	if (parent?.type === 'ObjectProperty' && parent.key?.type === 'Identifier') return parent.key.name;
	if (parent?.type === 'ObjectMethod' && parent.key?.type === 'Identifier') return parent.key.name;
	if (parent?.type === 'AssignmentExpression' && parent.left?.type === 'Identifier') return parent.left.name;
	if (
		parent?.type === 'CallExpression' &&
		grandparent?.type === 'VariableDeclarator' &&
		grandparent.id?.type === 'Identifier'
	) {
		return grandparent.id.name;
	}
	return '';
}

function isFunctionLike(node) {
	return (
		node.type === 'FunctionDeclaration' ||
		node.type === 'FunctionExpression' ||
		node.type === 'ArrowFunctionExpression' ||
		node.type === 'ObjectMethod' ||
		node.type === 'ClassMethod' ||
		node.type === 'ClassPrivateMethod'
	);
}

function isTraversableNode(value) {
	return value && typeof value === 'object' && typeof value.type === 'string';
}

function visit(node, ancestors, state) {
	if (!isTraversableNode(node)) return;
	let nextState = state;
	if (isFunctionLike(node)) {
		const rawScope = inferFunctionScopeName(node, ancestors);
		const scope = kebabCase(rawScope);
		if (scope && !GENERIC_SCOPE_NAMES.has(scope)) {
			nextState = {...state, scopes: [...state.scopes, scope]};
		}
	}
	if (node.type === 'JSXOpeningElement') {
		state.onOpening(node, nextState.scopes);
	}
	const nextAncestors = [...ancestors, node];
	for (const [key, value] of Object.entries(node)) {
		if (
			key === 'comments' ||
			key === 'end' ||
			key === 'extra' ||
			key === 'innerComments' ||
			key === 'leadingComments' ||
			key === 'loc' ||
			key === 'start' ||
			key === 'trailingComments'
		) {
			continue;
		}
		if (Array.isArray(value)) {
			for (const item of value) visit(item, nextAncestors, nextState);
		} else {
			visit(value, nextAncestors, nextState);
		}
	}
}

function dataFlxValue(filePath, opening, scopes, usedKeys) {
	const scopeParts = [relativeFileScope(filePath)];
	const localScope = scopes.at(-1);
	const localScopeToken = localScope && !scopeParts[0].split('.').includes(localScope) ? localScope : '';
	if (localScopeToken) scopeParts.push(localScopeToken);
	scopeParts.push(semanticTokenForOpening(opening));
	const base = scopeParts.filter(Boolean).join('.');
	const count = usedKeys.get(base) ?? 0;
	usedKeys.set(base, count + 1);
	return count === 0 ? base : `${base}--${count + 1}`;
}

function lineStartAt(source, position) {
	const previousNewline = source.lastIndexOf('\n', position - 1);
	return previousNewline === -1 ? 0 : previousNewline + 1;
}

function lineEndAt(source, position) {
	const nextNewline = source.indexOf('\n', position);
	return nextNewline === -1 ? source.length : nextNewline;
}

function indentationAt(source, position) {
	const lineStart = lineStartAt(source, position);
	const lineEnd = lineEndAt(source, position);
	const linePrefix = source.slice(lineStart, Math.min(position, lineEnd));
	const match = linePrefix.match(/^\s*/);
	return match ? match[0] : '';
}

function attributeIndent(source, opening) {
	const multiline = source.slice(opening.start, opening.end).includes('\n');
	if (!multiline) return '';
	for (const attribute of opening.attributes) {
		const start = attribute.start;
		const lineStart = lineStartAt(source, start);
		if (/^\s*$/.test(source.slice(lineStart, start))) return indentationAt(source, start);
	}
	return `${indentationAt(source, opening.start)}\t`;
}

function openingCloseTokenStart(source, opening) {
	const close = source.lastIndexOf('>', opening.end - 1);
	return opening.selfClosing ? close - 1 : close;
}

function buildInsertion(source, opening, value) {
	const attrText = `data-flx=${JSON.stringify(value)}`;
	const multiline = source.slice(opening.start, opening.end).includes('\n');
	const firstSpread = opening.attributes.find((attribute) => attribute.type === 'JSXSpreadAttribute');
	if (firstSpread) {
		if (!multiline) return {position: firstSpread.start, text: `${attrText} `};
		const start = lineStartAt(source, firstSpread.start);
		return {position: start, text: `${attributeIndent(source, opening)}${attrText}\n`};
	}
	const closeStart = openingCloseTokenStart(source, opening);
	if (!multiline) {
		const prefix = /\s/.test(source[closeStart - 1] ?? '') ? '' : ' ';
		const suffix = opening.selfClosing ? ' ' : '';
		return {position: closeStart, text: `${prefix}${attrText}${suffix}`};
	}
	return {position: lineStartAt(source, closeStart), text: `${attributeIndent(source, opening)}${attrText}\n`};
}

function processFile(filePath, options) {
	const source = readFileSync(filePath, 'utf8');
	const ast = parseSource(source, filePath);
	const usedKeys = new Map();
	const insertions = [];
	const stats = {
		added: 0,
		existing: 0,
		skipped: 0,
	};
	visit(ast, [], {
		scopes: [],
		onOpening(opening, scopes) {
			if (!shouldTagOpeningElement(opening, options.target)) {
				stats.skipped++;
				return;
			}
			if (hasAttribute(opening, 'data-flx')) {
				stats.existing++;
				return;
			}
			const value = dataFlxValue(filePath, opening, scopes, usedKeys);
			insertions.push(buildInsertion(source, opening, value));
			stats.added++;
		},
	});
	if (insertions.length > 0 && options.write) {
		let output = source;
		for (const insertion of insertions.sort((a, b) => b.position - a.position)) {
			output = `${output.slice(0, insertion.position)}${insertion.text}${output.slice(insertion.position)}`;
		}
		writeFileSync(filePath, output);
	}
	return stats;
}

function main() {
	const options = parseArgs(process.argv.slice(2));
	const files = collectFiles(options.paths);
	const totals = {
		filesScanned: 0,
		filesChanged: 0,
		added: 0,
		existing: 0,
		skipped: 0,
	};
	const changedFiles = [];
	for (const file of files) {
		const result = processFile(file, options);
		totals.filesScanned++;
		totals.added += result.added;
		totals.existing += result.existing;
		totals.skipped += result.skipped;
		if (result.added > 0) {
			totals.filesChanged++;
			changedFiles.push({file, added: result.added});
		}
	}
	const mode = options.write ? 'write' : options.check ? 'check' : 'dry-run';
	console.log(
		`data-flx ${mode}: filesScanned=${totals.filesScanned} filesChanged=${totals.filesChanged} added=${totals.added} existing=${totals.existing} skipped=${totals.skipped} target=${options.target}`,
	);
	for (const item of changedFiles.slice(0, options.summaryLimit)) {
		console.log(`  ${relative(APP_DIR, item.file)} +${item.added}`);
	}
	if (changedFiles.length > options.summaryLimit) {
		console.log(`  ...and ${changedFiles.length - options.summaryLimit} more files`);
	}
	if (options.check && totals.added > 0) {
		process.exitCode = 1;
	}
}

main();
