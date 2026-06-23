// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdirSync, readFileSync, statSync} from 'node:fs';
import {join, relative} from 'node:path';
import {fileURLToPath} from 'node:url';
import {parse} from '@babel/parser';
import {describe, expect, it} from 'vitest';

interface AstNode {
	type?: string;
	start?: number | null;
	end?: number | null;
	loc?: {
		start: {
			line: number;
		};
	};
	[key: string]: unknown;
}

interface HookOrderFinding {
	file: string;
	functionName: string;
	returnLine: number;
	hookLine: number;
	hookName: string;
}

const SRC_ROOT = fileURLToPath(new URL('../../../', import.meta.url));
const SOURCE_EXTENSIONS = new Set(['.ts', '.tsx']);

function listSourceFiles(directory: string): Array<string> {
	return readdirSync(directory).flatMap((entry) => {
		const filePath = join(directory, entry);
		const stats = statSync(filePath);
		if (stats.isDirectory()) {
			return listSourceFiles(filePath);
		}
		if (!stats.isFile()) {
			return [];
		}
		const extension = filePath.endsWith('.tsx') ? '.tsx' : filePath.endsWith('.ts') ? '.ts' : '';
		if (!SOURCE_EXTENSIONS.has(extension)) {
			return [];
		}
		return [filePath];
	});
}

function isFunctionNode(node: AstNode | null | undefined): boolean {
	return Boolean(
		node &&
			(node.type === 'FunctionDeclaration' ||
				node.type === 'FunctionExpression' ||
				node.type === 'ArrowFunctionExpression' ||
				node.type === 'ObjectMethod' ||
				node.type === 'ClassMethod'),
	);
}

function getCalleeName(callee: AstNode | null | undefined): string | null {
	if (!callee) {
		return null;
	}
	if (callee.type === 'Identifier' && typeof callee.name === 'string') {
		return callee.name;
	}
	if (
		callee.type === 'MemberExpression' &&
		callee.computed !== true &&
		typeof callee.property === 'object' &&
		callee.property !== null
	) {
		return getCalleeName(callee.property as AstNode);
	}
	return null;
}

function isHookName(name: string | null): name is string {
	return Boolean(name && (name === 'use' || /^use[A-Z0-9]/.test(name)));
}

function isLikelyReactFunctionName(name: string): boolean {
	return name === '<anonymous>' || /^[A-Z]/.test(name) || /^use[A-Z0-9]/.test(name);
}

function getFunctionName(node: AstNode, parent: AstNode | null | undefined): string {
	if (typeof node.id === 'object' && node.id && typeof (node.id as AstNode).name === 'string') {
		return (node.id as AstNode).name as string;
	}
	if (parent?.type === 'VariableDeclarator' && typeof parent.id === 'object' && parent.id) {
		const id = parent.id as AstNode;
		return typeof id.name === 'string' ? id.name : '<anonymous>';
	}
	if (parent?.type === 'CallExpression') {
		return getFunctionName(parent, parent.__parent as AstNode | null | undefined);
	}
	if (parent?.type === 'ExportNamedDeclaration') {
		return getFunctionName(node, parent.__parent as AstNode | null | undefined);
	}
	return '<anonymous>';
}

function walk(
	node: AstNode | null | undefined,
	visitor: (node: AstNode, parent: AstNode | null) => void,
	parent: AstNode | null = null,
) {
	if (!node || typeof node !== 'object') {
		return;
	}
	node.__parent = parent;
	visitor(node, parent);
	for (const [key, value] of Object.entries(node)) {
		if (key === 'loc' || key === 'start' || key === 'end' || key === '__parent') {
			continue;
		}
		if (Array.isArray(value)) {
			for (const child of value) {
				if (child && typeof child === 'object' && typeof child.type === 'string') {
					walk(child as AstNode, visitor, node);
				}
			}
		} else if (value && typeof value === 'object' && typeof (value as AstNode).type === 'string') {
			walk(value as AstNode, visitor, node);
		}
	}
}

function collectReturnsAndHooks(functionNode: AstNode): {returns: Array<AstNode>; hooks: Array<AstNode>} {
	const returns: Array<AstNode> = [];
	const hooks: Array<AstNode> = [];
	function visit(node: AstNode | null | undefined) {
		if (!node || typeof node !== 'object') {
			return;
		}
		if (node !== functionNode && isFunctionNode(node)) {
			return;
		}
		if (node.type === 'ReturnStatement') {
			returns.push(node);
		}
		if (node.type === 'CallExpression') {
			const hookName = getCalleeName(node.callee as AstNode | null | undefined);
			if (isHookName(hookName)) {
				hooks.push(node);
			}
		}
		for (const [key, value] of Object.entries(node)) {
			if (key === 'loc' || key === 'start' || key === 'end' || key === '__parent') {
				continue;
			}
			if (Array.isArray(value)) {
				for (const child of value) {
					if (child && typeof child === 'object' && typeof child.type === 'string') {
						visit(child as AstNode);
					}
				}
			} else if (value && typeof value === 'object' && typeof (value as AstNode).type === 'string') {
				visit(value as AstNode);
			}
		}
	}
	visit(functionNode.body as AstNode | null | undefined);
	return {returns, hooks};
}

function findHookOrderHazards(): Array<HookOrderFinding> {
	const findings: Array<HookOrderFinding> = [];
	for (const file of listSourceFiles(SRC_ROOT)) {
		const source = readFileSync(file, 'utf8');
		const ast = parse(source, {
			sourceType: 'module',
			plugins: ['typescript', 'jsx', 'decorators-legacy'],
		}) as unknown as AstNode;
		walk(ast, (node, parent) => {
			if (!isFunctionNode(node)) {
				return;
			}
			const functionName = getFunctionName(node, parent);
			if (!isLikelyReactFunctionName(functionName)) {
				return;
			}
			const {returns, hooks} = collectReturnsAndHooks(node);
			for (const hook of hooks) {
				const hookStart = hook.start ?? 0;
				const earlierReturn = returns.find((returnNode) => (returnNode.end ?? 0) < hookStart);
				if (!earlierReturn) {
					continue;
				}
				findings.push({
					file: relative(SRC_ROOT, file),
					functionName,
					returnLine: earlierReturn.loc?.start.line ?? 0,
					hookLine: hook.loc?.start.line ?? 0,
					hookName: getCalleeName(hook.callee as AstNode | null | undefined) ?? '<unknown>',
				});
				return;
			}
		});
	}
	return findings;
}

describe('React hook order stability', () => {
	it('keeps guard returns outside hook-bearing component bodies', () => {
		expect(findHookOrderHazards()).toEqual([]);
	}, 10_000);
});
