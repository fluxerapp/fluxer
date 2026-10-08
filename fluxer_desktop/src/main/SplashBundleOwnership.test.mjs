// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {existsSync, readFileSync, statSync} from 'node:fs';
import path from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';

const SRC_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const SOURCE_EXTENSIONS = ['.ts', '.tsx', '.js', '.jsx'];
const TYPE_ONLY_STATEMENT = /\b(?:import|export)\s+type\b[\s\S]*?from\s*'[^']*';/g;
const RUNTIME_SPECIFIER = /(?:from|import\s*\()\s*'([^']+)'/g;

const BOOTSTRAP_ENTRY = 'main/Bootstrap.ts';
const MAIN_APP_ENTRY = 'main/index.ts';
const SPLASH_MODULE = 'main/SplashWindow.ts';

function resolveSourceModule(absolutePath) {
	if (existsSync(absolutePath) && statSync(absolutePath).isFile()) {
		return absolutePath;
	}
	for (const extension of SOURCE_EXTENSIONS) {
		if (existsSync(absolutePath + extension)) {
			return absolutePath + extension;
		}
	}
	for (const extension of SOURCE_EXTENSIONS) {
		const indexPath = path.join(absolutePath, `index${extension}`);
		if (existsSync(indexPath)) {
			return indexPath;
		}
	}
	return null;
}

function resolveSpecifier(specifier, fromRelativePath) {
	if (specifier.startsWith('@electron/')) {
		return resolveSourceModule(path.join(SRC_DIR, specifier.slice('@electron/'.length)));
	}
	if (specifier.startsWith('.')) {
		return resolveSourceModule(path.resolve(SRC_DIR, path.dirname(fromRelativePath), specifier));
	}
	return null;
}

function readRuntimeEdges(relativePath) {
	const source = readFileSync(path.join(SRC_DIR, relativePath), 'utf8').replace(TYPE_ONLY_STATEMENT, '');
	const edges = new Set();
	for (const match of source.matchAll(RUNTIME_SPECIFIER)) {
		const resolved = resolveSpecifier(match[1], relativePath);
		if (resolved != null) {
			edges.add(path.relative(SRC_DIR, resolved));
		}
	}
	return edges;
}

function findImportChain(entryRelativePath, targetRelativePath) {
	const parents = new Map([[entryRelativePath, null]]);
	const queue = [entryRelativePath];
	while (queue.length > 0) {
		const current = queue.shift();
		if (current === targetRelativePath) {
			const chain = [];
			for (let step = current; step != null; step = parents.get(step)) {
				chain.unshift(step);
			}
			return chain;
		}
		for (const edge of readRuntimeEdges(current)) {
			if (!parents.has(edge)) {
				parents.set(edge, current);
				queue.push(edge);
			}
		}
	}
	return null;
}

describe('splash bundle ownership', () => {
	test('the bootstrap bundle owns the splash', () => {
		assert.notEqual(
			findImportChain(BOOTSTRAP_ENTRY, SPLASH_MODULE),
			null,
			'Bootstrap must still reach the splash, otherwise this walker proves nothing about the bundle that must not reach it.',
		);
	});

	test('the main app bundle never reaches the splash', () => {
		const chain = findImportChain(MAIN_APP_ENTRY, SPLASH_MODULE);
		assert.equal(
			chain,
			null,
			`scripts/build.mjs emits dist/main/index.js and dist/main/MainApp.js as two independent bundles, so a module reachable from both is instantiated twice with two copies of its module state. The splash keeps mutable state, so the second copy would take writes that no window ever reads and silently do nothing, which is the defect the ModuleBootHandoff symbol exists to prevent. Main talks to the splash through ModuleBootHandoff instead. Reached it through: ${chain?.join(' -> ')}`,
		);
	});

	test('the shell update decision carries no splash presentation', () => {
		const edges = readRuntimeEdges('main/ShellUpdateCapability.ts');
		assert.equal(
			edges.has(SPLASH_MODULE),
			false,
			'Updater.ts imports this module into the main app bundle, so a splash import here drags the whole splash across the bundle boundary. The presentation lives in ShellUpdateSplash.ts.',
		);
	});
});
