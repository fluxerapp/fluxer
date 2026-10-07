// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

export const MODULE_SYSTEM_BUILD_ENABLED = process.env.FLUXER_MODULES === '1';

const OFFLINE_RENDERER_DIRECTORY_SEGMENTS: ReadonlyArray<string> = Object.freeze(['..', 'renderer']);
const OFFLINE_RENDERER_INDEX_NAME = 'index.html';

const MODULE_SYSTEM_ENV = 'FLUXER_MODULE_SYSTEM';
const MODULE_SYSTEM_DISABLE_ARGUMENT = '--fluxer-no-module-system';
const FALSY_VALUES: ReadonlySet<string> = new Set(['0', 'false', 'no', 'off']);

export function readModuleSystemDisableRequest(
	argv: ReadonlyArray<string> = process.argv,
	env: NodeJS.ProcessEnv = process.env,
): boolean {
	if (argv.includes(MODULE_SYSTEM_DISABLE_ARGUMENT)) {
		return true;
	}
	const value = env[MODULE_SYSTEM_ENV]?.trim().toLowerCase();
	return value != null && FALSY_VALUES.has(value);
}

export const ModuleSystemDisableDecision = Object.freeze({
	NOT_REQUESTED: 'not-requested',
	HONOURED: 'honoured',
	IGNORED_WITHOUT_OFFLINE_RENDERER: 'ignored-without-offline-renderer',
} as const);

export type ModuleSystemDisableDecision =
	| {readonly kind: typeof ModuleSystemDisableDecision.NOT_REQUESTED}
	| {readonly kind: typeof ModuleSystemDisableDecision.HONOURED}
	| {readonly kind: typeof ModuleSystemDisableDecision.IGNORED_WITHOUT_OFFLINE_RENDERER};

export const ModuleSystemLaunchDecision = Object.freeze({
	DISABLED_BY_BUILD: 'disabled-by-build',
	DISABLED_BY_REQUEST: 'disabled-by-request',
	ENABLED: 'enabled',
	ENABLED_WITH_IGNORED_DISABLE_REQUEST: 'enabled-with-ignored-disable-request',
} as const);

export type ModuleSystemLaunchDecision =
	| {readonly kind: typeof ModuleSystemLaunchDecision.DISABLED_BY_BUILD}
	| {readonly kind: typeof ModuleSystemLaunchDecision.DISABLED_BY_REQUEST}
	| {readonly kind: typeof ModuleSystemLaunchDecision.ENABLED}
	| {readonly kind: typeof ModuleSystemLaunchDecision.ENABLED_WITH_IGNORED_DISABLE_REQUEST};

export function decideModuleSystemDisableRequest(
	requested: boolean,
	hasOfflineRenderer: boolean,
): ModuleSystemDisableDecision {
	if (!requested) {
		return {kind: ModuleSystemDisableDecision.NOT_REQUESTED};
	}
	if (hasOfflineRenderer) {
		return {kind: ModuleSystemDisableDecision.HONOURED};
	}
	return {kind: ModuleSystemDisableDecision.IGNORED_WITHOUT_OFFLINE_RENDERER};
}

export function resolveModuleSystemLaunch({
	argv = process.argv,
	env = process.env,
	hasOfflineRenderer,
}: {
	readonly argv?: ReadonlyArray<string>;
	readonly env?: NodeJS.ProcessEnv;
	readonly hasOfflineRenderer: boolean;
}): ModuleSystemLaunchDecision {
	if (!MODULE_SYSTEM_BUILD_ENABLED) {
		return {kind: ModuleSystemLaunchDecision.DISABLED_BY_BUILD};
	}
	const disable = decideModuleSystemDisableRequest(readModuleSystemDisableRequest(argv, env), hasOfflineRenderer);
	switch (disable.kind) {
		case ModuleSystemDisableDecision.NOT_REQUESTED:
			return {kind: ModuleSystemLaunchDecision.ENABLED};
		case ModuleSystemDisableDecision.HONOURED:
			return {kind: ModuleSystemLaunchDecision.DISABLED_BY_REQUEST};
		case ModuleSystemDisableDecision.IGNORED_WITHOUT_OFFLINE_RENDERER:
			return {kind: ModuleSystemLaunchDecision.ENABLED_WITH_IGNORED_DISABLE_REQUEST};
	}
}

export function getOfflineRendererRoot(mainModuleUrl: string): string {
	return path.join(path.dirname(fileURLToPath(mainModuleUrl)), ...OFFLINE_RENDERER_DIRECTORY_SEGMENTS);
}

export function hasOfflineRenderer(mainModuleUrl: string): boolean {
	try {
		return fs.statSync(path.join(getOfflineRendererRoot(mainModuleUrl), OFFLINE_RENDERER_INDEX_NAME)).isFile();
	} catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
			return false;
		}
		throw error;
	}
}
