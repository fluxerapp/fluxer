// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {getModuleStoreRoot, writeFileAtomically} from '@electron/main/ModuleStore';

export const INSTANCE_MODULE_PREFERENCE_FIELD = 'desktop_modules_enabled';

const INSTANCE_MODULE_PREFERENCE_FILE_NAME = 'instance-switch.json';
const INSTANCE_MODULE_PREFERENCE_FILE_MODE = 0o600;
const INSTANCE_MODULE_PREFERENCE_MAX_BYTES = 4096;

export function getInstanceModulePreferencePath(userDataPath: string): string {
	return path.join(getModuleStoreRoot(userDataPath), INSTANCE_MODULE_PREFERENCE_FILE_NAME);
}

export function instanceTurnedModulesOff(userDataPath: string): boolean {
	let raw: string;
	try {
		raw = fs.readFileSync(getInstanceModulePreferencePath(userDataPath), 'utf8');
	} catch {
		return false;
	}
	if (raw.length > INSTANCE_MODULE_PREFERENCE_MAX_BYTES) {
		return false;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch {
		return false;
	}
	if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) {
		return false;
	}
	return (parsed as Record<string, unknown>)[INSTANCE_MODULE_PREFERENCE_FIELD] === false;
}

export async function recordInstanceModulePreference(userDataPath: string, enabled: boolean): Promise<void> {
	const target = getInstanceModulePreferencePath(userDataPath);
	await fs.promises.mkdir(path.dirname(target), {recursive: true});
	await writeFileAtomically(
		target,
		`${JSON.stringify({[INSTANCE_MODULE_PREFERENCE_FIELD]: enabled})}\n`,
		INSTANCE_MODULE_PREFERENCE_FILE_MODE,
	);
}
