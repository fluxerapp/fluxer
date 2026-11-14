// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {app} from 'electron';
import log from 'electron-log';

const APPLY_STATE_FILE_NAME = 'update-apply-state.json';

export type VelopackApplyAttempt = {
	version: string;
	attemptedAt: number;
};

function getApplyStatePath(): string {
	return path.join(app.getPath('userData'), APPLY_STATE_FILE_NAME);
}

export function readVelopackApplyAttempt(): VelopackApplyAttempt | null {
	let raw: string;
	try {
		raw = fs.readFileSync(getApplyStatePath(), 'utf8');
	} catch {
		return null;
	}
	try {
		const parsed = JSON.parse(raw) as {version?: unknown; attemptedAt?: unknown};
		if (typeof parsed.version !== 'string' || parsed.version.length === 0) {
			return null;
		}
		return {
			version: parsed.version,
			attemptedAt: typeof parsed.attemptedAt === 'number' ? parsed.attemptedAt : 0,
		};
	} catch (error) {
		log.warn('Failed to parse the recorded update apply attempt', error);
		return null;
	}
}

export function recordVelopackApplyAttempt(version: string): void {
	const payload: VelopackApplyAttempt = {version, attemptedAt: Date.now()};
	try {
		fs.writeFileSync(getApplyStatePath(), JSON.stringify(payload), 'utf8');
	} catch (error) {
		log.warn('Failed to record the update apply attempt', error);
	}
}

export function clearVelopackApplyAttempt(): void {
	try {
		fs.rmSync(getApplyStatePath(), {force: true});
	} catch (error) {
		log.warn('Failed to clear the recorded update apply attempt', error);
	}
}
