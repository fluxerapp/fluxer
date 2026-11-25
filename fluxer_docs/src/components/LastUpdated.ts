// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';
import path from 'node:path';

const cache = new Map<string, Date | undefined>();

function readCommitDate(file: string): Date | undefined {
	const result = spawnSync(
		'git',
		['-c', 'log.showSignature=false', 'log', '-1', '--format=%ct', '--', path.basename(file)],
		{cwd: path.dirname(file), encoding: 'utf-8'},
	);
	if (result.error !== undefined || result.status !== 0) {
		return undefined;
	}
	const seconds = Number.parseInt(result.stdout.trim(), 10);
	if (!Number.isFinite(seconds)) {
		return undefined;
	}
	return new Date(seconds * 1000);
}

export function lastCommitDate(file: string): Date | undefined {
	const absolute = path.resolve(file);
	if (!cache.has(absolute)) {
		cache.set(absolute, readCommitDate(absolute));
	}
	return cache.get(absolute);
}
