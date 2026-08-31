// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawnSync} from 'node:child_process';

function tail(value: string, lines: number): string {
	return value
		.split('\n')
		.map((line) => line.trimEnd())
		.filter((line) => line.length > 0)
		.slice(-lines)
		.join('\n');
}

export function startDockerContainer(args: ReadonlyArray<string>): void {
	const result = spawnSync('docker', args, {encoding: 'utf8'});
	if (result.error) {
		throw new Error(`failed to run docker: ${result.error.message}\n  docker ${args.join(' ')}`);
	}
	if (result.status === 0) {
		return;
	}
	const stderr = tail(result.stderr ?? '', 5);
	const stdout = tail(result.stdout ?? '', 3);
	const detail = stderr || stdout || 'no output';
	throw new Error(
		`docker exited ${result.status} while starting a test container.\n  docker ${args.join(' ')}\n  ${detail}`,
	);
}
