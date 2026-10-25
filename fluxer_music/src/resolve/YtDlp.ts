// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawn} from 'node:child_process';
import fs from 'node:fs';
import {MusicError} from '../Errors';

export type YtDlpRunner = (args: Array<string>, opts?: {timeoutMs?: number}) => Promise<string>;

const DEFAULT_TIMEOUT_MS = 30_000;

export function createYtDlpRunner(opts: {bin: string; cookiesFile: string | null}): YtDlpRunner {
	return (args, runOpts) =>
		new Promise<string>((resolve, reject) => {
			const fixed = ['--no-warnings', '--js-runtimes', 'node'];
			if (opts.cookiesFile && fs.existsSync(opts.cookiesFile)) fixed.push('--cookies', opts.cookiesFile);

			const child = spawn(opts.bin, [...fixed, ...args], {shell: false, stdio: ['ignore', 'pipe', 'pipe']});
			let stdout = '';
			let stderr = '';
			let settled = false;
			const fail = (reason: string) => {
				if (settled) return;
				settled = true;
				clearTimeout(timer);
				console.error(`yt-dlp failed: ${reason}`);
				reject(new MusicError('SOURCE_FAILED'));
			};
			const timer = setTimeout(() => {
				child.kill('SIGKILL');
				fail('timed out');
			}, runOpts?.timeoutMs ?? DEFAULT_TIMEOUT_MS);

			child.stdout.setEncoding('utf8').on('data', (d: string) => {
				stdout += d;
			});
			child.stderr.setEncoding('utf8').on('data', (d: string) => {
				stderr += d;
			});
			child.on('error', (err) => fail(err.message));
			child.on('close', (code) => {
				if (code === 0) {
					if (settled) return;
					settled = true;
					clearTimeout(timer);
					resolve(stdout);
					return;
				}
				const last = stderr.trim().split('\n').at(-1) ?? '';
				fail(`exit ${code}: ${last}`);
			});
		});
}
