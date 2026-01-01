/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {execSync} from 'node:child_process';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import * as esbuild from 'esbuild';

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(scriptDir, '..');

const allowedChannels = new Set(['stable', 'canary']);
const rawChannel = process.env.BUILD_CHANNEL?.toLowerCase() ?? '';
const channel = allowedChannels.has(rawChannel) ? rawChannel : 'stable';

console.log(`Building Electron with channel: ${channel}`);

execSync('node scripts/set-build-channel.mjs', {cwd: projectRoot, stdio: 'inherit'});

execSync('npx tsc -p tsconfig.electron.json', {cwd: projectRoot, stdio: 'inherit'});

await esbuild.build({
	entryPoints: [path.join(projectRoot, 'src-electron/preload/index.ts')],
	bundle: true,
	platform: 'node',
	target: 'node18',
	format: 'cjs',
	outfile: path.join(projectRoot, 'src-electron/dist/preload/index.js'),
	external: ['electron'],
	define: {
		'process.env.BUILD_CHANNEL': JSON.stringify(channel),
	},
	sourcemap: true,
});

console.log('Electron build complete');
