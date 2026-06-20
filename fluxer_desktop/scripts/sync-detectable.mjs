// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';

const ROOT = path.resolve(import.meta.dirname, '..');
const ASSETS_DIR = path.join(ROOT, 'assets', 'rpc');
const LOCK_PATH = path.join(ASSETS_DIR, 'detectable-lock.json');

const lock = JSON.parse(fs.readFileSync(LOCK_PATH, 'utf8'));
const repo = lock.repo ?? 'fluxerapp/detectables';
const ref = lock.ref ?? 'main';
const base = `https://raw.githubusercontent.com/${repo}/${ref}`;

for (const file of lock.files) {
	const url = `${base}/${file}`;
	const dest = path.join(ASSETS_DIR, file);
	const res = await fetch(url);
	if (!res.ok) {
		throw new Error(`Failed ${url}: ${res.status}`);
	}
	fs.writeFileSync(dest, Buffer.from(await res.arrayBuffer()));
	console.log(`Synced ${file} (${dest})`);
}
