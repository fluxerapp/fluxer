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

async function listRepoFiles() {
	const url = `https://api.github.com/repos/${repo}/git/trees/${encodeURIComponent(ref)}?recursive=1`;
	const res = await fetch(url, {
		headers: {
			Accept: 'application/vnd.github+json',
			'User-Agent': 'fluxer-detectables-sync',
		},
	});
	if (!res.ok) {
		throw new Error(`Failed ${url}: ${res.status}`);
	}
	const payload = await res.json();
	if (payload.truncated) {
		throw new Error(`Detectables tree for ${repo}@${ref} is truncated`);
	}
	return (payload.tree ?? [])
		.filter((entry) => entry.type === 'blob' && typeof entry.path === 'string')
		.map((entry) => entry.path);
}

async function download(file) {
	const url = `${base}/${file}`;
	const dest = path.join(ASSETS_DIR, file);
	const res = await fetch(url);
	if (!res.ok) {
		throw new Error(`Failed ${url}: ${res.status}`);
	}
	fs.mkdirSync(path.dirname(dest), {recursive: true});
	fs.writeFileSync(dest, Buffer.from(await res.arrayBuffer()));
	console.log(`Synced ${file} (${dest})`);
}

for (const entry of fs.readdirSync(ASSETS_DIR)) {
	if (entry === path.basename(LOCK_PATH)) continue;
	fs.rmSync(path.join(ASSETS_DIR, entry), {recursive: true, force: true});
}

for (const file of await listRepoFiles()) {
	await download(file);
}
