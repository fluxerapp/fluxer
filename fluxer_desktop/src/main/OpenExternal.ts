// SPDX-License-Identifier: AGPL-3.0-or-later

import {spawn} from 'node:child_process';
import {userInfo} from 'node:os';
import {APP_PROTOCOL} from '@electron/common/Constants';
import {createChildLogger} from '@electron/common/Logger';
import {shell} from 'electron';

const logger = createChildLogger('OpenExternal');

const DEDUPE_WINDOW_MS = 500;
const ALLOWED_EXTERNAL_URL_PROTOCOLS = new Set([
	'http:',
	'https:',
	'mailto:',
	'tel:',
	'appstream:',
	`${APP_PROTOCOL}:`,
]);
const recentOpens = new Map<string, number>();

function pruneStale(now: number): void {
	for (const [url, timestamp] of recentOpens) {
		if (now - timestamp > DEDUPE_WINDOW_MS) {
			recentOpens.delete(url);
		}
	}
}

function getAppImagePortableEnv(): NodeJS.ProcessEnv | null {
	const appImage = process.env.APPIMAGE;
	if (process.platform !== 'linux' || !appImage) return null;
	const portableHome = process.env.HOME === `${appImage}.home`;
	const portableConfig = process.env.XDG_CONFIG_HOME === `${appImage}.config`;
	if (!portableHome && !portableConfig) return null;
	const env = {...process.env};
	if (portableHome) env.HOME = userInfo().homedir;
	if (portableConfig) delete env.XDG_CONFIG_HOME;
	return env;
}

function openWithXdgOpen(url: string, env: NodeJS.ProcessEnv): Promise<void> {
	return new Promise((resolve, reject) => {
		const child = spawn('xdg-open', [url], {env, detached: true, stdio: 'ignore'});
		child.once('error', reject);
		child.once('exit', (code) => {
			if (code !== 0) logger.warn('xdg-open exited with a failure code', {code});
		});
		child.once('spawn', () => {
			child.unref();
			resolve();
		});
	});
}

export async function openExternalDeduped(url: string): Promise<void> {
	let safeUrl: string | null;
	try {
		const parsed = new URL(url);
		const protocol = parsed.protocol.toLowerCase();
		safeUrl = ALLOWED_EXTERNAL_URL_PROTOCOLS.has(protocol) ? parsed.toString() : null;
	} catch {
		safeUrl = null;
	}
	if (!safeUrl) {
		throw new Error('External URL open request blocked');
	}
	const now = Date.now();
	pruneStale(now);
	const previous = recentOpens.get(safeUrl);
	if (previous !== undefined && now - previous < DEDUPE_WINDOW_MS) {
		recentOpens.set(safeUrl, now);
		return;
	}
	recentOpens.set(safeUrl, now);
	const portableEnv = getAppImagePortableEnv();
	if (portableEnv) {
		await openWithXdgOpen(safeUrl, portableEnv);
		return;
	}
	await shell.openExternal(safeUrl);
}
