// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFile} from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {isFlatpakRuntime} from '@electron/main/LinuxSandbox';

const DARK_LUMINANCE_THRESHOLD = 0.2;
const PORTAL_TIMEOUT_MS = 1500;
const WATCH_DEBOUNCE_MS = 250;
const WATCHED_FILES = new Set(['kdeglobals', 'plasmarc']);
const STYLE_FOLLOWS_COLOUR_SCHEME = new Set(['default', 'breeze']);
const KNOWN_STYLE_DARKNESS: Readonly<Record<string, boolean>> = {
	'breeze-dark': true,
	'breeze-light': false,
	oxygen: false,
};

type IniSections = Map<string, Map<string, string>>;
type Env = Readonly<Record<string, string | undefined>>;

function parseIni(text: string): IniSections {
	const sections: IniSections = new Map();
	let current: Map<string, string> | null = null;
	for (const rawLine of text.split(/\r?\n/u)) {
		const line = rawLine.trim();
		if (line.length === 0 || line.startsWith('#') || line.startsWith(';')) continue;
		const header = /^\[(.+)\]$/u.exec(line);
		if (header) {
			const name = header[1].trim();
			current = sections.get(name) ?? new Map();
			sections.set(name, current);
			continue;
		}
		if (!current) continue;
		const eq = line.indexOf('=');
		if (eq <= 0) continue;
		const key = line
			.slice(0, eq)
			.replace(/\[\$[a-z]+\]$/u, '')
			.trim();
		current.set(key, line.slice(eq + 1).trim());
	}
	return sections;
}

function isDarkRgb(value: string | undefined): boolean | null {
	if (!value) return null;
	const parts = value.split(',').map((part) => Number.parseInt(part.trim(), 10));
	if (parts.length < 3 || parts.slice(0, 3).some((part) => !Number.isFinite(part) || part < 0 || part > 255)) {
		return null;
	}
	const [r, g, b] = parts.map((part) => {
		const channel = part / 255;
		return channel <= 0.04045 ? channel / 12.92 : ((channel + 0.055) / 1.055) ** 2.4;
	});
	return 0.2126 * r + 0.7152 * g + 0.0722 * b < DARK_LUMINANCE_THRESHOLD;
}

function windowBackgroundIsDark(sections: IniSections): boolean | null {
	return isDarkRgb(sections.get('Colors:Window')?.get('BackgroundNormal'));
}

function colourSchemeNameIsDark(sections: IniSections): boolean | null {
	const name = sections.get('General')?.get('ColorScheme');
	if (!name) return null;
	return /dark|night|black/iu.test(name);
}

interface KdePanelSources {
	plasmarc: string | null;
	kdeglobals: string | null;
	styleColors: (style: string) => string | null;
}

function resolveKdePanelDark(sources: KdePanelSources): boolean | null {
	const style = (sources.plasmarc ? parseIni(sources.plasmarc).get('Theme')?.get('name') : undefined) ?? 'default';
	if (!STYLE_FOLLOWS_COLOUR_SCHEME.has(style)) {
		const known = KNOWN_STYLE_DARKNESS[style];
		if (known !== undefined) return known;
		const colors = sources.styleColors(style);
		if (colors) {
			const fixed = windowBackgroundIsDark(parseIni(colors));
			if (fixed !== null) return fixed;
		}
		if (/dark/iu.test(style)) return true;
	}
	if (!sources.kdeglobals) return null;
	const globals = parseIni(sources.kdeglobals);
	return windowBackgroundIsDark(globals) ?? colourSchemeNameIsDark(globals);
}

function kdeConfigDir(env: Env, flatpak: boolean): string {
	const home = env.HOME?.trim() || os.homedir();
	const configured = (flatpak ? env.HOST_XDG_CONFIG_HOME : env.XDG_CONFIG_HOME)?.trim();
	return configured && path.isAbsolute(configured) ? configured : path.join(home, '.config');
}

function kdeDataDirs(env: Env, flatpak: boolean): Array<string> {
	const home = env.HOME?.trim() || os.homedir();
	const dataHome = (flatpak ? env.HOST_XDG_DATA_HOME : env.XDG_DATA_HOME)?.trim();
	const dirs = [dataHome && path.isAbsolute(dataHome) ? dataHome : path.join(home, '.local', 'share')];
	if (!flatpak) {
		const systemDirs = (env.XDG_DATA_DIRS?.trim() || '/usr/local/share:/usr/share').split(':');
		dirs.push(...systemDirs.filter((dir) => path.isAbsolute(dir)));
	}
	return dirs;
}

function readText(file: string): string | null {
	try {
		return fs.readFileSync(file, 'utf8');
	} catch {
		return null;
	}
}

function readPortalWindowBackground(): Promise<string | null> {
	return new Promise((resolve) => {
		execFile(
			'gdbus',
			[
				'call',
				'--session',
				'--dest',
				'org.freedesktop.portal.Desktop',
				'--object-path',
				'/org/freedesktop/portal/desktop',
				'--method',
				'org.freedesktop.portal.Settings.Read',
				'org.kde.kdeglobals.Colors:Window',
				'BackgroundNormal',
			],
			{timeout: PORTAL_TIMEOUT_MS},
			(error, stdout) => {
				if (error) {
					resolve(null);
					return;
				}
				const match = /'(\d{1,3}\s*,\s*\d{1,3}\s*,\s*\d{1,3}(?:\s*,\s*\d{1,3})?)'/u.exec(stdout);
				resolve(match ? match[1] : null);
			},
		);
	});
}

function readKdePanelDarkWith(env: Env, portalWindowBackground: string | null): boolean | null {
	const flatpak = isFlatpakRuntime();
	const configDir = kdeConfigDir(env, flatpak);
	const dataDirs = kdeDataDirs(env, flatpak);
	let kdeglobals = readText(path.join(configDir, 'kdeglobals'));
	if (portalWindowBackground) {
		kdeglobals = `${kdeglobals ?? ''}\n[Colors:Window]\nBackgroundNormal=${portalWindowBackground}\n`;
	}
	return resolveKdePanelDark({
		plasmarc: readText(path.join(configDir, 'plasmarc')),
		kdeglobals,
		styleColors: (style) => {
			if (style.includes('/') || style.includes('\\') || style.startsWith('.')) return null;
			for (const dir of dataDirs) {
				const colors = readText(path.join(dir, 'plasma', 'desktoptheme', style, 'colors'));
				if (colors) return colors;
			}
			return null;
		},
	});
}

export function readKdePanelDarkSync(env: Env = process.env): boolean | null {
	return readKdePanelDarkWith(env, null);
}

export async function readKdePanelDark(env: Env = process.env): Promise<boolean | null> {
	const portal = isFlatpakRuntime() ? await readPortalWindowBackground() : null;
	return readKdePanelDarkWith(env, portal);
}

export function watchKdePanelTheme(onChange: () => void, env: Env = process.env): () => void {
	const dir = kdeConfigDir(env, isFlatpakRuntime());
	let timer: NodeJS.Timeout | null = null;
	let watcher: fs.FSWatcher | null = null;
	try {
		watcher = fs.watch(dir, {persistent: false}, (_event, filename) => {
			if (filename && !WATCHED_FILES.has(filename.toString())) return;
			if (timer) clearTimeout(timer);
			timer = setTimeout(() => {
				timer = null;
				onChange();
			}, WATCH_DEBOUNCE_MS);
		});
		watcher.on('error', () => watcher?.close());
	} catch {
		watcher = null;
	}
	return () => {
		if (timer) clearTimeout(timer);
		watcher?.close();
	};
}
