// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import type {LinuxDesktopKind, LinuxSessionType} from '@electron/common/Types';

type SessionEnv = Readonly<Record<string, string | undefined>>;

export function detectLinuxSessionType(env: SessionEnv, pathExists: (candidate: string) => boolean): LinuxSessionType {
	const sessionType = (env.XDG_SESSION_TYPE ?? '').trim().toLowerCase();
	if (sessionType === 'x11' || sessionType === 'wayland') return sessionType;
	const waylandDisplay = (env.WAYLAND_DISPLAY ?? '').trim();
	if (waylandDisplay.length > 0) {
		const runtimeDir = (env.XDG_RUNTIME_DIR ?? '').trim();
		const socketPath = waylandDisplay.startsWith('/')
			? waylandDisplay
			: runtimeDir.length > 0
				? `${runtimeDir.replace(/\/+$/, '')}/${waylandDisplay}`
				: null;
		if (socketPath !== null && pathExists(socketPath)) return 'wayland';
	}
	if ((env.DISPLAY ?? '').trim().length > 0) return 'x11';
	return 'unknown';
}

export function detectLinuxDesktop(env: SessionEnv): LinuxDesktopKind {
	const tokens = new Set((env.XDG_CURRENT_DESKTOP ?? '').split(':').map((token) => token.trim().toLowerCase()));
	if (tokens.has('kde')) return 'kde';
	if (tokens.has('hyprland') || (env.HYPRLAND_INSTANCE_SIGNATURE ?? '').trim().length > 0) return 'hyprland';
	if (tokens.has('gnome')) return 'gnome';
	return 'other';
}

export function isKdePlasma5Session(env: SessionEnv): boolean {
	return (env.KDE_SESSION_VERSION ?? '').trim() === '5';
}

function pathExists(candidate: string): boolean {
	try {
		return fs.existsSync(candidate);
	} catch {
		return false;
	}
}

export function getLinuxSessionType(): LinuxSessionType {
	if (process.platform !== 'linux') return 'unknown';
	return detectLinuxSessionType(process.env, pathExists);
}

export function isWaylandSession(): boolean {
	return getLinuxSessionType() === 'wayland';
}

export function isX11Session(): boolean {
	return getLinuxSessionType() === 'x11';
}

export function getLinuxDesktop(): LinuxDesktopKind {
	return detectLinuxDesktop(process.env);
}
