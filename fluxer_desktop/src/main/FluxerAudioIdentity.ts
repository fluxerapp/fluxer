// SPDX-License-Identifier: AGPL-3.0-or-later

import {basename} from 'node:path';
import type {VirtmicNode} from '@electron/common/Types';
import {app, webContents} from 'electron';

const FLUXER_AUDIO_DISPLAY_IDENTITY_KEYS = ['application.name', 'node.name', 'node.nick', 'node.description'] as const;
const FALLBACK_PRODUCT_NAMES = ['Fluxer', 'Fluxer Canary', 'Fluxer Development'];
const FLUXER_AUDIO_PREFIXES = ['fluxer ', 'fluxer-', 'fluxer_', 'fluxer.'];

function stripDesktopBinarySuffix(name: string): string | null {
	const suffixes = ['.AppImage', '.bin', '.exe'];
	for (const suffix of suffixes) {
		if (name.toLowerCase().endsWith(suffix.toLowerCase())) {
			return name.slice(0, -suffix.length);
		}
	}
	return null;
}

function addNonEmpty(out: Set<string>, value: unknown): void {
	if (typeof value !== 'string') return;
	const trimmed = value.trim();
	if (trimmed.length > 0) out.add(trimmed);
}

function getFluxerAudioProcessPids(): Array<string> {
	const pids = new Set<string>();
	const addPid = (pid: unknown): void => {
		if (typeof pid === 'number' && Number.isFinite(pid) && pid > 0) {
			pids.add(String(pid));
		}
	};
	addPid(process.pid);
	try {
		for (const metric of app.getAppMetrics()) {
			addPid(metric.pid);
		}
	} catch {}
	try {
		for (const contents of webContents.getAllWebContents()) {
			if (contents.isDestroyed()) continue;
			addPid(contents.getOSProcessId());
		}
	} catch {}
	return Array.from(pids);
}

function getFluxerAudioBinaryNames(): Array<string> {
	const names = new Set<string>();
	try {
		const exePath = app.getPath('exe');
		const exeBase = exePath ? basename(exePath) : '';
		addNonEmpty(names, exeBase);
		const stripped = exeBase ? stripDesktopBinarySuffix(exeBase) : null;
		addNonEmpty(names, stripped);
	} catch {}
	return Array.from(names);
}

function getFluxerAudioDisplayNames(): Array<string> {
	const names = new Set<string>();
	for (const name of FALLBACK_PRODUCT_NAMES) addNonEmpty(names, name);
	try {
		addNonEmpty(names, app.getName());
	} catch {}
	for (const binary of getFluxerAudioBinaryNames()) addNonEmpty(names, binary);
	return Array.from(names);
}

function matchesKnownDisplayIdentity(value: string, names: ReadonlyArray<string>): boolean {
	const normalized = value.trim().toLowerCase();
	if (!normalized) return false;
	if (names.some((name) => normalized === name.toLowerCase())) return true;
	return FLUXER_AUDIO_PREFIXES.some((prefix) => normalized.startsWith(prefix));
}

export function isFluxerAudioNode(node: VirtmicNode): boolean {
	const pids = new Set(getFluxerAudioProcessPids());
	const binaryNames = getFluxerAudioBinaryNames();
	const displayNames = getFluxerAudioDisplayNames();
	const processId = node['application.process.id'] ?? node['pipewire.sec.pid'];
	if (processId && pids.has(processId)) return true;
	const binary = node['application.process.binary'];
	if (binary && binaryNames.some((name) => binary.toLowerCase() === name.toLowerCase())) return true;
	for (const key of FLUXER_AUDIO_DISPLAY_IDENTITY_KEYS) {
		const value = node[key];
		if (value && matchesKnownDisplayIdentity(value, displayNames)) return true;
	}
	return false;
}

export function isKnownFluxerAudioProcessPid(pid: number): boolean {
	return getFluxerAudioProcessPids().includes(String(pid));
}

export function buildFluxerAudioExcludePatterns(maxPatterns = 32): Array<VirtmicNode> {
	const patterns: Array<VirtmicNode> = [];
	const append = (pattern: VirtmicNode): void => {
		if (patterns.length >= maxPatterns) return;
		if (Object.keys(pattern).length === 0) return;
		const key = JSON.stringify(Object.entries(pattern).sort(([a], [b]) => a.localeCompare(b)));
		if (
			!patterns.some(
				(existing) => JSON.stringify(Object.entries(existing).sort(([a], [b]) => a.localeCompare(b))) === key,
			)
		) {
			patterns.push(pattern);
		}
	};
	for (const pid of getFluxerAudioProcessPids()) {
		append({'application.process.id': pid});
		append({'pipewire.sec.pid': pid});
	}
	for (const binary of getFluxerAudioBinaryNames()) {
		append({'application.process.binary': binary});
	}
	for (const name of getFluxerAudioDisplayNames()) {
		for (const key of FLUXER_AUDIO_DISPLAY_IDENTITY_KEYS) {
			append({[key]: name});
		}
	}
	return patterns;
}
