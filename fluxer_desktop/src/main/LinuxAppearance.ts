// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {createChildLogger} from '@electron/common/Logger';
import type {LinuxAppearanceSnapshot} from '@electron/common/Types';
import {getLinuxPortalsMode} from '@electron/main/LaunchOptions';
import {loadOptionalNativeAddon} from '@electron/main/NativeAddon';

export type {LinuxAppearanceSnapshot} from '@electron/common/Types';

const logger = createChildLogger('LinuxAppearance');
const requireModule = createRequire(import.meta.url);

interface SettingsChangeEvent {
	namespace: string;
	key: string;
	uint32?: number;
	accent?: {r: number; g: number; b: number};
}

interface SettingsCtor {
	new (onChange: (event: SettingsChangeEvent) => void): {close: () => void};
}

interface PortalsModule {
	readColorScheme: () => 'no-preference' | 'prefer-dark' | 'prefer-light';
	readContrast: () => 'no-preference' | 'high';
	readAccentColor: () => {r: number; g: number; b: number} | null;
	Settings: SettingsCtor | null;
	loadError: Error | null;
}

export function readLinuxAppearance(): LinuxAppearanceSnapshot {
	const mod = loadAddon();
	if (!mod) {
		return {colorScheme: 'no-preference', contrast: 'no-preference', accent: null};
	}
	return {
		colorScheme: mod.readColorScheme(),
		contrast: mod.readContrast(),
		accent: mod.readAccentColor(),
	};
}

type LinuxAppearanceListener = (snapshot: LinuxAppearanceSnapshot) => void;

export interface LinuxAppearanceSubscription {
	close(): void;
}

export function subscribeLinuxAppearance(listener: LinuxAppearanceListener): LinuxAppearanceSubscription | null {
	const mod = loadAddon();
	if (!mod?.Settings) return null;
	let closed = false;
	let instance: {close: () => void} | null = null;
	try {
		instance = new mod.Settings((event) => {
			if (closed) return;
			const snapshot: LinuxAppearanceSnapshot = {
				colorScheme: mod.readColorScheme(),
				contrast: mod.readContrast(),
				accent: mod.readAccentColor(),
			};
			if (event.namespace !== 'org.freedesktop.appearance') {
				listener(snapshot);
				return;
			}
			if (event.key === 'color-scheme' && event.uint32 != null) {
				snapshot.colorScheme = 'no-preference';
				if (event.uint32 === 1) {
					snapshot.colorScheme = 'prefer-dark';
				} else if (event.uint32 === 2) {
					snapshot.colorScheme = 'prefer-light';
				}
			} else if (event.key === 'contrast' && event.uint32 != null) {
				snapshot.contrast = event.uint32 === 1 ? 'high' : 'no-preference';
			} else if (event.key === 'accent-color') {
				snapshot.accent =
					event.accent && event.accent.r >= 0 && event.accent.g >= 0 && event.accent.b >= 0 ? event.accent : null;
			}
			listener(snapshot);
		});
	} catch (error) {
		logger.warn('linux-portals Settings subscribe failed', {error});
		return null;
	}
	return {
		close: () => {
			if (closed) return;
			closed = true;
			try {
				instance?.close();
			} catch (error) {
				logger.warn('linux-portals Settings close threw', {error});
			}
		},
	};
}

const loadAddon = loadOptionalNativeAddon<PortalsModule>({
	platform: 'linux',
	load: () => requireModule('@fluxer/linux-portals'),
	logger: logger,
	level: 'info',
	unavailableMessage: '@fluxer/linux-portals not available; appearance reads disabled',
	disabled: () => getLinuxPortalsMode(process.argv) === 'off',
	disabledMessage: '@fluxer/linux-portals disabled by launch diagnostics; appearance reads disabled',
});
