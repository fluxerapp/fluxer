// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFileSync} from 'node:child_process';
import {createRequire} from 'node:module';
import log from 'electron-log';

const requireModule = createRequire(import.meta.url);
const VULKAN_IMPLICIT_LAYERS_REGISTRY_KEY = 'Software\\Khronos\\Vulkan\\ImplicitLayers';
const VULKAN_REGISTRY_ROOTS = ['HKCU', 'HKLM'] as const;

const FLUXER_VULKAN_LAYER_MANIFEST_FILE_NAME = /^fluxer-vulkan-layer\.win32-(?:x64|ia32|arm64)-msvc\.json$/;
const FLUXER_VULKAN_LAYER_PACKAGE_DIRECTORY_NAMES = new Set(['win-game-capture', 'win-screen-capture']);

interface VulkanLayerRegistrationState {
	registered: boolean;
	manifestExists: boolean;
	dllExists: boolean;
	manifestPath: string | null;
}

type WindowsGameCaptureModule = {
	loadError?: Error | null;
	isGameCaptureHookAvailable?: () => boolean;
	registerVulkanLayerManifest?: () => boolean;
	unregisterVulkanLayerManifest?: () => boolean;
	resolveVulkanLayerManifestPath?: () => string | null;
	getVulkanLayerRegistrationState?: () => VulkanLayerRegistrationState;
};

function parseRegistryValueNames(stdout: string): Array<string> {
	const valueNames: Array<string> = [];
	for (const line of stdout.split(/\r?\n/)) {
		const trimmed = line.trim();
		const match = trimmed.match(/^(.*?)\s+REG_DWORD\s+(?:0x[0-9a-f]+|\d+)$/i);
		if (!match) continue;
		const valueName = match[1].trim();
		if (valueName.length > 0) valueNames.push(valueName);
	}
	return valueNames;
}

function normalizeVulkanLayerValueName(valueName: string): string {
	return valueName.replace(/\//g, '\\').toLowerCase();
}

export function isFluxerGameCaptureVulkanLayerValue(valueName: string): boolean {
	const segments = normalizeVulkanLayerValueName(valueName).split('\\');
	const fileName = segments.at(-1) ?? '';
	const packageDirectoryName = segments.at(-2) ?? '';
	if (!FLUXER_VULKAN_LAYER_MANIFEST_FILE_NAME.test(fileName)) return false;
	return FLUXER_VULKAN_LAYER_PACKAGE_DIRECTORY_NAMES.has(packageDirectoryName);
}

function queryVulkanLayerRegistryValues(root: string): Array<string> {
	try {
		const stdout = execFileSync('reg.exe', ['query', `${root}\\${VULKAN_IMPLICIT_LAYERS_REGISTRY_KEY}`], {
			encoding: 'utf8',
			windowsHide: true,
		});
		return parseRegistryValueNames(stdout);
	} catch (error) {
		const status = (error as {status?: number} | null)?.status;
		if (status === 1) return [];
		throw error;
	}
}

function deleteVulkanLayerRegistryValue(root: string, valueName: string): void {
	execFileSync('reg.exe', ['delete', `${root}\\${VULKAN_IMPLICIT_LAYERS_REGISTRY_KEY}`, '/v', valueName, '/f'], {
		stdio: 'ignore',
		windowsHide: true,
	});
}

function isSameVulkanLayerManifestPath(left: string, right: string): boolean {
	return normalizeVulkanLayerValueName(left) === normalizeVulkanLayerValueName(right);
}

export function shouldRemoveStaleFluxerGameCaptureVulkanLayerValue(
	valueName: string,
	keepManifestPath: string | null,
): boolean {
	if (!isFluxerGameCaptureVulkanLayerValue(valueName)) return false;
	return keepManifestPath === null || !isSameVulkanLayerManifestPath(valueName, keepManifestPath);
}

function removeStaleFluxerGameCaptureVulkanLayers(keepManifestPath: string | null): void {
	if (process.platform !== 'win32') return;
	for (const root of VULKAN_REGISTRY_ROOTS) {
		let valueNames: Array<string>;
		try {
			valueNames = queryVulkanLayerRegistryValues(root);
		} catch (error) {
			log.warn('[VulkanGameCaptureLayer] Failed to enumerate Vulkan implicit layer registry values', {root, error});
			continue;
		}
		for (const valueName of valueNames) {
			if (!shouldRemoveStaleFluxerGameCaptureVulkanLayerValue(valueName, keepManifestPath)) continue;
			try {
				deleteVulkanLayerRegistryValue(root, valueName);
			} catch (error) {
				log.warn('[VulkanGameCaptureLayer] Failed to remove stale Fluxer Vulkan layer registry value', {
					root,
					valueName,
					error,
				});
				continue;
			}
			log.info('[VulkanGameCaptureLayer] Removed stale Fluxer Vulkan layer registry value', {root, valueName});
		}
	}
}

function loadWindowsGameCaptureModule(): WindowsGameCaptureModule | null {
	if (process.platform !== 'win32') return null;
	try {
		const addon = requireModule('@fluxer/win-game-capture') as WindowsGameCaptureModule;
		if (addon.loadError) {
			log.warn('[VulkanGameCaptureLayer] Native game capture addon unavailable', addon.loadError);
			return null;
		}
		return addon;
	} catch (error) {
		log.warn('[VulkanGameCaptureLayer] Failed to load the native game capture addon', error);
		return null;
	}
}

export function initializeWindowsVulkanGameCaptureLayer(): void {
	if (process.platform !== 'win32') return;
	const addon = loadWindowsGameCaptureModule();
	if (!addon || addon.isGameCaptureHookAvailable?.() !== true) {
		removeStaleFluxerGameCaptureVulkanLayers(null);
		log.info('[VulkanGameCaptureLayer] Vulkan implicit layer left unregistered; hook-based game capture is disabled');
		return;
	}
	try {
		const manifestPath = addon.resolveVulkanLayerManifestPath?.() ?? null;
		removeStaleFluxerGameCaptureVulkanLayers(manifestPath);
		const registered = addon.registerVulkanLayerManifest?.() ?? false;
		const state = addon.getVulkanLayerRegistrationState?.() ?? null;
		log.info('[VulkanGameCaptureLayer] Vulkan implicit layer registration checked', {
			registered,
			manifestPath,
			state,
		});
	} catch (error) {
		log.warn('[VulkanGameCaptureLayer] Failed to register Vulkan implicit layer', error);
	}
}

export function unregisterWindowsVulkanGameCaptureLayer(): void {
	if (process.platform !== 'win32') return;
	const addon = loadWindowsGameCaptureModule();
	try {
		const unregistered = addon?.unregisterVulkanLayerManifest?.() ?? false;
		log.info('[VulkanGameCaptureLayer] Vulkan implicit layer unregistration attempted', {
			unregistered,
			manifestPath: addon?.resolveVulkanLayerManifestPath?.() ?? null,
		});
	} catch (error) {
		log.warn('[VulkanGameCaptureLayer] Failed to unregister Vulkan implicit layer', error);
	}
	removeStaleFluxerGameCaptureVulkanLayers(null);
}
