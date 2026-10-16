// SPDX-License-Identifier: AGPL-3.0-or-later

import {execFileSync} from 'node:child_process';
import log from 'electron-log';

const VULKAN_IMPLICIT_LAYERS_REGISTRY_KEY = 'Software\\Khronos\\Vulkan\\ImplicitLayers';
const VULKAN_REGISTRY_ROOTS = ['HKCU', 'HKLM'] as const;

const FLUXER_VULKAN_LAYER_MANIFEST_FILE_NAME = /^fluxer-vulkan-layer\.win32-(?:x64|ia32|arm64)-msvc\.json$/;
const FLUXER_VULKAN_LAYER_PACKAGE_DIRECTORY_NAMES = new Set(['win-game-capture', 'win-screen-capture']);

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

function isFluxerVulkanLayerValue(valueName: string): boolean {
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

export function removeFluxerVulkanLayerRegistrations(): void {
	if (process.platform !== 'win32') return;
	for (const root of VULKAN_REGISTRY_ROOTS) {
		let valueNames: Array<string>;
		try {
			valueNames = queryVulkanLayerRegistryValues(root);
		} catch (error) {
			log.warn('[VulkanLayerCleanup] Failed to enumerate Vulkan implicit layer registry values', {root, error});
			continue;
		}
		for (const valueName of valueNames) {
			if (!isFluxerVulkanLayerValue(valueName)) continue;
			try {
				deleteVulkanLayerRegistryValue(root, valueName);
			} catch (error) {
				log.warn('[VulkanLayerCleanup] Failed to remove Fluxer Vulkan layer registry value', {
					root,
					valueName,
					error,
				});
				continue;
			}
			log.info('[VulkanLayerCleanup] Removed Fluxer Vulkan layer registry value', {root, valueName});
		}
	}
}
