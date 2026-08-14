// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import test from 'node:test';

import {
	isFluxerGameCaptureVulkanLayerValue,
	shouldRemoveStaleFluxerGameCaptureVulkanLayerValue,
} from './WindowsVulkanGameCaptureLayer';

const INSTALLED_MANIFEST_PATH =
	'C:\\Users\\hampus\\AppData\\Local\\fluxer_desktop_canary\\current\\resources\\app.asar.unpacked\\node_modules\\@fluxer\\win-game-capture\\fluxer-vulkan-layer.win32-arm64-msvc.json';
const DEV_TREE_MANIFEST_PATH =
	'C:\\t\\fluxer\\fluxer_desktop\\native\\win-game-capture\\fluxer-vulkan-layer.win32-arm64-msvc.json';
const RETIRED_PACKAGE_MANIFEST_PATH =
	'C:\\Users\\hampus\\AppData\\Local\\fluxer_desktop\\current\\resources\\app.asar.unpacked\\node_modules\\@fluxer\\win-screen-capture\\fluxer-vulkan-layer.win32-arm64-msvc.json';

test('isFluxerGameCaptureVulkanLayerValue matches the installed @fluxer/win-game-capture manifest', () => {
	assert.ok(isFluxerGameCaptureVulkanLayerValue(INSTALLED_MANIFEST_PATH));
});

test('isFluxerGameCaptureVulkanLayerValue matches a dev/build tree manifest outside node_modules/@fluxer', () => {
	assert.ok(isFluxerGameCaptureVulkanLayerValue(DEV_TREE_MANIFEST_PATH));
});

test('isFluxerGameCaptureVulkanLayerValue matches the retired win-screen-capture package name', () => {
	assert.ok(isFluxerGameCaptureVulkanLayerValue(RETIRED_PACKAGE_MANIFEST_PATH));
});

test('isFluxerGameCaptureVulkanLayerValue matches every supported architecture and registry slash style', () => {
	for (const arch of ['x64', 'ia32', 'arm64']) {
		assert.ok(
			isFluxerGameCaptureVulkanLayerValue(
				`C:\\t\\fluxer\\native\\win-game-capture\\fluxer-vulkan-layer.win32-${arch}-msvc.json`,
			),
			arch,
		);
	}
	assert.ok(
		isFluxerGameCaptureVulkanLayerValue(
			'C:/t/fluxer/fluxer_desktop/native/win-game-capture/Fluxer-Vulkan-Layer.win32-ARM64-msvc.json',
		),
	);
});

test('isFluxerGameCaptureVulkanLayerValue never matches another vendor Vulkan implicit layer', () => {
	const foreignValues = [
		'C:\\Program Files\\NVIDIA Corporation\\Nsight Graphics\\nvoglv64.json',
		'C:\\Program Files (x86)\\Steam\\steamoverlayvulkanlayer.json',
		'C:\\Program Files\\obs-studio\\data\\obs-plugins\\win-capture\\obs-vulkan64.json',
		'C:\\Windows\\System32\\VkLayer_khronos_validation.json',
		'C:\\Program Files\\OtherVendor\\layers\\fluxer-vulkan-layer.win32-x64-msvc.json',
		'C:\\t\\fluxer\\fluxer_desktop\\native\\win-game-capture\\othervendor-vulkan-layer.json',
		'C:\\t\\fluxer\\fluxer_desktop\\native\\linux-screen-capture\\fluxer-vulkan-layer.win32-x64-msvc.json',
		'C:\\t\\fluxer\\fluxer_desktop\\native\\mac-screen-capture\\fluxer-vulkan-layer.win32-x64-msvc.json',
		'C:\\t\\fluxer\\fluxer_desktop\\native\\win-game-capture\\fluxer-vulkan-layer.win32-arm64-msvc.dll',
	];
	for (const valueName of foreignValues) {
		assert.equal(isFluxerGameCaptureVulkanLayerValue(valueName), false, valueName);
	}
});

test('shouldRemoveStaleFluxerGameCaptureVulkanLayerValue keeps the live registration of the running install', () => {
	assert.equal(
		shouldRemoveStaleFluxerGameCaptureVulkanLayerValue(INSTALLED_MANIFEST_PATH, INSTALLED_MANIFEST_PATH),
		false,
	);
	assert.equal(
		shouldRemoveStaleFluxerGameCaptureVulkanLayerValue(
			INSTALLED_MANIFEST_PATH.replace(/\\/g, '/').toUpperCase(),
			INSTALLED_MANIFEST_PATH,
		),
		false,
	);
});

test('shouldRemoveStaleFluxerGameCaptureVulkanLayerValue removes orphaned registrations alongside the kept one', () => {
	assert.ok(shouldRemoveStaleFluxerGameCaptureVulkanLayerValue(DEV_TREE_MANIFEST_PATH, INSTALLED_MANIFEST_PATH));
	assert.ok(shouldRemoveStaleFluxerGameCaptureVulkanLayerValue(RETIRED_PACKAGE_MANIFEST_PATH, INSTALLED_MANIFEST_PATH));
});

test('shouldRemoveStaleFluxerGameCaptureVulkanLayerValue removes every Fluxer layer when nothing is kept', () => {
	for (const valueName of [INSTALLED_MANIFEST_PATH, DEV_TREE_MANIFEST_PATH, RETIRED_PACKAGE_MANIFEST_PATH]) {
		assert.ok(shouldRemoveStaleFluxerGameCaptureVulkanLayerValue(valueName, null), valueName);
	}
});

test('shouldRemoveStaleFluxerGameCaptureVulkanLayerValue never removes a foreign vendor layer, kept path or not', () => {
	const foreignValue = 'C:\\Program Files (x86)\\Steam\\steamoverlayvulkanlayer.json';
	assert.equal(shouldRemoveStaleFluxerGameCaptureVulkanLayerValue(foreignValue, null), false);
	assert.equal(shouldRemoveStaleFluxerGameCaptureVulkanLayerValue(foreignValue, INSTALLED_MANIFEST_PATH), false);
});
