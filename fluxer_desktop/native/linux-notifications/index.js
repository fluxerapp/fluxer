// SPDX-License-Identifier: AGPL-3.0-or-later

const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const MODULE_NAME = '@fluxer/linux-notifications';
const SKIP_NATIVE_PROBE_ENV = 'FLUXER_LINUX_NOTIFICATIONS_SKIP_NATIVE_PROBE';
const REQUIRED_FREEDESKTOP_METHODS = ['notify', 'closeNotification', 'getServerCapabilities', 'close'];

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName() {
	if (process.platform !== 'linux') {
		throw new Error(`@fluxer/linux-notifications is only supported on Linux, got ${process.platform}`);
	}
	switch (process.arch) {
		case 'x64':
			return 'linux-notifications.linux-x64-gnu.node';
		case 'arm64':
			return 'linux-notifications.linux-arm64-gnu.node';
		default:
			throw new Error(`Unsupported Linux architecture: ${process.arch}`);
	}
}

function ownPropertyNames(value) {
	if ((typeof value !== 'object' && typeof value !== 'function') || value == null) return [];
	return Object.getOwnPropertyNames(value).sort();
}

function bindingSurface(bindingValue) {
	const freedesktopNotifications = bindingValue ? bindingValue.FreedesktopNotifications : null;
	const prototype = freedesktopNotifications ? freedesktopNotifications.prototype : null;
	return {
		bindingKeys: ownPropertyNames(bindingValue),
		freedesktopNotificationsType: typeof freedesktopNotifications,
		freedesktopNotificationsName: freedesktopNotifications ? freedesktopNotifications.name : null,
		freedesktopNotificationsPrototypeKeys: ownPropertyNames(prototype),
	};
}

function validateBindingSurface(bindingValue, nativeRoot, nativePath) {
	const surface = bindingSurface(bindingValue);
	const missing = [];
	if (typeof bindingValue?.FreedesktopNotifications !== 'function') {
		missing.push('FreedesktopNotifications constructor');
	} else {
		for (const method of REQUIRED_FREEDESKTOP_METHODS) {
			if (typeof bindingValue.FreedesktopNotifications.prototype?.[method] !== 'function') {
				missing.push(`FreedesktopNotifications.prototype.${method}`);
			}
		}
	}
	if (missing.length === 0) return;
	throw createNativeLoadError({
		moduleName: MODULE_NAME,
		nativePath,
		nativeRoot,
		packageDir: __dirname,
		reason: `native binding surface mismatch: missing ${missing.join(', ')}`,
		skipNativeProbeEnv: SKIP_NATIVE_PROBE_ENV,
		extraDiagnostics: [{name: 'bindingSurface', text: JSON.stringify(surface, null, 2)}],
	});
}

let binding = null;
let loadError = null;

if (process.platform === 'linux') {
	try {
		const nativeRoot = resolveNativeRoot();
		const nativePath = join(nativeRoot, nativeFileName());
		const loaded = loadNativeBinding({
			moduleName: MODULE_NAME,
			nativePath,
			nativeRoot,
			packageDir: __dirname,
			skipNativeProbeEnv: SKIP_NATIVE_PROBE_ENV,
		});
		binding = loaded.binding;
		loadError = loaded.loadError;
		if (loadError) throw loadError;
		validateBindingSurface(binding, nativeRoot, nativePath);
	} catch (error) {
		loadError = createNativeLoadError({
			moduleName: MODULE_NAME,
			nativeRoot: resolveNativeRoot(),
			packageDir: __dirname,
			reason: 'native loader threw before binding load completed',
			cause: error,
			skipNativeProbeEnv: SKIP_NATIVE_PROBE_ENV,
		});
		throw loadError;
	}
}

module.exports = {
	FreedesktopNotifications: binding ? binding.FreedesktopNotifications : null,
	loadError,
};
