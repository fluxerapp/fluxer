// SPDX-License-Identifier: AGPL-3.0-or-later

const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const MODULE_NAME = '@fluxer/mac-screen-capture';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName() {
	if (process.platform !== 'darwin') {
		throw new Error(`@fluxer/mac-screen-capture is only supported on macOS, got ${process.platform}`);
	}
	switch (process.arch) {
		case 'x64':
			return 'mac-screen-capture.darwin-x64.node';
		case 'arm64':
			return 'mac-screen-capture.darwin-arm64.node';
		default:
			throw new Error(`Unsupported macOS architecture: ${process.arch}`);
	}
}

let binding = null;
let loadError = null;

if (process.platform === 'darwin') {
	try {
		const nativeRoot = resolveNativeRoot();
		const nativePath = join(nativeRoot, nativeFileName());
		const loaded = loadNativeBinding({
			moduleName: MODULE_NAME,
			nativePath,
			nativeRoot,
			packageDir: __dirname,
			probe: false,
		});
		binding = loaded.binding;
		loadError = loaded.loadError;
		if (loadError) throw loadError;
	} catch (error) {
		loadError = createNativeLoadError({
			moduleName: MODULE_NAME,
			nativeRoot: resolveNativeRoot(),
			packageDir: __dirname,
			reason: 'native loader threw before binding load completed',
			cause: error,
		});
		throw loadError;
	}
}

function probeScreenRecordingAccess() {
	if (!binding || typeof binding.probeScreenRecordingAccess !== 'function') return Promise.resolve('unsupported');
	return Promise.resolve(binding.probeScreenRecordingAccess());
}

module.exports = {
	loadError,
	probeScreenRecordingAccess,
};
