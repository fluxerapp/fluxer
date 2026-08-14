// SPDX-License-Identifier: AGPL-3.0-or-later

const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const {nativeFileName} = require('./pure.cjs');
const MODULE_NAME = '@fluxer/platform-info';
const SKIP_NATIVE_PROBE_ENV = 'FLUXER_PLATFORM_INFO_SKIP_NATIVE_PROBE';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

let binding = null;
let loadError = null;

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

module.exports = {
	getGpuInfo: binding ? binding.getGpuInfo : null,
	loadError,
	_private: {
		nativeFileName,
	},
};
