// SPDX-License-Identifier: AGPL-3.0-or-later

const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const MODULE_NAME = '@fluxer/linux-portals';
const SKIP_NATIVE_PROBE_ENV = 'FLUXER_LINUX_PORTALS_SKIP_NATIVE_PROBE';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName() {
	if (process.platform !== 'linux') {
		throw new Error(`@fluxer/linux-portals is only supported on Linux, got ${process.platform}`);
	}
	switch (process.arch) {
		case 'x64':
			return 'linux-portals.linux-x64-gnu.node';
		case 'arm64':
			return 'linux-portals.linux-arm64-gnu.node';
		default:
			throw new Error(`Unsupported Linux architecture: ${process.arch}`);
	}
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
	resolveKwinWindowPid: binding ? binding.resolveKwinWindowPid : null,
	resolveX11WindowPid: binding ? binding.resolveX11WindowPid : null,
	resolveWindowPid: binding ? binding.resolveWindowPid : null,
	openFile: binding ? binding.openFile : null,
	saveFile: binding ? binding.saveFile : null,
	requestBackground: binding ? binding.requestBackground : null,
	GlobalShortcutsPortal: binding ? binding.GlobalShortcutsPortal : null,
	SessionStateMonitor: binding ? binding.SessionStateMonitor : null,
	readColorScheme: binding ? binding.readColorScheme : () => 'no-preference',
	readContrast: binding ? binding.readContrast : () => 'no-preference',
	readAccentColor: binding ? binding.readAccentColor : () => null,
	Settings: binding ? binding.Settings : null,
	loadError,
};
