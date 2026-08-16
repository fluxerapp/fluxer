// SPDX-License-Identifier: AGPL-3.0-or-later

const {existsSync} = require('node:fs');
const {join, sep} = require('node:path');

const MODULE_NAME = '@fluxer/hardware-encoder';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName(platform = process.platform, arch = process.arch) {
	if ((arch !== 'x64' && arch !== 'arm64') || !['darwin', 'linux', 'win32'].includes(platform)) {
		throw new Error(`${MODULE_NAME} not supported on ${platform}-${arch}`);
	}
	if (platform === 'darwin') return `hardware-encoder.darwin-${arch}.node`;
	if (platform === 'linux') return `hardware-encoder.linux-${arch}-gnu.node`;
	return `hardware-encoder.win32-${arch}-msvc.node`;
}

let binding = null;
let loadError = null;

try {
	const nativePath = join(resolveNativeRoot(), nativeFileName());
	if (existsSync(nativePath)) {
		try {
			binding = require(nativePath);
		} catch (error) {
			loadError = error instanceof Error ? error : new Error(String(error));
		}
	} else {
		loadError = new Error(`${MODULE_NAME} native binary missing: ${nativePath}`);
	}
} catch (error) {
	loadError = error instanceof Error ? error : new Error(String(error));
}

function isSupported() {
	return Boolean(binding);
}

function unavailableHardwareEncoderCapability(reason, detail) {
	return {
		available: false,
		backend: 'none',
		compiled: false,
		runtime: false,
		codecs: [],
		zeroCopy: false,
		nativeInputs: [],
		reason,
		detail,
	};
}

function getHardwareEncoderCapability() {
	if (!binding) {
		return unavailableHardwareEncoderCapability(
			'native_binding_unavailable',
			loadError ? loadError.message : `${MODULE_NAME} binding unavailable`,
		);
	}
	if (typeof binding.getHardwareEncoderCapability !== 'function') {
		return unavailableHardwareEncoderCapability(
			'native_capability_unavailable',
			`${MODULE_NAME} native binding does not export getHardwareEncoderCapability`,
		);
	}
	return binding.getHardwareEncoderCapability();
}

function getHardwareEncoderCapabilities() {
	return getHardwareEncoderCapability();
}

module.exports = {
	isSupported,
	getHardwareEncoderCapability,
	getHardwareEncoderCapabilities,
};
