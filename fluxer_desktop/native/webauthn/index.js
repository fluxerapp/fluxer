// SPDX-License-Identifier: AGPL-3.0-or-later

const {existsSync} = require('node:fs');
const {createHash} = require('node:crypto');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const {
	base64Url,
	clientDataJSON,
	nativeFileName,
	normalizeCreateOptions,
	normalizeGetOptions,
	transportBits,
} = require('./pure.cjs');
const MODULE_NAME = '@fluxer/webauthn';
const SKIP_NATIVE_PROBE_ENV = 'FLUXER_WEBAUTHN_SKIP_NATIVE_PROBE';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

let binding = null;
let loadError = null;

const fileName = nativeFileName();

if (fileName) {
	try {
		const nativeRoot = resolveNativeRoot();
		const nativePath = join(nativeRoot, fileName);
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
} else {
	loadError = createNativeLoadError({
		moduleName: MODULE_NAME,
		nativeRoot: resolveNativeRoot(),
		packageDir: __dirname,
		reason: `no native binary mapping for ${process.platform}/${process.arch}`,
		skipNativeProbeEnv: SKIP_NATIVE_PROBE_ENV,
	});
	throw loadError;
}

function clientDataHash(data) {
	return createHash('sha256').update(data).digest();
}

function unavailableError() {
	return new Error(
		`@fluxer/webauthn native backend unavailable on ${process.platform}/${process.arch}: ${
			loadError instanceof Error ? loadError.message : 'unknown load error'
		}`,
	);
}

function requireBinding() {
	if (!binding) throw unavailableError();
	return binding;
}

async function isSupported() {
	if (!binding || typeof binding.isSupported !== 'function') return false;
	return Boolean(await binding.isSupported());
}

function rawIdCredential(rawId, response, authenticatorAttachment) {
	const id = base64Url(rawId);
	return {
		id,
		rawId,
		response: Buffer.from(JSON.stringify(response), 'utf8'),
		authenticatorAttachment,
		type: 'public-key',
	};
}

async function create(options) {
	const native = requireBinding();
	if (typeof native.create !== 'function') {
		throw new Error(
			`@fluxer/webauthn native backend did not export registration on ${process.platform}/${process.arch}`,
		);
	}
	const normalized = normalizeCreateOptions(options);
	normalized.clientDataHash = clientDataHash(normalized.clientDataJSON);
	const result = await native.create(normalized);
	return rawIdCredential(
		result.rawId,
		{
			clientDataJSON: base64Url(result.clientDataJSON),
			attestationObject: base64Url(result.attestationObject),
		},
		result.authenticatorAttachment,
	);
}

async function get(options) {
	const native = requireBinding();
	if (typeof native.get !== 'function') {
		throw new Error(
			`@fluxer/webauthn native backend did not export authentication on ${process.platform}/${process.arch}`,
		);
	}
	const normalized = normalizeGetOptions(options);
	normalized.clientDataHash = clientDataHash(normalized.clientDataJSON);
	const result = await native.get(normalized);
	const response = {
		clientDataJSON: base64Url(result.clientDataJSON),
		authenticatorData: base64Url(result.authenticatorData),
		signature: base64Url(result.signature),
	};
	if (result.userHandle) response.userHandle = base64Url(result.userHandle);
	return rawIdCredential(result.rawId, response, result.authenticatorAttachment);
}

function getBackendInfo() {
	if (!binding || typeof binding.getBackendInfo !== 'function') {
		return {
			target: `${process.platform}/${process.arch}`,
			backend: 'unavailable',
			nativeLoaded: false,
			ceremoniesImplemented: false,
			platformBrokerAvailable: false,
			platformAuthenticatorAvailable: false,
			supported: false,
			apiVersion: 0,
			reason: loadError instanceof Error ? loadError.message : 'native backend did not load',
		};
	}
	return binding.getBackendInfo();
}

module.exports = {
	create,
	get,
	getBackendInfo,
	isSupported,
	loadError,
	_private: {
		base64Url,
		clientDataJSON,
		nativeFileName,
		normalizeCreateOptions,
		normalizeGetOptions,
		resolveNativeRoot,
		transportBits,
	},
};
