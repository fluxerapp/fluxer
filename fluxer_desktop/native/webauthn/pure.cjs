// SPDX-License-Identifier: AGPL-3.0-or-later

const WEBAUTHN_AUTHENTICATOR_ATTACHMENT_ANY = 0;
const WEBAUTHN_AUTHENTICATOR_ATTACHMENT_PLATFORM = 1;
const WEBAUTHN_AUTHENTICATOR_ATTACHMENT_CROSS_PLATFORM = 2;
const WEBAUTHN_USER_VERIFICATION_REQUIREMENT_REQUIRED = 1;
const WEBAUTHN_USER_VERIFICATION_REQUIREMENT_PREFERRED = 2;
const WEBAUTHN_USER_VERIFICATION_REQUIREMENT_DISCOURAGED = 3;
const WEBAUTHN_ATTESTATION_CONVEYANCE_PREFERENCE_NONE = 1;
const WEBAUTHN_ATTESTATION_CONVEYANCE_PREFERENCE_INDIRECT = 2;
const WEBAUTHN_ATTESTATION_CONVEYANCE_PREFERENCE_DIRECT = 3;
const WEBAUTHN_ENTERPRISE_ATTESTATION_NONE = 0;
const WEBAUTHN_ENTERPRISE_ATTESTATION_VENDOR_FACILITATED = 1;
const WEBAUTHN_CTAP_TRANSPORT_USB = 0x00000001;
const WEBAUTHN_CTAP_TRANSPORT_NFC = 0x00000002;
const WEBAUTHN_CTAP_TRANSPORT_BLE = 0x00000004;
const WEBAUTHN_CTAP_TRANSPORT_INTERNAL = 0x00000010;
const WEBAUTHN_CTAP_TRANSPORT_HYBRID = 0x00000020;
const WEBAUTHN_CTAP_TRANSPORT_SMART_CARD = 0x00000040;

function nativeFileName(platform = process.platform, arch = process.arch) {
	if (platform === 'darwin' && (arch === 'x64' || arch === 'arm64')) return `webauthn.darwin-${arch}.node`;
	if (platform === 'win32' && (arch === 'x64' || arch === 'arm64')) return `webauthn.win32-${arch}-msvc.node`;
	if (platform === 'linux' && (arch === 'x64' || arch === 'arm64')) return `webauthn.linux-${arch}-gnu.node`;
	return null;
}

function base64Url(buffer) {
	return Buffer.from(buffer).toString('base64').replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

function normalizedOrigin(origin) {
	if (typeof origin !== 'string' || origin.length === 0) {
		throw new TypeError('@fluxer/webauthn requires an origin string');
	}
	return new URL(origin).origin;
}

function relyingPartyId(options, origin) {
	if (typeof options.rpId === 'string' && options.rpId.length > 0) return options.rpId;
	if (options.rp && typeof options.rp.id === 'string' && options.rp.id.length > 0) return options.rp.id;
	return new URL(origin).hostname;
}

function clientDataJSON(type, challenge, origin) {
	return Buffer.from(
		JSON.stringify({
			type,
			challenge: base64Url(challenge),
			origin,
			crossOrigin: false,
		}),
		'utf8',
	);
}

function authenticatorAttachment(value) {
	if (value === 'platform') return WEBAUTHN_AUTHENTICATOR_ATTACHMENT_PLATFORM;
	if (value === 'cross-platform') return WEBAUTHN_AUTHENTICATOR_ATTACHMENT_CROSS_PLATFORM;
	return WEBAUTHN_AUTHENTICATOR_ATTACHMENT_ANY;
}

function userVerification(value) {
	if (value === 'required') return WEBAUTHN_USER_VERIFICATION_REQUIREMENT_REQUIRED;
	if (value === 'discouraged') return WEBAUTHN_USER_VERIFICATION_REQUIREMENT_DISCOURAGED;
	return WEBAUTHN_USER_VERIFICATION_REQUIREMENT_PREFERRED;
}

function attestation(value) {
	if (value === 'direct') return WEBAUTHN_ATTESTATION_CONVEYANCE_PREFERENCE_DIRECT;
	if (value === 'indirect') return WEBAUTHN_ATTESTATION_CONVEYANCE_PREFERENCE_INDIRECT;
	return WEBAUTHN_ATTESTATION_CONVEYANCE_PREFERENCE_NONE;
}

function enterpriseAttestation(value) {
	return value === 'enterprise'
		? WEBAUTHN_ENTERPRISE_ATTESTATION_VENDOR_FACILITATED
		: WEBAUTHN_ENTERPRISE_ATTESTATION_NONE;
}

function transportBits(transports) {
	if (!Array.isArray(transports)) return 0;
	let bits = 0;
	for (const transport of transports) {
		if (transport === 'usb') bits |= WEBAUTHN_CTAP_TRANSPORT_USB;
		else if (transport === 'nfc') bits |= WEBAUTHN_CTAP_TRANSPORT_NFC;
		else if (transport === 'ble') bits |= WEBAUTHN_CTAP_TRANSPORT_BLE;
		else if (transport === 'internal') bits |= WEBAUTHN_CTAP_TRANSPORT_INTERNAL;
		else if (transport === 'hybrid') bits |= WEBAUTHN_CTAP_TRANSPORT_HYBRID;
		else if (transport === 'smart-card') bits |= WEBAUTHN_CTAP_TRANSPORT_SMART_CARD;
	}
	return bits;
}

function credentialDescriptors(descriptors) {
	if (!Array.isArray(descriptors)) return [];
	return descriptors.map((descriptor) => ({
		id: Buffer.from(descriptor.id),
		transports: transportBits(descriptor.transports),
	}));
}

function windowHandleBuffer(value) {
	if (Buffer.isBuffer(value)) return value;
	if (value instanceof Uint8Array) return Buffer.from(value);
	return undefined;
}

function pinString(value) {
	return typeof value === 'string' && value.length > 0 ? value : undefined;
}

function residentKeyFlags(selection) {
	const residentKey = selection?.residentKey;
	const requireResidentKey = Boolean(selection?.requireResidentKey) || residentKey === 'required';
	return {
		requireResidentKey,
		preferResidentKey: requireResidentKey || residentKey === 'preferred',
	};
}

function normalizeCreateOptions(options) {
	if (!options || typeof options !== 'object') throw new TypeError('registration options must be an object');
	const origin = normalizedOrigin(options.origin);
	const selection = options.authenticatorSelection || {};
	const residentKeys = residentKeyFlags(selection);
	return {
		rpId: relyingPartyId(options, origin),
		rpName: options.rp && typeof options.rp.name === 'string' ? options.rp.name : relyingPartyId(options, origin),
		challenge: Buffer.from(options.challenge),
		userId: Buffer.from(options.user.id),
		userName: options.user.name,
		userDisplayName: options.user.displayName,
		clientDataJSON: clientDataJSON('webauthn.create', options.challenge, origin),
		pubKeyCredParams: options.pubKeyCredParams,
		excludeCredentials: credentialDescriptors(options.excludeCredentials),
		timeout: Number.isFinite(options.timeout) ? Math.max(0, Math.trunc(options.timeout)) : 0,
		authenticatorAttachment: authenticatorAttachment(selection.authenticatorAttachment),
		userVerification: userVerification(selection.userVerification),
		attestation: attestation(options.attestation),
		enterpriseAttestation: enterpriseAttestation(options.attestation),
		windowHandle: windowHandleBuffer(options.windowHandle),
		pin: pinString(options.pin),
		...residentKeys,
	};
}

function normalizeGetOptions(options) {
	if (!options || typeof options !== 'object') throw new TypeError('assertion options must be an object');
	const origin = normalizedOrigin(options.origin);
	return {
		rpId: relyingPartyId(options, origin),
		challenge: Buffer.from(options.challenge),
		clientDataJSON: clientDataJSON('webauthn.get', options.challenge, origin),
		allowCredentials: credentialDescriptors(options.allowCredentials),
		timeout: Number.isFinite(options.timeout) ? Math.max(0, Math.trunc(options.timeout)) : 0,
		authenticatorAttachment: WEBAUTHN_AUTHENTICATOR_ATTACHMENT_ANY,
		userVerification: userVerification(options.userVerification),
		windowHandle: windowHandleBuffer(options.windowHandle),
		pin: pinString(options.pin),
	};
}

module.exports = {
	base64Url,
	clientDataJSON,
	nativeFileName,
	normalizeCreateOptions,
	normalizeGetOptions,
	transportBits,
};
