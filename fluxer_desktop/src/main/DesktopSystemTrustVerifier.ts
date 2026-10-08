// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, X509Certificate} from 'node:crypto';
import net from 'node:net';
import {createChildLogger} from '@electron/common/Logger';
import {CHROMIUM_SPKI_BLOCKLIST, CHROMIUM_SPKI_BLOCKLIST_SOURCE} from '@electron/main/DesktopChromiumSPKIBlocklist';
import {resolveDesktopLocallyAddedCertificates} from '@electron/main/DesktopTrustedCertificates';
import type {Certificate, Session} from 'electron';

const logger = createChildLogger('DesktopSystemTrustVerifier');

const CHROMIUM_DEFAULT_RESULT = -3;
const CHROMIUM_ACCEPT = 0;
const CHROMIUM_CERT_AUTHORITY_INVALID = -202;
const MAX_CHAIN_LENGTH = 6;
const MAX_SIGNATURE_CHECKS = 25;
const MIN_RSA_BITS = 2048;
const MAX_RSA_BITS = 8192;
const MIN_WILDCARD_BASE_LABELS = 3;
const DER_TRUE = 0xff;

const OID = {
	basicConstraints: '2.5.29.19',
	keyUsage: '2.5.29.15',
	subjectAltName: '2.5.29.17',
	extendedKeyUsage: '2.5.29.37',
	nameConstraints: '2.5.29.30',
	policyConstraints: '2.5.29.36',
	inhibitAnyPolicy: '2.5.29.54',
	serverAuth: '1.3.6.1.5.5.7.3.1',
	anyExtendedKeyUsage: '2.5.29.37.0',
} as const;

const ALLOWED_CRITICAL_EXTENSIONS: ReadonlySet<string> = new Set([
	OID.basicConstraints,
	OID.keyUsage,
	OID.subjectAltName,
	OID.extendedKeyUsage,
]);
const FORBIDDEN_EXTENSIONS: ReadonlySet<string> = new Set([
	OID.nameConstraints,
	OID.policyConstraints,
	OID.inhibitAnyPolicy,
]);
const ALLOWED_SIGNATURE_ALGORITHMS: ReadonlySet<string> = new Set([
	'1.2.840.113549.1.1.11',
	'1.2.840.113549.1.1.12',
	'1.2.840.113549.1.1.13',
	'1.2.840.10045.4.3.2',
	'1.2.840.10045.4.3.3',
	'1.2.840.10045.4.3.4',
	'1.3.101.112',
]);
const ALLOWED_EC_CURVES: ReadonlySet<string> = new Set(['prime256v1', 'secp384r1']);

const KEY_USAGE_DIGITAL_SIGNATURE = 0;
const KEY_USAGE_KEY_ENCIPHERMENT = 2;
const KEY_USAGE_KEY_CERT_SIGN = 5;

const TAG_BOOLEAN = 0x01;
const TAG_INTEGER = 0x02;
const TAG_BIT_STRING = 0x03;
const TAG_OCTET_STRING = 0x04;
const TAG_OID = 0x06;
const TAG_SEQUENCE = 0x30;
const TAG_VERSION = 0xa0;
const TAG_EXTENSIONS = 0xa3;
const TAG_SAN_DNS = 0x82;
const TAG_SAN_IP = 0x87;

class MalformedCertificateError extends Error {
	constructor() {
		super('Malformed certificate DER');
		this.name = 'MalformedCertificateError';
	}
}

interface DerNode {
	readonly tag: number;
	readonly content: Buffer;
}

interface Extension {
	readonly critical: boolean;
	readonly value: Buffer;
}

interface ParsedCertificate {
	readonly x509: X509Certificate;
	readonly signatureAlgorithm: string;
	readonly extensions: ReadonlyMap<string, Extension>;
	readonly isCA: boolean;
	readonly pathLength: number | null;
}

interface DesktopSystemTrustRequest {
	readonly hostname: string;
	readonly certificate: Certificate;
	readonly errorCode: number;
}

export type DesktopTrustAnchors = ReadonlyMap<string, ReadonlyArray<ParsedCertificate>>;

function readNodes(buffer: Buffer): Array<DerNode> {
	const nodes: Array<DerNode> = [];
	let offset = 0;
	while (offset < buffer.length) {
		if (offset + 2 > buffer.length) throw new MalformedCertificateError();
		const tag = buffer[offset];
		if ((tag & 0x1f) === 0x1f) throw new MalformedCertificateError();
		let length = buffer[offset + 1];
		let cursor = offset + 2;
		if (length & 0x80) {
			const size = length & 0x7f;
			if (size === 0 || size > 4 || cursor + size > buffer.length) throw new MalformedCertificateError();
			length = 0;
			for (let index = 0; index < size; index++) length = length * 256 + buffer[cursor + index];
			cursor += size;
		}
		if (cursor + length > buffer.length) throw new MalformedCertificateError();
		nodes.push({tag, content: buffer.subarray(cursor, cursor + length)});
		offset = cursor + length;
	}
	return nodes;
}

function expect(node: DerNode | undefined, tag: number): DerNode {
	if (node == null || node.tag !== tag) throw new MalformedCertificateError();
	return node;
}

function only(buffer: Buffer, tag: number): DerNode {
	const nodes = readNodes(buffer);
	if (nodes.length !== 1) throw new MalformedCertificateError();
	return expect(nodes[0], tag);
}

function readDerTrue(node: DerNode): true {
	if (node.content.length !== 1 || node.content[0] !== DER_TRUE) throw new MalformedCertificateError();
	return true;
}

function decodeOid(content: Buffer): string {
	if (content.length === 0) throw new MalformedCertificateError();
	const parts: Array<number> = [];
	let value = 0;
	for (const byte of content) {
		value = value * 128 + (byte & 0x7f);
		if (value > Number.MAX_SAFE_INTEGER / 128) throw new MalformedCertificateError();
		if ((byte & 0x80) === 0) {
			parts.push(value);
			value = 0;
		}
	}
	if ((content[content.length - 1] & 0x80) !== 0) throw new MalformedCertificateError();
	const first = parts.shift() ?? 0;
	const head = first < 80 ? [Math.floor(first / 40), first % 40] : [2, first - 80];
	return [...head, ...parts].join('.');
}

function parseExtensions(tbs: ReadonlyArray<DerNode>): Map<string, Extension> {
	const extensions = new Map<string, Extension>();
	const wrapper = tbs.find((node) => node.tag === TAG_EXTENSIONS);
	if (wrapper == null) return extensions;
	for (const entry of readNodes(only(wrapper.content, TAG_SEQUENCE).content)) {
		const fields = readNodes(expect(entry, TAG_SEQUENCE).content);
		const oid = decodeOid(expect(fields[0], TAG_OID).content);
		const hasCritical = fields[1]?.tag === TAG_BOOLEAN;
		const critical = hasCritical && readDerTrue(fields[1]);
		const value = expect(fields[hasCritical ? 2 : 1], TAG_OCTET_STRING).content;
		if (fields.length !== (hasCritical ? 3 : 2) || extensions.has(oid)) throw new MalformedCertificateError();
		extensions.set(oid, {critical, value});
	}
	return extensions;
}

function parseBasicConstraints(extension: Extension | undefined): {isCA: boolean; pathLength: number | null} {
	if (extension == null) return {isCA: false, pathLength: null};
	const fields = readNodes(only(extension.value, TAG_SEQUENCE).content);
	let index = 0;
	let isCA = false;
	if (fields[index]?.tag === TAG_BOOLEAN) {
		isCA = readDerTrue(fields[index]);
		index++;
	}
	let pathLength: number | null = null;
	if (fields[index]?.tag === TAG_INTEGER) {
		const content = fields[index].content;
		if (content.length === 0 || content.length > 2 || (content[0] & 0x80) !== 0) throw new MalformedCertificateError();
		pathLength = content.readUIntBE(0, content.length);
		index++;
	}
	if (index !== fields.length) throw new MalformedCertificateError();
	return {isCA, pathLength};
}

function parseCertificate(x509: X509Certificate): ParsedCertificate {
	const parts = readNodes(only(x509.raw, TAG_SEQUENCE).content);
	if (parts.length !== 3) throw new MalformedCertificateError();
	const tbs = readNodes(expect(parts[0], TAG_SEQUENCE).content);
	const algorithm = readNodes(expect(parts[1], TAG_SEQUENCE).content);
	const signatureAlgorithm = decodeOid(expect(algorithm[0], TAG_OID).content);
	if (tbs[0]?.tag !== TAG_VERSION && tbs.some((node) => node.tag === TAG_EXTENSIONS)) {
		throw new MalformedCertificateError();
	}
	const extensions = parseExtensions(tbs);
	return {x509, signatureAlgorithm, extensions, ...parseBasicConstraints(extensions.get(OID.basicConstraints))};
}

function keyUsageHas(certificate: ParsedCertificate, bit: number): boolean | null {
	const extension = certificate.extensions.get(OID.keyUsage);
	if (extension == null) return null;
	const bits = only(extension.value, TAG_BIT_STRING).content;
	const byte = bits[1 + (bit >> 3)];
	return byte != null && (byte & (0x80 >> (bit & 7))) !== 0;
}

function extendedKeyUsages(certificate: ParsedCertificate): ReadonlyArray<string> | null {
	const extension = certificate.extensions.get(OID.extendedKeyUsage);
	if (extension == null) return null;
	return readNodes(only(extension.value, TAG_SEQUENCE).content).map((node) => decodeOid(expect(node, TAG_OID).content));
}

function allowsServerAuth(certificate: ParsedCertificate): boolean {
	const usages = extendedKeyUsages(certificate);
	return usages == null || usages.includes(OID.serverAuth) || usages.includes(OID.anyExtendedKeyUsage);
}

function hasAcceptableKey(certificate: ParsedCertificate): boolean {
	const key = certificate.x509.publicKey;
	const details = key.asymmetricKeyDetails ?? {};
	switch (key.asymmetricKeyType) {
		case 'rsa': {
			const bits = details.modulusLength ?? 0;
			return bits >= MIN_RSA_BITS && bits <= MAX_RSA_BITS;
		}
		case 'ec':
			return details.namedCurve != null && ALLOWED_EC_CURVES.has(details.namedCurve);
		case 'ed25519':
			return true;
		default:
			return false;
	}
}

function isBlocklisted(certificate: ParsedCertificate): boolean {
	const spki = certificate.x509.publicKey.export({type: 'spki', format: 'der'});
	return CHROMIUM_SPKI_BLOCKLIST.has(createHash('sha256').update(spki).digest('hex'));
}

function isCurrentlyValid(certificate: ParsedCertificate, now: number): boolean {
	return Date.parse(certificate.x509.validFrom) <= now && now <= Date.parse(certificate.x509.validTo);
}

function passesCommonChecks(certificate: ParsedCertificate, now: number): boolean {
	for (const [oid, extension] of certificate.extensions) {
		if (FORBIDDEN_EXTENSIONS.has(oid)) return false;
		if (extension.critical && !ALLOWED_CRITICAL_EXTENSIONS.has(oid)) return false;
	}
	return isCurrentlyValid(certificate, now) && hasAcceptableKey(certificate) && !isBlocklisted(certificate);
}

function isUsableAuthority(certificate: ParsedCertificate, now: number): boolean {
	return (
		certificate.isCA &&
		keyUsageHas(certificate, KEY_USAGE_KEY_CERT_SIGN) !== false &&
		allowsServerAuth(certificate) &&
		passesCommonChecks(certificate, now)
	);
}

function isUsableIntermediate(certificate: ParsedCertificate, now: number): boolean {
	return ALLOWED_SIGNATURE_ALGORITHMS.has(certificate.signatureAlgorithm) && isUsableAuthority(certificate, now);
}

function isUsableLeaf(certificate: ParsedCertificate, now: number): boolean {
	if (certificate.isCA || !ALLOWED_SIGNATURE_ALGORITHMS.has(certificate.signatureAlgorithm)) return false;
	const digitalSignature = keyUsageHas(certificate, KEY_USAGE_DIGITAL_SIGNATURE);
	if (digitalSignature === false && keyUsageHas(certificate, KEY_USAGE_KEY_ENCIPHERMENT) === false) return false;
	return allowsServerAuth(certificate) && passesCommonChecks(certificate, now);
}

function normalizeHost(hostname: string): string {
	return hostname
		.replace(/^\[(.*)\]$/u, '$1')
		.replace(/\.$/u, '')
		.toLowerCase();
}

function ipBytes(host: string): Buffer | null {
	if (net.isIPv4(host)) return Buffer.from(host.split('.').map(Number));
	if (!net.isIPv6(host)) return null;
	const [head = '', tail = ''] = host.split('::');
	const words = (part: string) => (part === '' ? [] : part.split(':'));
	const headWords = words(head);
	const tailWords = words(tail);
	const all = host.includes('::')
		? [...headWords, ...Array(8 - headWords.length - tailWords.length).fill('0'), ...tailWords]
		: headWords;
	if (all.length !== 8) return null;
	return Buffer.from(all.flatMap((word) => [parseInt(word, 16) >> 8, parseInt(word, 16) & 0xff]));
}

function matchesDnsName(pattern: string, host: string): boolean {
	const name = pattern.replace(/\.$/u, '').toLowerCase();
	if (!name.includes('*')) return name === host;
	if (!name.startsWith('*.') || name.indexOf('*', 1) !== -1) return false;
	const base = name.slice(2);
	if (base.split('.').length < MIN_WILDCARD_BASE_LABELS) return false;
	const dot = host.indexOf('.');
	return dot > 0 && host.slice(dot + 1) === base;
}

function matchesHost(leaf: ParsedCertificate, hostname: string): boolean {
	const extension = leaf.extensions.get(OID.subjectAltName);
	if (extension == null || typeof hostname !== 'string') return false;
	const host = normalizeHost(hostname);
	const address = ipBytes(host);
	for (const name of readNodes(only(extension.value, TAG_SEQUENCE).content)) {
		if (address != null && name.tag === TAG_SAN_IP && name.content.equals(address)) return true;
		if (address == null && name.tag === TAG_SAN_DNS && matchesDnsName(name.content.toString('latin1'), host)) {
			return true;
		}
	}
	return false;
}

interface PathSearch {
	readonly intermediates: ReadonlyArray<ParsedCertificate>;
	readonly anchors: DesktopTrustAnchors;
	readonly now: number;
	readonly signatures: Map<ParsedCertificate, Map<ParsedCertificate, boolean>>;
	signatureChecks: number;
}

function isIssuedBy(search: PathSearch, certificate: ParsedCertificate, issuer: ParsedCertificate): boolean {
	if (!certificate.x509.checkIssued(issuer.x509)) return false;
	let byIssuer = search.signatures.get(certificate);
	if (byIssuer == null) {
		byIssuer = new Map();
		search.signatures.set(certificate, byIssuer);
	}
	const cached = byIssuer.get(issuer);
	if (cached != null) return cached;
	if (search.signatureChecks >= MAX_SIGNATURE_CHECKS) return false;
	search.signatureChecks++;
	const verified = certificate.x509.verify(issuer.x509.publicKey);
	byIssuer.set(issuer, verified);
	return verified;
}

function withinPathLength(authority: ParsedCertificate, intermediatesBelow: number): boolean {
	return authority.pathLength == null || intermediatesBelow <= authority.pathLength;
}

function buildsPath(search: PathSearch, current: ParsedCertificate, path: ReadonlyArray<ParsedCertificate>): boolean {
	const below = path.length - 1;
	for (const anchor of search.anchors.get(current.x509.issuer) ?? []) {
		if (
			isCurrentlyValid(anchor, search.now) &&
			withinPathLength(anchor, below) &&
			isIssuedBy(search, current, anchor)
		) {
			return true;
		}
	}
	if (path.length >= MAX_CHAIN_LENGTH - 1) return false;
	for (const candidate of search.intermediates) {
		if (path.includes(candidate) || !withinPathLength(candidate, below)) continue;
		if (isIssuedBy(search, current, candidate) && buildsPath(search, candidate, [...path, candidate])) return true;
	}
	return false;
}

function presentedChain(certificate: Certificate): Array<X509Certificate> {
	const chain: Array<X509Certificate> = [];
	let current: Certificate | undefined = certificate;
	while (current != null) {
		if (chain.length >= MAX_CHAIN_LENGTH) throw new MalformedCertificateError();
		chain.push(new X509Certificate(current.data));
		current = current.issuerCert;
	}
	return chain;
}

function chainsToDesktopTrustAnchor(
	certificate: Certificate,
	hostname: string,
	anchors: DesktopTrustAnchors,
	now: number,
): boolean {
	const [leafX509, ...rest] = presentedChain(certificate);
	if (leafX509 == null) return false;
	const leaf = parseCertificate(leafX509);
	if (!isUsableLeaf(leaf, now) || !matchesHost(leaf, hostname)) return false;
	const intermediates = rest.map(parseCertificate).filter((candidate) => isUsableIntermediate(candidate, now));
	return buildsPath({intermediates, anchors, now, signatures: new Map(), signatureChecks: 0}, leaf, [leaf]);
}

export function buildDesktopTrustAnchors(
	certificates: ReadonlyArray<string>,
	now: number = Date.now(),
): DesktopTrustAnchors {
	const anchors = new Map<string, Array<ParsedCertificate>>();
	for (const pem of certificates) {
		let anchor: ParsedCertificate;
		try {
			anchor = parseCertificate(new X509Certificate(pem));
		} catch {
			continue;
		}
		if (!isUsableAuthority(anchor, now)) continue;
		const bySubject = anchors.get(anchor.x509.subject);
		if (bySubject == null) anchors.set(anchor.x509.subject, [anchor]);
		else bySubject.push(anchor);
	}
	return anchors;
}

export function verifyAgainstDesktopSystemTrust(
	request: DesktopSystemTrustRequest,
	loadAnchors: () => DesktopTrustAnchors,
): number {
	if (request.errorCode !== CHROMIUM_CERT_AUTHORITY_INVALID) return CHROMIUM_DEFAULT_RESULT;
	try {
		const anchors = loadAnchors();
		if (anchors.size > 0 && chainsToDesktopTrustAnchor(request.certificate, request.hostname, anchors, Date.now())) {
			logger.debug('Accepted a certificate that chains to a locally added CA', {hostname: request.hostname});
			return CHROMIUM_ACCEPT;
		}
	} catch (error) {
		logger.warn('Could not check a certificate against locally added CAs', {hostname: request.hostname, error});
	}
	return CHROMIUM_DEFAULT_RESULT;
}

export function installDesktopSystemTrustVerifier(target: Session): void {
	if (process.platform !== 'linux') return;
	const anchors = buildDesktopTrustAnchors(resolveDesktopLocallyAddedCertificates());
	if (anchors.size === 0) return;
	logger.info('Trusting locally added CAs for Chromium requests', {
		count: anchors.size,
		blocklist: CHROMIUM_SPKI_BLOCKLIST_SOURCE,
	});
	target.setCertificateVerifyProc((request, callback) => {
		callback(verifyAgainstDesktopSystemTrust(request, () => anchors));
	});
}
