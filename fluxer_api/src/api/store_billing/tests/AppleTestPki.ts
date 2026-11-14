// SPDX-License-Identifier: AGPL-3.0-or-later

import {generateKeyPairSync, type KeyObject, randomBytes, sign, X509Certificate} from 'node:crypto';
import {ms} from 'itty-time';
import {CompactSign} from 'jose';

export const APPLE_RECEIPT_SIGNING_OID = '1.2.840.113635.100.6.11.1';
export const APPLE_WWDR_INTERMEDIATE_OID = '1.2.840.113635.100.6.2.1';
const OID_ECDSA_WITH_SHA256 = '1.2.840.10045.4.3.2';
const OID_ECDSA_WITH_SHA384 = '1.2.840.10045.4.3.3';
const OID_COMMON_NAME = '2.5.4.3';
const OID_ORGANIZATION = '2.5.4.10';
const OID_BASIC_CONSTRAINTS = '2.5.29.19';
const OID_KEY_USAGE = '2.5.29.15';
const OID_CERTIFICATE_POLICIES = '2.5.29.32';

export interface TestCertificateExtension {
	oid: string;
	critical?: boolean;
	value: Buffer;
}

interface TestCertificateOptions {
	commonName?: string;
	notBefore?: Date;
	notAfter?: Date;
	appleExtension?: boolean;
	ca?: boolean;
	extraExtensions?: Array<TestCertificateExtension>;
}

export interface AppleTestPkiOptions {
	root?: TestCertificateOptions;
	intermediate?: TestCertificateOptions & {issuer?: AppleTestPki};
	leaf?: TestCertificateOptions;
}

interface SignJwsOptions {
	header?: Record<string, unknown>;
	signingKey?: KeyObject;
}

export interface AppleTestPki {
	root: X509Certificate;
	intermediate: X509Certificate;
	leaf: X509Certificate;
	rootKey: KeyObject;
	intermediateKey: KeyObject;
	leafKey: KeyObject;
	x5c: Array<string>;
	signJws(payload: Record<string, unknown>, options?: SignJwsOptions): Promise<string>;
}

function derLength(length: number): Buffer {
	if (length < 0x80) {
		return Buffer.from([length]);
	}
	const bytes: Array<number> = [];
	let remaining = length;
	while (remaining > 0) {
		bytes.unshift(remaining & 0xff);
		remaining = Math.floor(remaining / 256);
	}
	return Buffer.from([0x80 | bytes.length, ...bytes]);
}

function der(tag: number, ...contents: Array<Buffer>): Buffer {
	const body = Buffer.concat(contents);
	return Buffer.concat([Buffer.from([tag]), derLength(body.length), body]);
}

function derSequence(...contents: Array<Buffer>): Buffer {
	return der(0x30, ...contents);
}

function derInteger(value: Buffer): Buffer {
	const trimmed = value[0] & 0x80 ? Buffer.concat([Buffer.from([0]), value]) : value;
	return der(0x02, trimmed);
}

function derObjectIdentifier(oid: string): Buffer {
	const arcs = oid.split('.').map(Number);
	const bytes = [arcs[0] * 40 + arcs[1]];
	for (const arc of arcs.slice(2)) {
		const chunk = [arc % 128];
		for (let rest = Math.floor(arc / 128); rest > 0; rest = Math.floor(rest / 128)) {
			chunk.unshift((rest % 128) | 0x80);
		}
		bytes.push(...chunk);
	}
	return der(0x06, Buffer.from(bytes));
}

function derTime(date: Date): Buffer {
	const iso = date.toISOString();
	const compact = `${iso.slice(0, 4)}${iso.slice(5, 7)}${iso.slice(8, 10)}${iso.slice(11, 13)}${iso.slice(14, 16)}${iso.slice(17, 19)}Z`;
	const year = date.getUTCFullYear();
	if (year >= 1950 && year < 2050) {
		return der(0x17, Buffer.from(compact.slice(2), 'ascii'));
	}
	return der(0x18, Buffer.from(compact, 'ascii'));
}

function derName(commonName: string): Buffer {
	const attribute = (oid: string, value: string) =>
		der(0x31, derSequence(derObjectIdentifier(oid), der(0x0c, Buffer.from(value, 'utf8'))));
	return derSequence(attribute(OID_ORGANIZATION, 'Fluxer Test PKI'), attribute(OID_COMMON_NAME, commonName));
}

function derExtension(extension: TestCertificateExtension): Buffer {
	return derSequence(
		derObjectIdentifier(extension.oid),
		...(extension.critical ? [der(0x01, Buffer.from([0xff]))] : []),
		der(0x04, extension.value),
	);
}

export function certificatePoliciesExtension(policyOid: string): TestCertificateExtension {
	return {oid: OID_CERTIFICATE_POLICIES, value: derSequence(derSequence(derObjectIdentifier(policyOid)))};
}

interface IssueCertificateParams {
	subjectKey: KeyObject;
	subjectName: string;
	issuerKey: KeyObject;
	issuerName: string;
	issuerHash: 'sha256' | 'sha384';
	options: TestCertificateOptions;
	defaultCa: boolean;
	appleOid: string | null;
}

function issueCertificate(params: IssueCertificateParams): X509Certificate {
	const now = Date.now();
	const ca = params.options.ca ?? params.defaultCa;
	const extensions: Array<TestCertificateExtension> = [
		{
			oid: OID_BASIC_CONSTRAINTS,
			critical: true,
			value: ca ? derSequence(der(0x01, Buffer.from([0xff]))) : derSequence(),
		},
		{
			oid: OID_KEY_USAGE,
			critical: true,
			value: ca ? der(0x03, Buffer.from([0x01, 0x06])) : der(0x03, Buffer.from([0x07, 0x80])),
		},
	];
	if (params.appleOid && params.options.appleExtension !== false) {
		extensions.push({oid: params.appleOid, value: der(0x05)});
	}
	extensions.push(...(params.options.extraExtensions ?? []));
	const signatureAlgorithm = derSequence(
		derObjectIdentifier(params.issuerHash === 'sha256' ? OID_ECDSA_WITH_SHA256 : OID_ECDSA_WITH_SHA384),
	);
	const serial = randomBytes(8);
	serial[0] = (serial[0] & 0x3f) | 0x40;
	const tbs = derSequence(
		der(0xa0, derInteger(Buffer.from([2]))),
		derInteger(serial),
		signatureAlgorithm,
		derName(params.issuerName),
		derSequence(
			derTime(params.options.notBefore ?? new Date(now - ms('365 days'))),
			derTime(params.options.notAfter ?? new Date(now + ms('1825 days'))),
		),
		derName(params.subjectName),
		params.subjectKey.export({type: 'spki', format: 'der'}),
		der(0xa3, derSequence(...extensions.map(derExtension))),
	);
	const signature = sign(params.issuerHash, tbs, params.issuerKey);
	const certificate = derSequence(tbs, signatureAlgorithm, der(0x03, Buffer.concat([Buffer.from([0]), signature])));
	return new X509Certificate(certificate);
}

export function createAppleTestPki(options: AppleTestPkiOptions = {}): AppleTestPki {
	const rootPair = generateKeyPairSync('ec', {namedCurve: 'secp384r1'});
	const intermediatePair = generateKeyPairSync('ec', {namedCurve: 'secp384r1'});
	const leafPair = generateKeyPairSync('ec', {namedCurve: 'prime256v1'});
	const rootName = options.root?.commonName ?? 'Fluxer Test Root CA';
	const intermediateName = options.intermediate?.commonName ?? 'Fluxer Test WWDR CA';
	const root = issueCertificate({
		subjectKey: rootPair.publicKey,
		subjectName: rootName,
		issuerKey: rootPair.privateKey,
		issuerName: rootName,
		issuerHash: 'sha384',
		options: options.root ?? {},
		defaultCa: true,
		appleOid: null,
	});
	const intermediateIssuer = options.intermediate?.issuer;
	const intermediate = issueCertificate({
		subjectKey: intermediatePair.publicKey,
		subjectName: intermediateName,
		issuerKey: intermediateIssuer?.rootKey ?? rootPair.privateKey,
		issuerName: intermediateIssuer ? commonNameOf(intermediateIssuer.root) : rootName,
		issuerHash: 'sha384',
		options: options.intermediate ?? {},
		defaultCa: true,
		appleOid: APPLE_WWDR_INTERMEDIATE_OID,
	});
	const leaf = issueCertificate({
		subjectKey: leafPair.publicKey,
		subjectName: options.leaf?.commonName ?? 'Fluxer Test Receipt Signing',
		issuerKey: intermediatePair.privateKey,
		issuerName: intermediateName,
		issuerHash: 'sha384',
		options: options.leaf ?? {},
		defaultCa: false,
		appleOid: APPLE_RECEIPT_SIGNING_OID,
	});
	const x5c = [leaf, intermediate, root].map((certificate) => certificate.raw.toString('base64'));
	return {
		root,
		intermediate,
		leaf,
		rootKey: rootPair.privateKey,
		intermediateKey: intermediatePair.privateKey,
		leafKey: leafPair.privateKey,
		x5c,
		async signJws(payload, signOptions = {}) {
			return await new CompactSign(new TextEncoder().encode(JSON.stringify(payload)))
				.setProtectedHeader({alg: 'ES256', x5c, ...signOptions.header})
				.sign(signOptions.signingKey ?? leafPair.privateKey);
		},
	};
}

function commonNameOf(certificate: X509Certificate): string {
	const match = /CN=(.+)/.exec(certificate.subject);
	if (!match) {
		throw new Error('Test certificate has no common name');
	}
	return match[1];
}

export function encodeJwsWithSignature(
	header: Record<string, unknown>,
	payload: Record<string, unknown>,
	signature: string,
): string {
	const encode = (value: Record<string, unknown>) => Buffer.from(JSON.stringify(value)).toString('base64url');
	return `${encode(header)}.${encode(payload)}.${signature}`;
}
