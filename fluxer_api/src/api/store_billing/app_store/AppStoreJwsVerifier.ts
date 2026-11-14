// SPDX-License-Identifier: AGPL-3.0-or-later

import {type KeyObject, X509Certificate} from 'node:crypto';
import type {AppStoreAppConfig} from '@app/api/config/APIConfig';
import {getAppleRootCertificates} from '@app/api/store_billing/app_store/AppleRootCertificates';
import {getAppStoreApp} from '@app/api/store_billing/StoreBillingConfig';
import type {StoreEnvironment} from '@app/api/store_billing/StoreBillingTypes';
import {ms} from 'itty-time';
import {compactVerify, decodeProtectedHeader} from 'jose';
import {z} from 'zod';

const APPLE_RECEIPT_SIGNING_EXTENSION_OID = '1.2.840.113635.100.6.11.1';
const APPLE_WWDR_INTERMEDIATE_EXTENSION_OID = '1.2.840.113635.100.6.2.1';
const APPLE_CERTIFICATE_CHAIN_LENGTH = 3;
const CERTIFICATE_DATE_SKEW_MS = ms('1 minute');
const STANDARD_BASE64_PATTERN = /^[A-Za-z0-9+/]+={0,2}$/;
const BASE64URL_SEGMENT_PATTERN = /^[A-Za-z0-9_-]+$/;
const DER_SEQUENCE = 0x30;
const DER_OBJECT_IDENTIFIER = 0x06;
const DER_EXTENSIONS_CONTEXT_TAG = 0xa3;

export type AppStoreEnvironmentName = 'Production' | 'Sandbox';

export type AppStoreJwsFailureReason =
	| 'malformed'
	| 'unsupported_algorithm'
	| 'invalid_chain_length'
	| 'invalid_certificate'
	| 'untrusted_chain'
	| 'missing_certificate_extension'
	| 'certificate_expired'
	| 'invalid_signature'
	| 'invalid_payload'
	| 'invalid_app_identifier'
	| 'invalid_environment';

export class AppStoreJwsVerificationError extends Error {
	constructor(
		readonly reason: AppStoreJwsFailureReason,
		options?: {cause?: unknown},
	) {
		super(`App Store signed data rejected: ${reason}`, options);
		this.name = 'AppStoreJwsVerificationError';
	}
}

const AppStoreTransactionPayloadSchema = z.object({
	transactionId: z.string().min(1),
	originalTransactionId: z.string().min(1),
	bundleId: z.string().min(1),
	productId: z.string().min(1),
	type: z.string().min(1),
	purchaseDate: z.number(),
	signedDate: z.number(),
	environment: z.string(),
	originalPurchaseDate: z.number().nullish(),
	expiresDate: z.number().nullish(),
	webOrderLineItemId: z.string().nullish(),
	subscriptionGroupIdentifier: z.string().nullish(),
	quantity: z.number().nullish(),
	appAccountToken: z.string().nullish(),
	appTransactionId: z.string().nullish(),
	inAppOwnershipType: z.string().nullish(),
	revocationDate: z.number().nullish(),
	revocationReason: z.number().nullish(),
	revocationType: z.string().nullish(),
	revocationPercentage: z.number().nullish(),
	isUpgraded: z.boolean().nullish(),
	offerType: z.number().nullish(),
	offerIdentifier: z.string().nullish(),
	offerDiscountType: z.string().nullish(),
	offerPeriod: z.string().nullish(),
	storefront: z.string().nullish(),
	storefrontId: z.string().nullish(),
	transactionReason: z.string().nullish(),
	currency: z.string().nullish(),
	price: z.number().nullish(),
});

const AppStoreRenewalInfoPayloadSchema = z.object({
	originalTransactionId: z.string().min(1),
	productId: z.string().min(1),
	signedDate: z.number(),
	environment: z.string(),
	autoRenewStatus: z.number().nullish(),
	autoRenewProductId: z.string().nullish(),
	expirationIntent: z.number().nullish(),
	gracePeriodExpiresDate: z.number().nullish(),
	isInBillingRetryPeriod: z.boolean().nullish(),
	priceIncreaseStatus: z.number().nullish(),
	recentSubscriptionStartDate: z.number().nullish(),
	renewalDate: z.number().nullish(),
	renewalPrice: z.number().nullish(),
	currency: z.string().nullish(),
	offerType: z.number().nullish(),
	offerIdentifier: z.string().nullish(),
	offerDiscountType: z.string().nullish(),
	offerPeriod: z.string().nullish(),
	eligibleWinBackOfferIds: z.array(z.string()).nullish(),
	appAccountToken: z.string().nullish(),
	appTransactionId: z.string().nullish(),
});

const AppStoreNotificationDataSchema = z.object({
	bundleId: z.string().min(1),
	environment: z.string(),
	appAppleId: z.number().nullish(),
	bundleVersion: z.string().nullish(),
	signedTransactionInfo: z.string().nullish(),
	signedRenewalInfo: z.string().nullish(),
	status: z.number().nullish(),
	consumptionRequestReason: z.string().nullish(),
});

const AppStoreNotificationSummarySchema = z.object({
	bundleId: z.string().min(1),
	environment: z.string(),
	appAppleId: z.number().nullish(),
	requestIdentifier: z.string().nullish(),
	productId: z.string().nullish(),
	storefrontCountryCodes: z.array(z.string()).nullish(),
	failedCount: z.number().nullish(),
	succeededCount: z.number().nullish(),
});

const AppStoreNotificationAppDataSchema = z.object({
	bundleId: z.string().min(1),
	environment: z.string(),
	appAppleId: z.number().nullish(),
	signedAppTransactionInfo: z.string().nullish(),
});

const AppStoreExternalPurchaseTokenSchema = z.object({
	bundleId: z.string().min(1),
	appAppleId: z.number().nullish(),
	externalPurchaseId: z.string().nullish(),
	tokenCreationDate: z.number().nullish(),
});

const AppStoreNotificationPayloadSchema = z.object({
	notificationType: z.string().min(1),
	notificationUUID: z.string().min(1),
	signedDate: z.number(),
	subtype: z.string().nullish(),
	version: z.string().nullish(),
	data: AppStoreNotificationDataSchema.nullish(),
	summary: AppStoreNotificationSummarySchema.nullish(),
	appData: AppStoreNotificationAppDataSchema.nullish(),
	externalPurchaseToken: AppStoreExternalPurchaseTokenSchema.nullish(),
});

type WithEnvironment<T> = Omit<T, 'environment'> & {environment: AppStoreEnvironmentName};

export type AppStoreTransactionPayload = WithEnvironment<z.infer<typeof AppStoreTransactionPayloadSchema>>;

export type AppStoreRenewalInfoPayload = WithEnvironment<z.infer<typeof AppStoreRenewalInfoPayloadSchema>>;

type AppStoreNotificationPayload = z.infer<typeof AppStoreNotificationPayloadSchema>;

export interface AppStoreVerifiedNotification {
	notificationType: string;
	subtype: string | null;
	notificationUUID: string;
	signedDate: number;
	environment: AppStoreEnvironmentName;
	bundleId: string;
	appAppleId: number | null;
	status: number | null;
	transaction: AppStoreTransactionPayload | null;
	renewalInfo: AppStoreRenewalInfoPayload | null;
	payload: AppStoreNotificationPayload;
}

export interface AppStoreVerifyOptions {
	expectedEnvironment?: StoreEnvironment | null;
}

export function toStoreEnvironment(environment: AppStoreEnvironmentName): StoreEnvironment {
	return environment === 'Production' ? 'production' : 'sandbox';
}

function fail(reason: AppStoreJwsFailureReason, cause?: unknown): never {
	throw new AppStoreJwsVerificationError(reason, cause === undefined ? undefined : {cause});
}

interface DerElement {
	tag: number;
	contentStart: number;
	end: number;
}

function readDerElement(buffer: Buffer, offset: number, limit: number): DerElement {
	if (offset + 2 > limit) {
		throw new RangeError('Truncated DER element');
	}
	const tag = buffer[offset];
	if ((tag & 0x1f) === 0x1f) {
		throw new RangeError('Unsupported DER tag');
	}
	const firstLengthByte = buffer[offset + 1];
	let position = offset + 2;
	let length = firstLengthByte;
	if (firstLengthByte >= 0x80) {
		const lengthBytes = firstLengthByte & 0x7f;
		if (lengthBytes === 0 || lengthBytes > 4 || position + lengthBytes > limit) {
			throw new RangeError('Invalid DER length');
		}
		length = 0;
		for (let i = 0; i < lengthBytes; i++) {
			length = length * 256 + buffer[position + i];
		}
		position += lengthBytes;
	}
	const end = position + length;
	if (end > limit) {
		throw new RangeError('DER element exceeds its parent');
	}
	return {tag, contentStart: position, end};
}

function readDerChildren(buffer: Buffer, parent: DerElement): Array<DerElement> {
	const children: Array<DerElement> = [];
	let offset = parent.contentStart;
	while (offset < parent.end) {
		const child = readDerElement(buffer, offset, parent.end);
		children.push(child);
		offset = child.end;
	}
	return children;
}

function encodeObjectIdentifier(oid: string): Buffer {
	const arcs = oid.split('.').map((arc) => Number.parseInt(arc, 10));
	const bytes: Array<number> = [arcs[0] * 40 + arcs[1]];
	for (const arc of arcs.slice(2)) {
		const encoded = [arc & 0x7f];
		let remaining = Math.floor(arc / 128);
		while (remaining > 0) {
			encoded.unshift((remaining & 0x7f) | 0x80);
			remaining = Math.floor(remaining / 128);
		}
		bytes.push(...encoded);
	}
	return Buffer.from(bytes);
}

function listCertificateExtensionOids(der: Buffer): Array<Buffer> {
	const certificate = readDerElement(der, 0, der.length);
	if (certificate.tag !== DER_SEQUENCE || certificate.end !== der.length) {
		throw new RangeError('Certificate is not a DER sequence');
	}
	const [tbsCertificate] = readDerChildren(der, certificate);
	if (tbsCertificate?.tag !== DER_SEQUENCE) {
		throw new RangeError('Certificate has no TBSCertificate');
	}
	const extensionsWrapper = readDerChildren(der, tbsCertificate).find(
		(element) => element.tag === DER_EXTENSIONS_CONTEXT_TAG,
	);
	if (!extensionsWrapper) {
		return [];
	}
	const [extensions] = readDerChildren(der, extensionsWrapper);
	if (extensions?.tag !== DER_SEQUENCE) {
		throw new RangeError('Certificate extensions are not a sequence');
	}
	return readDerChildren(der, extensions).map((extension) => {
		const [extensionId] = readDerChildren(der, extension);
		if (extension.tag !== DER_SEQUENCE || extensionId?.tag !== DER_OBJECT_IDENTIFIER) {
			throw new RangeError('Certificate extension has no identifier');
		}
		return der.subarray(extensionId.contentStart, extensionId.end);
	});
}

export function certificateHasExtension(certificate: X509Certificate, oid: string): boolean {
	const expected = encodeObjectIdentifier(oid);
	try {
		return listCertificateExtensionOids(certificate.raw).some((extensionOid) => extensionOid.equals(expected));
	} catch {
		return false;
	}
}

function parseChainCertificate(encoded: unknown): X509Certificate {
	if (typeof encoded !== 'string' || encoded.length % 4 !== 0 || !STANDARD_BASE64_PATTERN.test(encoded)) {
		fail('invalid_certificate');
	}
	try {
		return new X509Certificate(Buffer.from(encoded, 'base64'));
	} catch (error) {
		fail('invalid_certificate', error);
	}
}

function safeVerify(certificate: X509Certificate, issuer: X509Certificate): boolean {
	try {
		return certificate.verify(issuer.publicKey);
	} catch {
		return false;
	}
}

function isValidAt(certificate: X509Certificate, effectiveDate: Date): boolean {
	const time = effectiveDate.getTime();
	return (
		certificate.validFromDate.getTime() <= time + CERTIFICATE_DATE_SKEW_MS &&
		certificate.validToDate.getTime() >= time - CERTIFICATE_DATE_SKEW_MS
	);
}

export function verifyAppStoreCertificateChain(
	x5c: ReadonlyArray<unknown>,
	effectiveDate: Date,
	roots: ReadonlyArray<X509Certificate> = getAppleRootCertificates(),
): KeyObject {
	if (x5c.length !== APPLE_CERTIFICATE_CHAIN_LENGTH) {
		fail('invalid_chain_length');
	}
	const leaf = parseChainCertificate(x5c[0]);
	const intermediate = parseChainCertificate(x5c[1]);
	const root = roots.find(
		(candidate) => intermediate.issuer === candidate.subject && safeVerify(intermediate, candidate),
	);
	if (!root || !intermediate.ca) {
		fail('untrusted_chain');
	}
	if (leaf.ca || leaf.issuer !== intermediate.subject || !safeVerify(leaf, intermediate)) {
		fail('untrusted_chain');
	}
	if (
		!certificateHasExtension(leaf, APPLE_RECEIPT_SIGNING_EXTENSION_OID) ||
		!certificateHasExtension(intermediate, APPLE_WWDR_INTERMEDIATE_EXTENSION_OID)
	) {
		fail('missing_certificate_extension');
	}
	if (!isValidAt(leaf, effectiveDate) || !isValidAt(intermediate, effectiveDate) || !isValidAt(root, effectiveDate)) {
		fail('certificate_expired');
	}
	return leaf.publicKey;
}

function decodeUnverifiedPayload(segment: string): Record<string, unknown> {
	let decoded: unknown;
	try {
		decoded = JSON.parse(Buffer.from(segment, 'base64url').toString('utf8'));
	} catch (error) {
		fail('malformed', error);
	}
	if (decoded === null || typeof decoded !== 'object' || Array.isArray(decoded)) {
		fail('malformed');
	}
	return decoded as Record<string, unknown>;
}

async function verifyAppStoreSignedData(
	signedData: string,
	roots: ReadonlyArray<X509Certificate> = getAppleRootCertificates(),
): Promise<unknown> {
	if (typeof signedData !== 'string') {
		fail('malformed');
	}
	const segments = signedData.split('.');
	if (segments.length !== 3 || !segments.every((segment) => BASE64URL_SEGMENT_PATTERN.test(segment))) {
		fail('malformed');
	}
	let header: ReturnType<typeof decodeProtectedHeader>;
	try {
		header = decodeProtectedHeader(signedData);
	} catch (error) {
		fail('malformed', error);
	}
	if (header.alg !== 'ES256') {
		fail('unsupported_algorithm');
	}
	if (!Array.isArray(header.x5c)) {
		fail('invalid_chain_length');
	}
	const unverified = decodeUnverifiedPayload(segments[1]);
	const signedDate = unverified.signedDate;
	if (typeof signedDate !== 'number' || !Number.isFinite(signedDate)) {
		fail('invalid_payload');
	}
	const leafKey = verifyAppStoreCertificateChain(header.x5c, new Date(signedDate), roots);
	let verifiedBytes: Uint8Array;
	try {
		const result = await compactVerify(signedData, leafKey, {algorithms: ['ES256']});
		verifiedBytes = result.payload;
	} catch (error) {
		fail('invalid_signature', error);
	}
	try {
		return JSON.parse(Buffer.from(verifiedBytes).toString('utf8'));
	} catch (error) {
		fail('malformed', error);
	}
}

function parsePayload<T>(schema: z.ZodType<T>, payload: unknown): T {
	const result = schema.safeParse(payload);
	if (!result.success) {
		fail('invalid_payload', result.error);
	}
	return result.data;
}

function resolveEnvironment(
	environment: string,
	expected: StoreEnvironment | null | undefined,
): AppStoreEnvironmentName {
	if (environment !== 'Production' && environment !== 'Sandbox') {
		fail('invalid_environment');
	}
	if (expected && toStoreEnvironment(environment) !== expected) {
		fail('invalid_environment');
	}
	return environment;
}

function resolveApp(bundleId: string): AppStoreAppConfig {
	const app = getAppStoreApp(bundleId);
	if (!app) {
		fail('invalid_app_identifier');
	}
	return app;
}

export async function verifyTransaction(
	signedTransaction: string,
	options: AppStoreVerifyOptions = {},
): Promise<AppStoreTransactionPayload> {
	const payload = parsePayload(AppStoreTransactionPayloadSchema, await verifyAppStoreSignedData(signedTransaction));
	resolveApp(payload.bundleId);
	const environment = resolveEnvironment(payload.environment, options.expectedEnvironment);
	return {...payload, environment};
}

export async function verifyRenewalInfo(
	signedRenewalInfo: string,
	options: AppStoreVerifyOptions = {},
): Promise<AppStoreRenewalInfoPayload> {
	const payload = parsePayload(AppStoreRenewalInfoPayloadSchema, await verifyAppStoreSignedData(signedRenewalInfo));
	const environment = resolveEnvironment(payload.environment, options.expectedEnvironment);
	return {...payload, environment};
}

interface NotificationIdentity {
	bundleId: string;
	appAppleId: number | null;
	environment: string;
}

function resolveNotificationIdentity(payload: AppStoreNotificationPayload): NotificationIdentity {
	const block = payload.data ?? payload.summary ?? payload.appData;
	if (block) {
		return {bundleId: block.bundleId, appAppleId: block.appAppleId ?? null, environment: block.environment};
	}
	const token = payload.externalPurchaseToken;
	if (token) {
		return {
			bundleId: token.bundleId,
			appAppleId: token.appAppleId ?? null,
			environment: token.externalPurchaseId?.startsWith('SANDBOX') ? 'Sandbox' : 'Production',
		};
	}
	fail('invalid_payload');
}

export async function verifyNotification(
	signedPayload: string,
	options: AppStoreVerifyOptions = {},
): Promise<AppStoreVerifiedNotification> {
	const payload = parsePayload(AppStoreNotificationPayloadSchema, await verifyAppStoreSignedData(signedPayload));
	const identity = resolveNotificationIdentity(payload);
	const app = resolveApp(identity.bundleId);
	const environment = resolveEnvironment(identity.environment, options.expectedEnvironment);
	if (environment === 'Production' && identity.appAppleId !== app.appAppleId) {
		fail('invalid_app_identifier');
	}
	const nestedOptions: AppStoreVerifyOptions = {expectedEnvironment: toStoreEnvironment(environment)};
	const signedTransactionInfo = payload.data?.signedTransactionInfo;
	const signedRenewalInfo = payload.data?.signedRenewalInfo;
	const transaction = signedTransactionInfo ? await verifyTransaction(signedTransactionInfo, nestedOptions) : null;
	const renewalInfo = signedRenewalInfo ? await verifyRenewalInfo(signedRenewalInfo, nestedOptions) : null;
	if (transaction && transaction.bundleId !== identity.bundleId) {
		fail('invalid_app_identifier');
	}
	if (transaction && renewalInfo && transaction.originalTransactionId !== renewalInfo.originalTransactionId) {
		fail('invalid_payload');
	}
	return {
		notificationType: payload.notificationType,
		subtype: payload.subtype ?? null,
		notificationUUID: payload.notificationUUID,
		signedDate: payload.signedDate,
		environment,
		bundleId: identity.bundleId,
		appAppleId: identity.appAppleId,
		status: payload.data?.status ?? null,
		transaction,
		renewalInfo,
		payload,
	};
}
