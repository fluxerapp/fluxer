// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, generateKeyPairSync, X509Certificate} from 'node:crypto';
import {
	APPLE_ROOT_CA_G3_PEM,
	getAppleRootCertificates,
	setInjectedAppleRootCertificates,
} from '@app/api/store_billing/app_store/AppleRootCertificates';
import {
	type AppStoreJwsFailureReason,
	AppStoreJwsVerificationError,
	certificateHasExtension,
	verifyAppStoreCertificateChain,
	verifyNotification,
	verifyRenewalInfo,
	verifyTransaction,
} from '@app/api/store_billing/app_store/AppStoreJwsVerifier';
import {
	REAL_APPLE_CHAIN_EFFECTIVE_DATE,
	REAL_APPLE_INTERMEDIATE,
	REAL_APPLE_ROOT,
	REAL_APPLE_SIGNING_CERTIFICATE,
} from '@app/api/store_billing/tests/AppleProductionChain';
import {
	APPLE_RECEIPT_SIGNING_OID,
	APPLE_WWDR_INTERMEDIATE_OID,
	type AppleTestPki,
	certificatePoliciesExtension,
	createAppleTestPki,
	encodeJwsWithSignature,
} from '@app/api/store_billing/tests/AppleTestPki';
import {ms} from 'itty-time';
import {CompactSign} from 'jose';
import {afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const APPLE_ROOT_CA_G3_SHA256 = '63343ABFB89A6A03EBB57E9B3F5FA7BE7C4F5C756F3017B3A8C488C3653E9179';
const APPLE_ROOT_CA_G3_DER_BASE64 = APPLE_ROOT_CA_G3_PEM.replace(/-----[A-Z ]+-----/g, '').replace(/\s+/g, '');

async function expectRejection(promise: Promise<unknown>, reason: AppStoreJwsFailureReason): Promise<void> {
	const error = await promise.then(
		() => null,
		(caught: unknown) => caught,
	);
	expect(error).toBeInstanceOf(AppStoreJwsVerificationError);
	expect((error as AppStoreJwsVerificationError).reason).toBe(reason);
}

function expectSyncRejection(run: () => unknown, reason: AppStoreJwsFailureReason): void {
	let caught: unknown = null;
	try {
		run();
	} catch (error) {
		caught = error;
	}
	expect(caught).toBeInstanceOf(AppStoreJwsVerificationError);
	expect((caught as AppStoreJwsVerificationError).reason).toBe(reason);
}

function transactionPayload(overrides: Record<string, unknown> = {}): Record<string, unknown> {
	const now = Date.now();
	return {
		transactionId: '2000000123456789',
		originalTransactionId: '2000000123456789',
		webOrderLineItemId: '2000000011112222',
		bundleId: 'com.fluxer',
		productId: 'com.fluxer.plutonium.monthly',
		subscriptionGroupIdentifier: '21000000',
		purchaseDate: now - ms('1 hour'),
		originalPurchaseDate: now - ms('1 hour'),
		expiresDate: now + ms('30 days'),
		quantity: 1,
		type: 'Auto-Renewable Subscription',
		appAccountToken: '7e3fb20b-4cdb-47cc-936d-99d65f608138',
		inAppOwnershipType: 'PURCHASED',
		signedDate: now,
		environment: 'Production',
		transactionReason: 'PURCHASE',
		storefront: 'USA',
		storefrontId: '143441',
		price: 4990,
		currency: 'USD',
		appTransactionId: '704432222233344455',
		...overrides,
	};
}

function renewalPayload(overrides: Record<string, unknown> = {}): Record<string, unknown> {
	const now = Date.now();
	return {
		originalTransactionId: '2000000123456789',
		autoRenewProductId: 'com.fluxer.plutonium.monthly',
		productId: 'com.fluxer.plutonium.monthly',
		autoRenewStatus: 1,
		signedDate: now,
		environment: 'Production',
		recentSubscriptionStartDate: now - ms('1 hour'),
		renewalDate: now + ms('30 days'),
		...overrides,
	};
}

async function notificationPayload(
	pki: AppleTestPki,
	overrides: Record<string, unknown> = {},
	dataOverrides: Record<string, unknown> = {},
): Promise<Record<string, unknown>> {
	return {
		notificationType: 'DID_RENEW',
		notificationUUID: '002e14d5-51f5-4503-b5a8-c3a1af68eb20',
		version: '2.0',
		signedDate: Date.now(),
		data: {
			appAppleId: 1234567890,
			bundleId: 'com.fluxer',
			bundleVersion: '42',
			environment: 'Production',
			status: 1,
			signedTransactionInfo: await pki.signJws(transactionPayload()),
			signedRenewalInfo: await pki.signJws(renewalPayload()),
			...dataOverrides,
		},
		...overrides,
	};
}

describe('Apple root certificate pin', () => {
	afterEach(() => {
		setInjectedAppleRootCertificates(undefined);
	});

	it('embeds Apple Root CA - G3 with the published fingerprint', () => {
		const der = Buffer.from(APPLE_ROOT_CA_G3_DER_BASE64, 'base64');
		expect(createHash('sha256').update(der).digest('hex').toUpperCase()).toBe(APPLE_ROOT_CA_G3_SHA256);
		const [root] = getAppleRootCertificates();
		expect(root.fingerprint256.replaceAll(':', '')).toBe(APPLE_ROOT_CA_G3_SHA256);
		expect(root.subject).toContain('CN=Apple Root CA - G3');
		expect(root.ca).toBe(true);
		expect(getAppleRootCertificates()).toHaveLength(1);
	});

	it('matches the root of the real Apple chain', () => {
		expect(REAL_APPLE_ROOT).toBe(APPLE_ROOT_CA_G3_DER_BASE64);
	});
});

describe('certificate extension lookup', () => {
	it('finds the Apple extensions by walking the extension list', () => {
		const pki = createAppleTestPki();
		expect(certificateHasExtension(pki.leaf, APPLE_RECEIPT_SIGNING_OID)).toBe(true);
		expect(certificateHasExtension(pki.leaf, APPLE_WWDR_INTERMEDIATE_OID)).toBe(false);
		expect(certificateHasExtension(pki.intermediate, APPLE_WWDR_INTERMEDIATE_OID)).toBe(true);
		expect(certificateHasExtension(pki.root, APPLE_WWDR_INTERMEDIATE_OID)).toBe(false);
	});

	it('ignores the identifier when it only appears inside another extension value', () => {
		const pki = createAppleTestPki({
			leaf: {appleExtension: false, extraExtensions: [certificatePoliciesExtension(APPLE_RECEIPT_SIGNING_OID)]},
		});
		expect(pki.leaf.raw.includes(Buffer.from('060a2a864886f76364060b01', 'hex'))).toBe(true);
		expect(certificateHasExtension(pki.leaf, APPLE_RECEIPT_SIGNING_OID)).toBe(false);
	});

	it('finds the extensions on the real Apple chain', () => {
		const leaf = new X509Certificate(Buffer.from(REAL_APPLE_SIGNING_CERTIFICATE, 'base64'));
		const intermediate = new X509Certificate(Buffer.from(REAL_APPLE_INTERMEDIATE, 'base64'));
		expect(certificateHasExtension(leaf, APPLE_RECEIPT_SIGNING_OID)).toBe(true);
		expect(certificateHasExtension(intermediate, APPLE_WWDR_INTERMEDIATE_OID)).toBe(true);
		expect(certificateHasExtension(intermediate, APPLE_RECEIPT_SIGNING_OID)).toBe(false);
	});
});

describe('real Apple certificate chain', () => {
	const chain = [REAL_APPLE_SIGNING_CERTIFICATE, REAL_APPLE_INTERMEDIATE, REAL_APPLE_ROOT];

	it('verifies under the pinned root at a date inside the leaf validity', () => {
		const key = verifyAppStoreCertificateChain(chain, REAL_APPLE_CHAIN_EFFECTIVE_DATE);
		const leaf = new X509Certificate(Buffer.from(REAL_APPLE_SIGNING_CERTIFICATE, 'base64'));
		expect(key.export({type: 'spki', format: 'der'})).toEqual(leaf.publicKey.export({type: 'spki', format: 'der'}));
		expect(key.asymmetricKeyDetails?.namedCurve).toBe('prime256v1');
	});

	it('rejects the chain after the leaf expires', () => {
		expectSyncRejection(
			() => verifyAppStoreCertificateChain(chain, new Date('2027-10-14T00:00:00Z')),
			'certificate_expired',
		);
	});

	it('rejects the chain before the leaf was issued', () => {
		expectSyncRejection(
			() => verifyAppStoreCertificateChain(chain, new Date('2025-09-01T00:00:00Z')),
			'certificate_expired',
		);
	});

	it('rejects the chain under a root that is not Apple', () => {
		const pki = createAppleTestPki();
		expectSyncRejection(
			() => verifyAppStoreCertificateChain(chain, REAL_APPLE_CHAIN_EFFECTIVE_DATE, [pki.root]),
			'untrusted_chain',
		);
	});

	it('rejects a test chain under the pinned Apple root', () => {
		const pki = createAppleTestPki();
		expectSyncRejection(() => verifyAppStoreCertificateChain(pki.x5c, new Date()), 'untrusted_chain');
	});
});

describe('compact serialization', () => {
	it('rejects malformed compact serializations', async () => {
		await expectRejection(verifyNotification('a.b.c.d'), 'malformed');
		await expectRejection(verifyNotification('a.b.c'), 'malformed');
		await expectRejection(verifyNotification(''), 'malformed');
		await expectRejection(verifyNotification('..'), 'malformed');
	});
});

describe('App Store JWS verification with a test chain', () => {
	let pki: AppleTestPki;

	beforeAll(() => {
		pki = createAppleTestPki();
	});

	beforeEach(() => {
		setInjectedAppleRootCertificates([pki.root]);
	});

	afterEach(() => {
		setInjectedAppleRootCertificates(undefined);
	});

	it('verifies and types a transaction', async () => {
		const transaction = await verifyTransaction(await pki.signJws(transactionPayload()));
		expect(transaction.transactionId).toBe('2000000123456789');
		expect(transaction.environment).toBe('Production');
		expect(transaction.type).toBe('Auto-Renewable Subscription');
		expect(transaction.appAccountToken).toBe('7e3fb20b-4cdb-47cc-936d-99d65f608138');
	});

	it('verifies renewal info', async () => {
		const renewal = await verifyRenewalInfo(await pki.signJws(renewalPayload({environment: 'Sandbox'})), {
			expectedEnvironment: 'sandbox',
		});
		expect(renewal.autoRenewStatus).toBe(1);
		expect(renewal.environment).toBe('Sandbox');
	});

	it('tolerates enum values it does not know', async () => {
		const transaction = await verifyTransaction(
			await pki.signJws(
				transactionPayload({
					type: 'Future Product Type',
					inAppOwnershipType: 'SOMETHING_NEW',
					revocationType: 'NEW_REVOCATION',
					offerDiscountType: 'NEW_OFFER',
					transactionReason: 'NEW_REASON',
				}),
			),
		);
		expect(transaction.type).toBe('Future Product Type');
		expect(transaction.inAppOwnershipType).toBe('SOMETHING_NEW');
		const notification = await verifyNotification(
			await pki.signJws(await notificationPayload(pki, {notificationType: 'BRAND_NEW', subtype: 'ALSO_NEW'})),
		);
		expect(notification.notificationType).toBe('BRAND_NEW');
		expect(notification.subtype).toBe('ALSO_NEW');
	});

	it('ignores the root certificate sent in x5c', async () => {
		const other = createAppleTestPki();
		const signed = await pki.signJws(transactionPayload(), {header: {x5c: [pki.x5c[0], pki.x5c[1], other.x5c[2]]}});
		await expect(verifyTransaction(signed)).resolves.toMatchObject({transactionId: '2000000123456789'});
	});

	it.each([
		['transactionId'],
		['originalTransactionId'],
		['bundleId'],
		['productId'],
		['type'],
		['purchaseDate'],
		['signedDate'],
		['environment'],
	])('requires %s on a transaction', async (field) => {
		const payload = transactionPayload();
		delete payload[field];
		await expectRejection(verifyTransaction(await pki.signJws(payload)), 'invalid_payload');
	});

	it.each([['originalTransactionId'], ['productId'], ['signedDate'], ['environment']])(
		'requires %s on renewal info',
		async (field) => {
			const payload = renewalPayload();
			delete payload[field];
			await expectRejection(verifyRenewalInfo(await pki.signJws(payload)), 'invalid_payload');
		},
	);

	it.each([['notificationType'], ['notificationUUID'], ['signedDate']])(
		'requires %s on a notification',
		async (field) => {
			const payload = await notificationPayload(pki);
			delete payload[field];
			await expectRejection(verifyNotification(await pki.signJws(payload)), 'invalid_payload');
		},
	);

	it('rejects a wrongly typed field', async () => {
		await expectRejection(
			verifyTransaction(await pki.signJws(transactionPayload({expiresDate: '2026-10-01'}))),
			'invalid_payload',
		);
	});

	it('rejects a chain that ends in another root', async () => {
		const other = createAppleTestPki();
		await expectRejection(verifyTransaction(await other.signJws(transactionPayload())), 'untrusted_chain');
	});

	it('rejects an intermediate that copies the root name but was signed by another key', async () => {
		const impostor = createAppleTestPki({root: {commonName: 'Fluxer Test Root CA'}});
		expect(impostor.root.subject).toBe(pki.root.subject);
		await expectRejection(verifyTransaction(await impostor.signJws(transactionPayload())), 'untrusted_chain');
	});

	it('rejects a leaf issued by a different intermediate', async () => {
		const other = createAppleTestPki({intermediate: {issuer: pki}});
		await expect(verifyTransaction(await other.signJws(transactionPayload()))).resolves.toBeDefined();
		const mixed = await other.signJws(transactionPayload(), {header: {x5c: [other.x5c[0], pki.x5c[1], pki.x5c[2]]}});
		await expectRejection(verifyTransaction(mixed), 'untrusted_chain');
	});

	it('rejects an intermediate that is not a CA', async () => {
		const notCa = createAppleTestPki({intermediate: {ca: false}});
		setInjectedAppleRootCertificates([notCa.root]);
		await expectRejection(verifyTransaction(await notCa.signJws(transactionPayload())), 'untrusted_chain');
	});

	it('rejects a leaf that is a CA', async () => {
		const caLeaf = createAppleTestPki({leaf: {ca: true}});
		setInjectedAppleRootCertificates([caLeaf.root]);
		await expectRejection(verifyTransaction(await caLeaf.signJws(transactionPayload())), 'untrusted_chain');
	});

	it('rejects a leaf without the receipt signing extension', async () => {
		const missing = createAppleTestPki({leaf: {appleExtension: false}});
		setInjectedAppleRootCertificates([missing.root]);
		await expectRejection(
			verifyTransaction(await missing.signJws(transactionPayload())),
			'missing_certificate_extension',
		);
	});

	it('rejects an intermediate without the WWDR extension', async () => {
		const missing = createAppleTestPki({intermediate: {appleExtension: false}});
		setInjectedAppleRootCertificates([missing.root]);
		await expectRejection(
			verifyTransaction(await missing.signJws(transactionPayload())),
			'missing_certificate_extension',
		);
	});

	it('rejects a leaf that has the identifier only as a policy', async () => {
		const policyOnly = createAppleTestPki({
			leaf: {appleExtension: false, extraExtensions: [certificatePoliciesExtension(APPLE_RECEIPT_SIGNING_OID)]},
		});
		setInjectedAppleRootCertificates([policyOnly.root]);
		await expectRejection(
			verifyTransaction(await policyOnly.signJws(transactionPayload())),
			'missing_certificate_extension',
		);
	});

	describe('certificate dates at signedDate', () => {
		const signedDate = Date.parse('2026-06-01T12:00:00Z');
		let expiredLeaf: AppleTestPki;

		beforeAll(() => {
			expiredLeaf = createAppleTestPki({
				leaf: {notBefore: new Date(signedDate - ms('30 days')), notAfter: new Date(signedDate - ms('2 minutes'))},
			});
		});

		beforeEach(() => {
			setInjectedAppleRootCertificates([expiredLeaf.root]);
		});

		it('rejects a leaf that expired before signedDate', async () => {
			await expectRejection(
				verifyTransaction(await expiredLeaf.signJws(transactionPayload({signedDate}))),
				'certificate_expired',
			);
		});

		it('accepts the same leaf for data signed while it was valid', async () => {
			const earlier = signedDate - ms('1 day');
			await expect(
				verifyTransaction(await expiredLeaf.signJws(transactionPayload({signedDate: earlier}))),
			).resolves.toMatchObject({signedDate: earlier});
		});

		it('allows one minute of clock skew', async () => {
			const withinSkew = signedDate - ms('2 minutes') + ms('50 seconds');
			await expect(
				verifyTransaction(await expiredLeaf.signJws(transactionPayload({signedDate: withinSkew}))),
			).resolves.toBeDefined();
		});

		it('rejects a leaf that is not valid yet', async () => {
			await expectRejection(
				verifyTransaction(await expiredLeaf.signJws(transactionPayload({signedDate: signedDate - ms('31 days')}))),
				'certificate_expired',
			);
		});

		it('rejects an intermediate that expired before signedDate', async () => {
			const expiredIntermediate = createAppleTestPki({
				intermediate: {notAfter: new Date(signedDate - ms('1 day'))},
			});
			setInjectedAppleRootCertificates([expiredIntermediate.root]);
			await expectRejection(
				verifyTransaction(await expiredIntermediate.signJws(transactionPayload({signedDate}))),
				'certificate_expired',
			);
		});

		it('rejects a root that expired before signedDate', async () => {
			const expiredRoot = createAppleTestPki({root: {notAfter: new Date(signedDate - ms('1 day'))}});
			setInjectedAppleRootCertificates([expiredRoot.root]);
			await expectRejection(
				verifyTransaction(await expiredRoot.signJws(transactionPayload({signedDate}))),
				'certificate_expired',
			);
		});
	});

	it('rejects a signature from a key other than the leaf', async () => {
		const {privateKey} = generateKeyPairSync('ec', {namedCurve: 'prime256v1'});
		await expectRejection(
			verifyTransaction(await pki.signJws(transactionPayload(), {signingKey: privateKey})),
			'invalid_signature',
		);
	});

	it('rejects a payload changed after signing', async () => {
		const signed = await pki.signJws(transactionPayload());
		const [header, , signature] = signed.split('.');
		const forged = Buffer.from(JSON.stringify(transactionPayload({productId: 'com.fluxer.plutonium.yearly'}))).toString(
			'base64url',
		);
		await expectRejection(verifyTransaction(`${header}.${forged}.${signature}`), 'invalid_signature');
	});

	it('rejects alg none', async () => {
		const unsigned = encodeJwsWithSignature({alg: 'none', x5c: pki.x5c}, transactionPayload(), 'AA');
		await expectRejection(verifyTransaction(unsigned), 'unsupported_algorithm');
	});

	it('rejects HS256', async () => {
		const secret = new TextEncoder().encode('0123456789abcdef0123456789abcdef');
		const hmac = await new CompactSign(new TextEncoder().encode(JSON.stringify(transactionPayload())))
			.setProtectedHeader({alg: 'HS256', x5c: pki.x5c})
			.sign(secret);
		await expectRejection(verifyTransaction(hmac), 'unsupported_algorithm');
	});

	it('rejects ES384', async () => {
		const {privateKey} = generateKeyPairSync('ec', {namedCurve: 'secp384r1'});
		const signed = await new CompactSign(new TextEncoder().encode(JSON.stringify(transactionPayload())))
			.setProtectedHeader({alg: 'ES384', x5c: pki.x5c})
			.sign(privateKey);
		await expectRejection(verifyTransaction(signed), 'unsupported_algorithm');
	});

	it.each([
		['one certificate', (chain: Array<string>) => [chain[0]]],
		['two certificates', (chain: Array<string>) => chain.slice(0, 2)],
		['four certificates', (chain: Array<string>) => [...chain, chain[2]]],
		['no certificates', () => []],
	])('rejects an x5c with %s', async (_label, pick) => {
		const signed = await pki.signJws(transactionPayload(), {header: {x5c: pick(pki.x5c)}});
		await expectRejection(verifyTransaction(signed), 'invalid_chain_length');
	});

	it('rejects an x5c that is not an array', async () => {
		const signed = await pki.signJws(transactionPayload(), {header: {x5c: pki.x5c[0]}});
		await expectRejection(verifyTransaction(signed), 'invalid_chain_length');
	});

	it('rejects x5c entries that are not standard base64 DER', async () => {
		const urlSafe = pki.leaf.raw.toString('base64url');
		const signed = await pki.signJws(transactionPayload(), {header: {x5c: [urlSafe, pki.x5c[1], pki.x5c[2]]}});
		const garbage = await pki.signJws(transactionPayload(), {header: {x5c: ['AAAA', pki.x5c[1], pki.x5c[2]]}});
		const notString = await pki.signJws(transactionPayload(), {header: {x5c: [42, pki.x5c[1], pki.x5c[2]]}});
		if (urlSafe !== pki.x5c[0]) {
			await expectRejection(verifyTransaction(signed), 'invalid_certificate');
		}
		await expectRejection(verifyTransaction(garbage), 'invalid_certificate');
		await expectRejection(verifyTransaction(notString), 'invalid_certificate');
	});

	it.each([['Xcode'], ['LocalTesting'], ['production'], ['']])('rejects environment %j', async (environment) => {
		await expectRejection(
			verifyTransaction(await pki.signJws(transactionPayload({environment}))),
			'invalid_environment',
		);
		await expectRejection(verifyRenewalInfo(await pki.signJws(renewalPayload({environment}))), 'invalid_environment');
	});

	it('rejects an environment other than the expected one', async () => {
		const sandbox = await pki.signJws(transactionPayload({environment: 'Sandbox'}));
		await expectRejection(verifyTransaction(sandbox, {expectedEnvironment: 'production'}), 'invalid_environment');
		await expect(verifyTransaction(sandbox, {expectedEnvironment: 'sandbox'})).resolves.toBeDefined();
		await expect(verifyTransaction(sandbox, {expectedEnvironment: null})).resolves.toBeDefined();
	});

	it('rejects a bundle that is not configured', async () => {
		await expectRejection(
			verifyTransaction(await pki.signJws(transactionPayload({bundleId: 'com.fluxer.other'}))),
			'invalid_app_identifier',
		);
	});

	describe('notifications', () => {
		it('verifies the outer payload and the nested signed data', async () => {
			const notification = await verifyNotification(await pki.signJws(await notificationPayload(pki)));
			expect(notification.notificationType).toBe('DID_RENEW');
			expect(notification.environment).toBe('Production');
			expect(notification.appAppleId).toBe(1234567890);
			expect(notification.status).toBe(1);
			expect(notification.transaction?.originalTransactionId).toBe('2000000123456789');
			expect(notification.renewalInfo?.autoRenewStatus).toBe(1);
		});

		it('rejects a production notification for another app id', async () => {
			const payload = await notificationPayload(pki, {}, {appAppleId: 999});
			await expectRejection(verifyNotification(await pki.signJws(payload)), 'invalid_app_identifier');
		});

		it('rejects a production notification without an app id', async () => {
			const payload = await notificationPayload(pki, {}, {appAppleId: undefined});
			await expectRejection(verifyNotification(await pki.signJws(payload)), 'invalid_app_identifier');
		});

		it('accepts a sandbox notification without an app id', async () => {
			const payload = await notificationPayload(
				pki,
				{},
				{
					appAppleId: undefined,
					environment: 'Sandbox',
					signedTransactionInfo: await pki.signJws(transactionPayload({environment: 'Sandbox'})),
					signedRenewalInfo: await pki.signJws(renewalPayload({environment: 'Sandbox'})),
				},
			);
			const notification = await verifyNotification(await pki.signJws(payload));
			expect(notification.environment).toBe('Sandbox');
			expect(notification.appAppleId).toBeNull();
		});

		it('rejects a notification for a bundle that is not configured', async () => {
			const payload = await notificationPayload(pki, {}, {bundleId: 'com.fluxer.other'});
			await expectRejection(verifyNotification(await pki.signJws(payload)), 'invalid_app_identifier');
		});

		it('rejects an Xcode notification', async () => {
			const payload = await notificationPayload(pki, {}, {environment: 'Xcode'});
			await expectRejection(verifyNotification(await pki.signJws(payload)), 'invalid_environment');
		});

		it('rejects nested signed data from another environment', async () => {
			const payload = await notificationPayload(
				pki,
				{},
				{signedTransactionInfo: await pki.signJws(transactionPayload({environment: 'Sandbox'}))},
			);
			await expectRejection(verifyNotification(await pki.signJws(payload)), 'invalid_environment');
		});

		it('rejects nested signed data that is not signed by Apple', async () => {
			const other = createAppleTestPki();
			const payload = await notificationPayload(
				pki,
				{},
				{signedTransactionInfo: await other.signJws(transactionPayload())},
			);
			await expectRejection(verifyNotification(await pki.signJws(payload)), 'untrusted_chain');
		});

		it('rejects nested renewal info for another subscription', async () => {
			const payload = await notificationPayload(
				pki,
				{},
				{signedRenewalInfo: await pki.signJws(renewalPayload({originalTransactionId: '2000000999999999'}))},
			);
			await expectRejection(verifyNotification(await pki.signJws(payload)), 'invalid_payload');
		});

		it('rejects a notification without app data', async () => {
			const payload = await notificationPayload(pki, {data: undefined});
			await expectRejection(verifyNotification(await pki.signJws(payload)), 'invalid_payload');
		});

		it('accepts a renewal extension summary', async () => {
			const payload = await notificationPayload(pki, {
				notificationType: 'RENEWAL_EXTENSION',
				subtype: 'SUMMARY',
				data: undefined,
				summary: {
					requestIdentifier: 'b3a7d8c4-4f6e-4a53-9b1e-2c4f7e8a9d10',
					environment: 'Production',
					appAppleId: 1234567890,
					bundleId: 'com.fluxer',
					productId: 'com.fluxer.plutonium.monthly',
					succeededCount: 5,
					failedCount: 0,
				},
			});
			const notification = await verifyNotification(await pki.signJws(payload));
			expect(notification.transaction).toBeNull();
			expect(notification.payload.summary?.succeededCount).toBe(5);
		});

		it('derives the environment of an external purchase token notification', async () => {
			const payload = await notificationPayload(pki, {
				notificationType: 'EXTERNAL_PURCHASE_TOKEN',
				subtype: 'UNREPORTED',
				data: undefined,
				externalPurchaseToken: {
					externalPurchaseId: 'SANDBOX_b2158121-7af9-49d4-9561-1f588205523e',
					tokenCreationDate: Date.now(),
					bundleId: 'com.fluxer',
				},
			});
			const notification = await verifyNotification(await pki.signJws(payload));
			expect(notification.environment).toBe('Sandbox');
		});

		it('accepts a notification without a version', async () => {
			const payload = await notificationPayload(pki, {version: undefined});
			await expect(verifyNotification(await pki.signJws(payload))).resolves.toBeDefined();
		});
	});
});
