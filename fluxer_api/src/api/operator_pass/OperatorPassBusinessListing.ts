// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {
	loadOperatorPassEntitlementState,
	resolveOperatorPassHolder,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import sharp, {type Metadata} from 'sharp';
import type Stripe from 'stripe';

const LOGO_MAX_BYTES = 2 * 1024 * 1024;
const LOGO_MIN_PX = 256;
const LOGO_MAX_PIXELS = 4096 * 4096;
const PAYMENT_HOLD_MS = 7 * MS_PER_DAY;
const PNG_SIGNATURE = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a];

type LogoFormat = 'png' | 'jpeg' | 'webp';

function ascii(bytes: Uint8Array, start: number, end: number): string {
	return String.fromCharCode(...bytes.subarray(start, end));
}

function sniffLogoFormat(bytes: Uint8Array): LogoFormat | null {
	if (bytes.length >= 8 && PNG_SIGNATURE.every((byte, index) => bytes[index] === byte)) {
		return 'png';
	}
	if (bytes.length >= 3 && bytes[0] === 0xff && bytes[1] === 0xd8 && bytes[2] === 0xff) {
		return 'jpeg';
	}
	if (bytes.length >= 12 && ascii(bytes, 0, 4) === 'RIFF' && ascii(bytes, 8, 12) === 'WEBP') {
		return 'webp';
	}
	return null;
}

function isAnimatedPng(bytes: Uint8Array): boolean {
	const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
	let offset = PNG_SIGNATURE.length;
	while (offset + 8 <= bytes.length) {
		const type = ascii(bytes, offset + 4, offset + 8);
		if (type === 'acTL') {
			return true;
		}
		if (type === 'IDAT') {
			return false;
		}
		offset += 12 + view.getUint32(offset);
	}
	return false;
}

function invalidLogo(message: string): InputValidationError {
	return InputValidationError.create('logo', message);
}

export async function validateOperatorPassBusinessLogo(bytes: Uint8Array): Promise<void> {
	if (bytes.length > LOGO_MAX_BYTES) {
		throw invalidLogo('The logo must be under 2 MB');
	}
	const format = sniffLogoFormat(bytes);
	if (!format) {
		throw invalidLogo('The logo must be a PNG, JPEG or WebP image');
	}
	if (format === 'png' && isAnimatedPng(bytes)) {
		throw invalidLogo('The logo must not be animated');
	}
	let metadata: Metadata;
	try {
		metadata = await sharp(Buffer.from(bytes)).metadata();
	} catch {
		throw invalidLogo('The logo could not be read as an image');
	}
	const width = metadata.width ?? 0;
	const height = metadata.height ?? 0;
	if (metadata.format !== format) {
		throw invalidLogo('The logo must be a PNG, JPEG or WebP image');
	}
	if ((metadata.pages ?? 1) > 1) {
		throw invalidLogo('The logo must not be animated');
	}
	if (width * height > LOGO_MAX_PIXELS) {
		throw invalidLogo('The logo must be at most 4096 by 4096 pixels');
	}
	if (width !== height) {
		throw invalidLogo('The logo must be square');
	}
	if (width < LOGO_MIN_PX) {
		throw invalidLogo(`The logo must be at least ${LOGO_MIN_PX} by ${LOGO_MIN_PX} pixels`);
	}
}

function isPaymentSettled(purchase: OperatorPassPurchaseRow, now: Date): boolean {
	const paidAt = purchase.completed_at ?? purchase.purchased_at;
	return (
		purchase.status === 'completed' &&
		purchase.refunded_at == null &&
		purchase.fraud_warning_at == null &&
		(purchase.disputed_at == null || purchase.dispute_closed_at != null) &&
		paidAt != null &&
		now.getTime() - paidAt.getTime() >= PAYMENT_HOLD_MS
	);
}

function isQualifyingTaxId(taxId: Stripe.TaxId): boolean {
	if (taxId.type !== 'eu_vat' || taxId.country === 'SE') {
		return true;
	}
	return taxId.verification?.status === 'verified';
}

async function hasQualifyingTaxId(stripe: Stripe, purchase: OperatorPassPurchaseRow): Promise<boolean> {
	const session = await stripe.checkout.sessions.retrieve(purchase.checkout_session_id, {
		expand: ['customer.tax_ids'],
	});
	const customer = session.customer;
	if (!customer || typeof customer === 'string' || customer.deleted) {
		return false;
	}
	return (customer.tax_ids?.data ?? []).some(isQualifyingTaxId);
}

export async function findOperatorPassBusinessPurchase(params: {
	stripe: Stripe;
	repository: OperatorPassRepository;
	userId: UserID;
	now: Date;
}): Promise<OperatorPassPurchaseRow | null> {
	const {stripe, repository, userId, now} = params;
	for (const purchase of await repository.listPurchasesByPurchaser(userId)) {
		if (isPaymentSettled(purchase, now) && (await hasQualifyingTaxId(stripe, purchase))) {
			return purchase;
		}
	}
	return null;
}

export async function requireOperatorPassBusinessPurchase(params: {
	stripe: Stripe;
	repository: OperatorPassRepository;
	userId: UserID;
	now: Date;
}): Promise<OperatorPassPurchaseRow> {
	const state = await loadOperatorPassEntitlementState(params.repository, params.userId);
	if (!resolveOperatorPassHolder(state, params.now)) {
		throw InputValidationError.create('user_id', 'The account holds no Operator Pass that shows on the wall');
	}
	const purchase = await findOperatorPassBusinessPurchase(params);
	if (!purchase) {
		throw InputValidationError.create(
			'user_id',
			'No purchase of this account qualifies. A business listing needs a purchase with a tax ID, verified when it is an EU VAT number from outside Sweden, paid at least 7 days ago with no refund, open dispute or fraud warning.',
		);
	}
	return purchase;
}
