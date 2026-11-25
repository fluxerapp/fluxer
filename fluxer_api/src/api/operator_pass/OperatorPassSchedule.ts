// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, timingSafeEqual} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {OperatorPassConfig} from '@app/api/config/APIConfig';
import {OPERATOR_PASS_PRICES_MINOR} from '@app/api/operator_pass/OperatorPassPrices';
import {deriveOperatorPassSecret, OPERATOR_PASS_QUOTE_SECRET_LABEL} from '@app/api/operator_pass/OperatorPassSecrets';
import {getEnvCurrencyPreferences} from '@app/api/utils/CurrencyUtils';
import {
	OPERATOR_PASS_CURRENCIES,
	type OperatorPassCurrency,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {ms} from 'itty-time';

export type OperatorPassSchedule = Pick<OperatorPassConfig, 'clockOffsetMs'>;

const OPERATOR_PASS_CHECKOUT_EXPIRY_MS = ms('2 hours');

const QUOTE_VERSION = 3;
const QUOTE_MAX_LENGTH = 1024;
const USER_ID_PATTERN = /^\d{1,20}$/u;

function getOperatorPassSchedule(): OperatorPassSchedule {
	return Config.operatorPass;
}

export function operatorPassNow(
	schedule: OperatorPassSchedule = getOperatorPassSchedule(),
	realNowMs = Date.now(),
): Date {
	return new Date(realNowMs + schedule.clockOffsetMs);
}

export function getOperatorPassPriceMinor(currency: OperatorPassCurrency): number {
	return OPERATOR_PASS_PRICES_MINOR[currency];
}

export function isOperatorPassCurrency(value: string): value is OperatorPassCurrency {
	return (OPERATOR_PASS_CURRENCIES as ReadonlyArray<string>).includes(value);
}

export function resolveOperatorPassCurrency(
	countryCode: string | null | undefined,
	disabled: ReadonlySet<OperatorPassCurrency> = Config.operatorPass.disabledCurrencies,
): OperatorPassCurrency {
	return (
		getEnvCurrencyPreferences(countryCode).find(
			(currency): currency is OperatorPassCurrency => isOperatorPassCurrency(currency) && !disabled.has(currency),
		) ?? 'USD'
	);
}

interface OperatorPassQuote {
	currency: OperatorPassCurrency;
	amountMinor: number;
	issuedAtMs: number;
	userId: string | null;
}

function getQuoteSecret(): string {
	return deriveOperatorPassSecret(OPERATOR_PASS_QUOTE_SECRET_LABEL);
}

function signQuotePayload(payload: string, secret: string): string {
	return createHmac('sha256', secret).update(payload).digest('base64url');
}

function signOperatorPassQuote(quote: OperatorPassQuote, secret: string = getQuoteSecret()): string {
	const payload = Buffer.from(
		JSON.stringify({
			v: QUOTE_VERSION,
			c: quote.currency,
			a: quote.amountMinor,
			t: quote.issuedAtMs,
			u: quote.userId,
		}),
		'utf8',
	).toString('base64url');
	return `${payload}.${signQuotePayload(payload, secret)}`;
}

export interface OperatorPassQuoteRequest {
	now: Date;
	currency: OperatorPassCurrency;
	amountMinor: number;
	userId: string | null;
}

export function issueOperatorPassQuote(request: OperatorPassQuoteRequest, secret: string = getQuoteSecret()): string {
	return signOperatorPassQuote(
		{
			currency: request.currency,
			amountMinor: request.amountMinor,
			issuedAtMs: request.now.getTime(),
			userId: request.userId,
		},
		secret,
	);
}

function parseQuotePayload(payload: string): OperatorPassQuote | null {
	let decoded: unknown;
	try {
		decoded = JSON.parse(Buffer.from(payload, 'base64url').toString('utf8'));
	} catch {
		return null;
	}
	if (typeof decoded !== 'object' || decoded === null || Array.isArray(decoded)) {
		return null;
	}
	const {v, c, a, t, u} = decoded as Record<string, unknown>;
	if (v !== QUOTE_VERSION) return null;
	if (typeof c !== 'string' || !isOperatorPassCurrency(c)) return null;
	if (typeof a !== 'number' || !Number.isSafeInteger(a) || a <= 0) return null;
	if (typeof t !== 'number' || !Number.isSafeInteger(t) || t < 0) return null;
	if (u !== null && (typeof u !== 'string' || !USER_ID_PATTERN.test(u))) return null;
	return {currency: c, amountMinor: a, issuedAtMs: t, userId: u};
}

function verifyOperatorPassQuote(token: string, secret: string = getQuoteSecret()): OperatorPassQuote | null {
	if (token.length === 0 || token.length > QUOTE_MAX_LENGTH) {
		return null;
	}
	const parts = token.split('.');
	if (parts.length !== 2) {
		return null;
	}
	const [payload, signature] = parts;
	const expected = Buffer.from(signQuotePayload(payload, secret), 'utf8');
	const provided = Buffer.from(signature, 'utf8');
	if (expected.length !== provided.length || !timingSafeEqual(expected, provided)) {
		return null;
	}
	return parseQuotePayload(payload);
}

export interface OperatorPassCheckoutPriceInput {
	currency: OperatorPassCurrency;
	userId: string | null;
	quote: string | null | undefined;
	secret?: string;
}

export interface OperatorPassCheckoutPrice {
	amountMinor: number;
	priceChanged: boolean;
}

export function resolveOperatorPassCheckoutPrice(input: OperatorPassCheckoutPriceInput): OperatorPassCheckoutPrice {
	const verified = input.quote ? verifyOperatorPassQuote(input.quote, input.secret) : null;
	const quote = verified && (verified.userId === null || verified.userId === input.userId) ? verified : null;
	const amountMinor = getOperatorPassPriceMinor(input.currency);
	return {
		amountMinor,
		priceChanged: quote !== null && (quote.currency !== input.currency || quote.amountMinor !== amountMinor),
	};
}

export function getOperatorPassSessionExpiresAt(realNowMs = Date.now()): number {
	return Math.floor((realNowMs + OPERATOR_PASS_CHECKOUT_EXPIRY_MS) / 1000);
}
