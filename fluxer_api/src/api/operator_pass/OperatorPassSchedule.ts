// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHmac, timingSafeEqual} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {OperatorPassConfig} from '@app/api/config/APIConfig';
import {OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR} from '@app/api/operator_pass/OperatorPassPrices';
import {deriveOperatorPassSecret, OPERATOR_PASS_QUOTE_SECRET_LABEL} from '@app/api/operator_pass/OperatorPassSecrets';
import {getEnvCurrencyPreferences} from '@app/api/utils/CurrencyUtils';
import {
	OPERATOR_PASS_CURRENCIES,
	OPERATOR_PASS_PHASES,
	type OperatorPassCurrency,
	type OperatorPassPhase,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {ms} from 'itty-time';

export type OperatorPassSchedule = Pick<
	OperatorPassConfig,
	'saleOpensAt' | 'priceRisesAt' | 'graceMs' | 'clockOffsetMs' | 'standardPricesMinor'
>;

export type OperatorPassPricedPhase = Exclude<OperatorPassPhase, 'waitlist'>;

const STRIPE_CHECKOUT_MIN_EXPIRY_MS = ms('30 minutes');
const OPERATOR_PASS_CHECKOUT_EXPIRY_MS = ms('2 hours');
const STRIPE_CHECKOUT_EXPIRY_MARGIN_MS = ms('1 minute');

const QUOTE_VERSION = 2;
const QUOTE_MAX_LENGTH = 1024;
const USER_ID_PATTERN = /^\d{1,20}$/u;

export function getOperatorPassSchedule(): OperatorPassSchedule {
	return Config.operatorPass;
}

export function operatorPassNow(
	schedule: OperatorPassSchedule = getOperatorPassSchedule(),
	realNowMs = Date.now(),
): Date {
	return new Date(realNowMs + schedule.clockOffsetMs);
}

export function getOperatorPassPhase(
	now: Date,
	schedule: OperatorPassSchedule = getOperatorPassSchedule(),
	previewUnlocked = false,
): OperatorPassPhase {
	const at = now.getTime();
	if (!schedule.saleOpensAt || !schedule.priceRisesAt) {
		return 'waitlist';
	}
	if (at < schedule.saleOpensAt.getTime() && !previewUnlocked) {
		return 'waitlist';
	}
	if (at < schedule.priceRisesAt.getTime()) {
		return 'early_bird';
	}
	return 'standard';
}

function getOperatorPassPricedPhase(phase: OperatorPassPhase): OperatorPassPricedPhase {
	return phase === 'standard' ? 'standard' : 'early_bird';
}

export function getOperatorPassPriceMinor(
	phase: OperatorPassPhase,
	currency: OperatorPassCurrency,
	schedule: OperatorPassSchedule = getOperatorPassSchedule(),
): number {
	if (getOperatorPassPricedPhase(phase) === 'standard' && schedule.standardPricesMinor) {
		return schedule.standardPricesMinor[currency];
	}
	return OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR[currency];
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
	phase: OperatorPassPhase;
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
			p: quote.phase,
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
	phase: OperatorPassPhase;
	currency: OperatorPassCurrency;
	amountMinor: number;
	userId: string | null;
}

export function issueOperatorPassQuote(request: OperatorPassQuoteRequest, secret: string = getQuoteSecret()): string {
	return signOperatorPassQuote(
		{
			phase: request.phase,
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
	const {v, p, c, a, t, u} = decoded as Record<string, unknown>;
	if (v !== QUOTE_VERSION) return null;
	if (typeof p !== 'string' || !(OPERATOR_PASS_PHASES as ReadonlyArray<string>).includes(p)) return null;
	if (typeof c !== 'string' || !isOperatorPassCurrency(c)) return null;
	if (typeof a !== 'number' || !Number.isSafeInteger(a) || a <= 0) return null;
	if (typeof t !== 'number' || !Number.isSafeInteger(t) || t < 0) return null;
	if (u !== null && (typeof u !== 'string' || !USER_ID_PATTERN.test(u))) return null;
	return {phase: p as OperatorPassPhase, currency: c, amountMinor: a, issuedAtMs: t, userId: u};
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
	now: Date;
	schedule?: OperatorPassSchedule;
	currency: OperatorPassCurrency;
	userId: string | null;
	quote: string | null | undefined;
	secret?: string;
	previewUnlocked?: boolean;
}

export interface OperatorPassCheckoutPrice {
	phase: OperatorPassPhase;
	pricedPhase: OperatorPassPricedPhase;
	amountMinor: number;
	quoteHonored: boolean;
	priceChanged: boolean;
}

function isQuoteHonored(
	quote: OperatorPassQuote,
	input: OperatorPassCheckoutPriceInput,
	schedule: OperatorPassSchedule,
	phase: OperatorPassPhase,
): boolean {
	if (!schedule.priceRisesAt) {
		return false;
	}
	const risesAt = schedule.priceRisesAt.getTime();
	return (
		phase === 'standard' &&
		quote.phase === 'early_bird' &&
		quote.issuedAtMs < risesAt &&
		input.now.getTime() < risesAt + schedule.graceMs &&
		quote.currency === input.currency &&
		quote.amountMinor === OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR[input.currency]
	);
}

export function resolveOperatorPassCheckoutPrice(input: OperatorPassCheckoutPriceInput): OperatorPassCheckoutPrice {
	const schedule = input.schedule ?? getOperatorPassSchedule();
	const phase = getOperatorPassPhase(input.now, schedule, input.previewUnlocked);
	const verified = input.quote ? verifyOperatorPassQuote(input.quote, input.secret) : null;
	const quote = verified && (verified.userId === null || verified.userId === input.userId) ? verified : null;
	if (quote && isQuoteHonored(quote, input, schedule, phase)) {
		return {
			phase,
			pricedPhase: 'early_bird',
			amountMinor: OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR[input.currency],
			quoteHonored: true,
			priceChanged: false,
		};
	}
	const amountMinor = getOperatorPassPriceMinor(phase, input.currency, schedule);
	return {
		phase,
		pricedPhase: getOperatorPassPricedPhase(phase),
		amountMinor,
		quoteHonored: false,
		priceChanged: quote !== null && (quote.currency !== input.currency || quote.amountMinor !== amountMinor),
	};
}

export function getOperatorPassSessionExpiresAt(
	pricedPhase: OperatorPassPricedPhase,
	now: Date,
	schedule: OperatorPassSchedule = getOperatorPassSchedule(),
	realNowMs = Date.now(),
): number {
	let lifetimeMs = OPERATOR_PASS_CHECKOUT_EXPIRY_MS;
	if (pricedPhase === 'early_bird' && schedule.priceRisesAt) {
		const untilGraceEndMs = schedule.priceRisesAt.getTime() + schedule.graceMs - now.getTime();
		lifetimeMs = Math.max(
			STRIPE_CHECKOUT_MIN_EXPIRY_MS + STRIPE_CHECKOUT_EXPIRY_MARGIN_MS,
			Math.min(lifetimeMs, untilGraceEndMs),
		);
	}
	return Math.floor((realNowMs + lifetimeMs) / 1000);
}
