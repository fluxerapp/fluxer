// SPDX-License-Identifier: AGPL-3.0-or-later

import {getWithdrawalPeriodEndsAt, getWithdrawalPeriodLastDay} from '@app/api/billing/WithdrawalPeriod';
import {
	getOperatorPassCountryWithdrawalDeadline,
	getOperatorPassWithdrawalEndsAt,
	getOperatorPassWithdrawalEndsOn,
} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import {isEuEeaCountryCode} from '@fluxer/constants/src/EuropeanEconomicArea';
import {ms} from 'itty-time';

const BRAZIL_WITHDRAWAL_DAYS = 7;
const UK_WITHDRAWAL_DAYS = 14;
const UK_TIME_ZONE = 'Europe/London';
const DAY_MS = ms('1 day');

export type PlutoniumWithdrawalRule = 'eu' | 'uk' | 'brazil';

export interface PlutoniumWithdrawalPeriod {
	endsAt: Date;
	endsOn: string;
	rule: PlutoniumWithdrawalRule;
}

const londonClock = new Intl.DateTimeFormat('en-GB', {
	timeZone: UK_TIME_ZONE,
	year: 'numeric',
	month: '2-digit',
	day: '2-digit',
	hour: '2-digit',
	minute: '2-digit',
	second: '2-digit',
	hourCycle: 'h23',
});

function londonOffsetMs(at: Date): number {
	const parts = Object.fromEntries(londonClock.formatToParts(at).map((part) => [part.type, Number(part.value)]));
	const wallClock = Date.UTC(parts.year!, parts.month! - 1, parts.day!, parts.hour!, parts.minute!, parts.second!);
	return wallClock - Math.floor(at.getTime() / 1000) * 1000;
}

function ukWithdrawalPeriod(paidAt: Date): PlutoniumWithdrawalPeriod {
	const local = new Date(paidAt.getTime() + londonOffsetMs(paidAt));
	const year = local.getUTCFullYear();
	const month = local.getUTCMonth();
	const day = local.getUTCDate() + UK_WITHDRAWAL_DAYS;
	const nextMidnight = Date.UTC(year, month, day + 1);
	return {
		endsAt: new Date(nextMidnight - londonOffsetMs(new Date(nextMidnight))),
		endsOn: new Date(Date.UTC(year, month, day)).toISOString().slice(0, 10),
		rule: 'uk',
	};
}

export function plutoniumWithdrawalPeriod(
	metadata: Readonly<Record<string, string>> | null | undefined,
	paidAt: Date,
): PlutoniumWithdrawalPeriod | null {
	const country =
		metadata?.purchase_geoip_country_code ?? metadata?.purchase_client_country_code ?? metadata?.country_code ?? null;
	const business = metadata?.is_business === 'true';
	if (metadata?.withdrawal_country === 'BR' || country === 'BR') {
		if (business && metadata?.cnpj) {
			return null;
		}
		const endsAt = getOperatorPassWithdrawalEndsAt(paidAt, BRAZIL_WITHDRAWAL_DAYS);
		return {
			endsAt: getOperatorPassCountryWithdrawalDeadline(endsAt),
			endsOn: getOperatorPassWithdrawalEndsOn(endsAt),
			rule: 'brazil',
		};
	}
	if (business) {
		return null;
	}
	if (country === 'GB') {
		return ukWithdrawalPeriod(paidAt);
	}
	if (metadata?.eu_withdrawal_waiver_required === 'true' || isEuEeaCountryCode(country)) {
		return {endsAt: getWithdrawalPeriodEndsAt(paidAt), endsOn: getWithdrawalPeriodLastDay(paidAt), rule: 'eu'};
	}
	return null;
}

export function isProRataWithdrawal(metadata: Readonly<Record<string, string>> | null | undefined): boolean {
	return metadata?.withdrawal_pro_rata === 'true';
}

export function opensPlutoniumWithdrawalPeriod(
	rule: PlutoniumWithdrawalRule,
	payment: {
		billingReason: string | null;
		priceId: string | null;
		metadata: Readonly<Record<string, string>> | null;
	},
	previousPriceId: string | null,
): boolean {
	if (payment.billingReason === 'subscription_create' || payment.billingReason === 'subscription_update') {
		return true;
	}
	if (payment.billingReason !== 'subscription_cycle') {
		return false;
	}
	if (rule === 'eu' && !isProRataWithdrawal(payment.metadata)) {
		return true;
	}
	return previousPriceId !== null && payment.priceId !== previousPriceId;
}

export function proRataWithdrawalRefundMinor(
	amountMinor: number,
	usage: {startedAt: Date; endsAt: Date} | null,
	at: Date,
): number {
	if (!usage) {
		return amountMinor;
	}
	const periodDays = Math.max(1, Math.round((usage.endsAt.getTime() - usage.startedAt.getTime()) / DAY_MS));
	const usedDays = Math.min(periodDays, Math.max(0, Math.ceil((at.getTime() - usage.startedAt.getTime()) / DAY_MS)));
	return amountMinor - Math.round((amountMinor * usedDays) / periodDays);
}
