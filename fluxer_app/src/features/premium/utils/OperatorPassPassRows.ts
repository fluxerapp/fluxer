// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	OperatorPassOfferResponse,
	OperatorPassPassesResponse,
	OperatorPassPurchaseResponse,
	OperatorPassUnitResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

type HeldPassGroup = [OperatorPassUnitResponse, ...Array<OperatorPassUnitResponse>];

export type OperatorPassPassRow =
	| {kind: 'incoming'; key: string; offer: OperatorPassOfferResponse}
	| {kind: 'undelivered'; key: string; purchase: OperatorPassPurchaseResponse}
	| {
			kind: 'held';
			key: string;
			passes: Readonly<HeldPassGroup>;
			date: string;
			receivedFrom: OperatorPassOfferResponse['sender'];
			paused: boolean;
			passOn: OperatorPassUnitResponse | null;
			lockEndsAt: string | null;
			cancel: OperatorPassUnitResponse | null;
			purchaseActions: boolean;
	  }
	| {kind: 'offered'; key: string; pass: OperatorPassUnitResponse; offer: OperatorPassOfferResponse}
	| {kind: 'passed_on'; key: string; offer: OperatorPassOfferResponse};

export function isOperatorPassBenefitsRemoved(holdings: Pick<OperatorPassPassesResponse, 'count' | 'passes'>): boolean {
	return holdings.count === 0 && holdings.passes.length > 0;
}

function groupKey(pass: OperatorPassUnitResponse): string {
	if (pass.source !== 'purchase' || !pass.purchase_id) return `${pass.source}:${pass.id}`;
	return `purchase:${pass.purchase_id}${pass.paused ? ':paused' : ''}`;
}

function isFuture(iso: string | null, now: number): boolean {
	return iso != null && Date.parse(iso) > now;
}

export function buildOperatorPassPassRows(
	holdings: OperatorPassPassesResponse,
	purchases: ReadonlyArray<OperatorPassPurchaseResponse>,
	now: number = Date.now(),
): Array<OperatorPassPassRow> {
	const removed = isOperatorPassBenefitsRemoved(holdings);
	const rows: Array<OperatorPassPassRow> = [];
	for (const offer of holdings.incoming_offers) {
		if (offer.status === 'pending') rows.push({kind: 'incoming', key: `incoming:${offer.id}`, offer});
	}
	for (const purchase of purchases) {
		if (purchase.can_withdraw && !purchase.delivered_at) {
			rows.push({kind: 'undelivered', key: `undelivered:${purchase.id}`, purchase});
		}
	}
	const groups = new Map<string, HeldPassGroup>();
	const dated: Array<{date: string; row: OperatorPassPassRow}> = [];
	for (const pass of holdings.passes) {
		if (pass.offer) {
			dated.push({date: pass.delivered_at, row: {kind: 'offered', key: `offered:${pass.id}`, pass, offer: pass.offer}});
			continue;
		}
		const key = groupKey(pass);
		const group = groups.get(key);
		if (group) group.push(pass);
		else groups.set(key, [pass]);
	}
	for (const [key, passes] of groups) {
		const date = passes.reduce(
			(earliest, pass) => (Date.parse(pass.delivered_at) < Date.parse(earliest) ? pass.delivered_at : earliest),
			passes[0].delivered_at,
		);
		const passOn = removed ? null : (passes.find((pass) => pass.can_pass_on && !pass.paused) ?? null);
		const cancel =
			passes.find((pass) => pass.can_withdraw && pass.withdrawal_ends_on && isFuture(pass.withdrawal_ends_at, now)) ??
			null;
		const lockEndsAt = passOn && isFuture(passOn.transfer_lock_ends_at, now) ? passOn.transfer_lock_ends_at : null;
		dated.push({
			date,
			row: {
				kind: 'held',
				key: `held:${key}`,
				passes,
				date,
				receivedFrom: passes[0].source === 'transfer' ? (passes[0].received_from ?? null) : null,
				paused: passes.some((pass) => pass.paused),
				passOn,
				lockEndsAt,
				cancel,
				purchaseActions: !(key.endsWith(':paused') && groups.has(key.slice(0, -':paused'.length))),
			},
		});
	}
	dated.sort((left, right) => Date.parse(left.date) - Date.parse(right.date));
	rows.push(...dated.map(({row}) => row));
	for (const offer of holdings.passed_on ?? []) {
		rows.push({kind: 'passed_on', key: `passed_on:${offer.id}`, offer});
	}
	return rows;
}
