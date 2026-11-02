// SPDX-License-Identifier: AGPL-3.0-or-later

import type {OperatorPassPurchaseRow, OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import {buildOperatorPassDeliveryExport} from '@app/api/operator_pass/OperatorPassDeliveryExport';
import {expect, test} from 'vitest';

function purchase(id: string, deliveredAt: string | null, extra: Partial<OperatorPassPurchaseRow> = {}) {
	return {
		checkout_session_id: id,
		quantity: 2,
		amount_minor: 39800,
		currency: 'USD',
		phase: 'early_bird',
		payment_intent_id: `pi_${id}`,
		charge_id: `ch_${id}`,
		delivered_at: deliveredAt ? new Date(deliveredAt) : null,
		confirmation_sent_at: new Date('2026-11-05T17:09:21.002Z'),
		holder_had_pass: false,
		business_buyer: false,
		withdrawal_country: null,
		...extra,
	} as OperatorPassPurchaseRow;
}

test('the delivery export has one row per pass of each purchase delivered in the month, in the columns bokfor reads', async () => {
	const rows = [
		purchase('cs_nov', '2026-11-05T17:09:29.334Z'),
		purchase('cs_oct', '2026-10-31T23:59:59.999Z'),
		purchase('cs_dec', '2026-12-01T00:00:00.000Z'),
		purchase('cs_pending', null),
		purchase('cs_br', '2026-11-30T23:59:59.999Z', {
			quantity: 1,
			amount_minor: 102000,
			currency: 'BRL',
			withdrawal_country: 'BR',
			holder_had_pass: true,
			business_buyer: true,
			invoice_number: 'A,"1"',
		}),
	];
	const units = (id: string, count: number) =>
		Array.from({length: count}, (_, index) => ({pass_id: `${id}:${index + 1}`}) as OperatorPassUnitRow);
	const exported = await buildOperatorPassDeliveryExport(
		{
			scanPurchases: async () => ({rows, pageState: null}),
			listUnitsByPurchase: async (id) => units(id, rows.find((row) => row.checkout_session_id === id)?.quantity ?? 0),
		},
		'2026-11',
	);
	const [header, ...lines] = exported.csv.trimEnd().split('\r\n');
	const columns = header!.split(',');
	for (const required of [
		'pass_id',
		'source',
		'charge_id',
		'payment_intent_id',
		'delivered_at',
		'holder_had_pass',
		'is_business',
		'withdrawal_country',
		'confirmation_sent_at',
	]) {
		expect(columns).toContain(required);
	}
	expect(exported).toMatchObject({filename: 'operator-pass-leveranser-2026-11.csv', rows: 3, purchases: 2});
	const field = (line: string, name: string) => line.split(',')[columns.indexOf(name)];
	expect(lines.map((line) => field(line, 'pass_id'))).toEqual(['cs_nov:1', 'cs_nov:2', 'cs_br:1']);
	expect(field(lines[0]!, 'delivered_at')).toBe('2026-11-05T17:09:29.334Z');
	expect(field(lines[0]!, 'holder_had_pass')).toBe('false');
	expect(field(lines[0]!, 'source')).toBe('purchase');
	expect(field(lines[2]!, 'is_business')).toBe('true');
	expect(field(lines[2]!, 'withdrawal_country')).toBe('BR');
	expect(lines[2]).toContain('"A,""1"""');
});

test('a pass reversed by an accepted dispute carries the du: credit note next to any refund notes', async () => {
	const rows = [
		purchase('cs_du', '2026-11-05T17:09:29.334Z', {dispute_id: 'dp_1', withdrawal_refund_id: 're_1'}),
		purchase('cs_fraud', '2026-11-06T10:00:00.000Z', {dispute_id: 'dp_2'}),
	];
	const notes: Record<string, {document_id: bigint; display_number: string}> = {
		're:re_1': {document_id: 1n, display_number: 'KR-1'},
		'du:dp_1': {document_id: 2n, display_number: 'KR-2'},
	};
	const exported = await buildOperatorPassDeliveryExport(
		{
			scanPurchases: async () => ({rows, pageState: null}),
			listUnitsByPurchase: async (id) =>
				[{pass_id: `${id}:1`, ended_at: new Date('2026-12-01T00:00:00.000Z')}] as Array<OperatorPassUnitRow>,
		},
		'2026-11',
		{findDocumentBySource: async (source: string) => (notes[source] ?? null) as never},
	);
	const [header, ...lines] = exported.csv.trimEnd().split('\r\n');
	const column = header!.split(',').indexOf('credit_note_id');
	expect(lines.map((line) => line.split(',')[column])).toEqual(['KR-1 KR-2', '']);
});
