// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import type {OperatorPassPurchaseRow, OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import type Stripe from 'stripe';

const SCAN_PAGE_SIZE = 500;

const COLUMNS = [
	'pass_id',
	'purchase_id',
	'source',
	'charge_id',
	'payment_intent_id',
	'delivered_at',
	'holder_had_pass',
	'is_business',
	'withdrawal_country',
	'confirmation_sent_at',
	'invoice_id',
	'invoice_number',
	'currency',
	'unit_amount_minor',
	'quantity',
	'amount_minor',
	'stripe_tax_minor',
	'phase',
	'customer_country',
	'card_country',
	'vat_id_check',
	'terms_version',
	'terms_sha256',
	'consent_version',
	'consent_at',
	'stripe_consent',
	'confirmation_states_waiver',
	'waiver_consent_at',
	'withdrawal_days',
	'withdrawal_ends_at',
	'badge_active_at',
	'wall_choice',
	'wall_published_at',
	'bonus_role_outcome',
	'ended_at',
	'end_kind',
	'refund_id',
	'refund_minor',
	'credit_note_id',
	'dispute_id',
	'last_row_hash',
] as const;

type Column = (typeof COLUMNS)[number];

const MONTH_PATTERN = /^(\d{4})-(0[1-9]|1[0-2])$/u;

interface OperatorPassDeliveryExport {
	filename: string;
	csv: string;
	rows: number;
	purchases: number;
}

function monthRange(month: string): {start: Date; end: Date} {
	const match = MONTH_PATTERN.exec(month);
	if (!match) {
		throw new Error(`Not a YYYY-MM month: ${month}`);
	}
	const year = Number(match[1]);
	const monthIndex = Number(match[2]) - 1;
	return {start: new Date(Date.UTC(year, monthIndex, 1)), end: new Date(Date.UTC(year, monthIndex + 1, 1))};
}

function csvField(value: string): string {
	return /[",\r\n]/u.test(value) ? `"${value.replaceAll('"', '""')}"` : value;
}

function iso(value: Date | null | undefined): string {
	return value ? value.toISOString() : '';
}

function bool(value: boolean | null | undefined): string {
	return value == null ? '' : String(value);
}

function wallChoice(purchase: OperatorPassPurchaseRow): string {
	if (purchase.wall_opt_out == null) return '';
	return purchase.wall_opt_out ? 'opted_out' : 'listed';
}

type CreditNoteLookup = Pick<BillingDocumentRepository, 'findDocumentBySource'>;
type RefundLookup = Pick<Stripe, 'refunds'>;

const WITHDRAWAL_REFUND_PREFIX = 'operator_pass_withdrawal:';

interface PurchaseCreditNotes {
	byPass: Map<string, Array<string>>;
	wholePurchase: Array<string>;
	dispute: Array<string>;
}

async function purchaseCreditNotes(
	documents: CreditNoteLookup | null,
	stripe: RefundLookup | null,
	purchase: OperatorPassPurchaseRow,
): Promise<PurchaseCreditNotes> {
	const notes: PurchaseCreditNotes = {byPass: new Map(), wholePurchase: [], dispute: []};
	if (!documents) return notes;
	const refunds = new Map<string, string | null>();
	if (stripe && purchase.payment_intent_id) {
		const listed = await stripe.refunds.list({payment_intent: purchase.payment_intent_id, limit: 100});
		for (const refund of listed.data) {
			if (refund.status !== 'succeeded') continue;
			const scope = (refund.metadata?.rejection_reason ?? '').startsWith(WITHDRAWAL_REFUND_PREFIX)
				? (refund.metadata?.rejection_reason ?? '').slice(WITHDRAWAL_REFUND_PREFIX.length)
				: null;
			refunds.set(refund.id, scope && !scope.startsWith('all:') ? scope : null);
		}
	} else if (purchase.withdrawal_refund_id) {
		refunds.set(purchase.withdrawal_refund_id, null);
	}
	const seen = new Set<bigint>();
	const add = (number: string, passId: string | null) => {
		if (!passId) {
			notes.wholePurchase.push(number);
			return;
		}
		notes.byPass.set(passId, [...(notes.byPass.get(passId) ?? []), number]);
	};
	for (const [refundId, passId] of refunds) {
		const document = await documents.findDocumentBySource(`re:${refundId}`);
		if (document && !seen.has(document.document_id)) {
			seen.add(document.document_id);
			add(document.display_number, passId);
		}
	}
	if (purchase.dispute_id) {
		const document = await documents.findDocumentBySource(`du:${purchase.dispute_id}`);
		if (document && !seen.has(document.document_id)) {
			notes.dispute.push(document.display_number);
		}
	}
	return notes;
}

function unitCreditNotes(notes: PurchaseCreditNotes, unit: OperatorPassUnitRow): string {
	const own = notes.byPass.get(unit.pass_id) ?? (unit.ended_at ? notes.wholePurchase : []);
	return [...own, ...notes.dispute].join(' ');
}

function passRow(
	purchase: OperatorPassPurchaseRow,
	unit: OperatorPassUnitRow,
	creditNoteId: string,
): Record<Column, string> {
	const quantity = Math.max(1, purchase.quantity ?? 1);
	return {
		pass_id: unit.pass_id,
		purchase_id: purchase.checkout_session_id,
		source: purchase.preview_label == null ? 'purchase' : 'preview',
		charge_id: purchase.charge_id ?? '',
		payment_intent_id: purchase.payment_intent_id ?? '',
		delivered_at: iso(purchase.delivered_at),
		holder_had_pass: bool(purchase.holder_had_pass),
		is_business: bool(purchase.business_buyer),
		withdrawal_country: purchase.withdrawal_country ?? '',
		confirmation_sent_at: iso(purchase.confirmation_sent_at),
		invoice_id: purchase.invoice_document_id?.toString() ?? purchase.invoice_id ?? '',
		invoice_number: purchase.invoice_number ?? '',
		currency: purchase.currency.toLowerCase(),
		unit_amount_minor: String(Math.round(purchase.amount_minor / quantity)),
		quantity: String(quantity),
		amount_minor: String(purchase.amount_minor),
		stripe_tax_minor: '',
		phase: purchase.phase,
		customer_country: '',
		card_country: '',
		vat_id_check: '',
		terms_version: purchase.operator_pass_terms_version ?? '',
		terms_sha256: '',
		consent_version: purchase.waiver_consent_version ?? '',
		consent_at: iso(purchase.waiver_consent_at),
		stripe_consent: purchase.stripe_terms_consent ?? '',
		confirmation_states_waiver: purchase.confirmation_sent_at ? String(purchase.withdrawal_days == null) : '',
		waiver_consent_at: iso(purchase.waiver_consent_at),
		withdrawal_days: purchase.withdrawal_days?.toString() ?? '',
		withdrawal_ends_at: iso(purchase.withdrawal_ends_at),
		badge_active_at: '',
		wall_choice: wallChoice(purchase),
		wall_published_at: '',
		bonus_role_outcome: '',
		ended_at: iso(unit.ended_at),
		end_kind: unit.end_reason ?? '',
		refund_id: purchase.withdrawal_refund_id ?? '',
		refund_minor: '',
		credit_note_id: creditNoteId,
		dispute_id: purchase.dispute_id ?? '',
		last_row_hash: '',
	};
}

export async function buildOperatorPassDeliveryExport(
	repository: Pick<OperatorPassRepository, 'scanPurchases' | 'listUnitsByPurchase'>,
	month: string,
	documents: CreditNoteLookup | null = null,
	stripe: RefundLookup | null = null,
): Promise<OperatorPassDeliveryExport> {
	const range = monthRange(month);
	const delivered: Array<OperatorPassPurchaseRow> = [];
	let pageState: string | null = null;
	do {
		const page = await repository.scanPurchases(SCAN_PAGE_SIZE, pageState);
		for (const purchase of page.rows) {
			const at = purchase.delivered_at?.getTime();
			if (at != null && at >= range.start.getTime() && at < range.end.getTime()) {
				delivered.push(purchase);
			}
		}
		pageState = page.pageState;
	} while (pageState);
	delivered.sort(
		(left, right) =>
			(left.delivered_at?.getTime() ?? 0) - (right.delivered_at?.getTime() ?? 0) ||
			left.checkout_session_id.localeCompare(right.checkout_session_id),
	);
	const lines = [COLUMNS.join(',')];
	for (const purchase of delivered) {
		const notes = await purchaseCreditNotes(documents, stripe, purchase);
		for (const unit of await repository.listUnitsByPurchase(purchase.checkout_session_id)) {
			const row = passRow(purchase, unit, unitCreditNotes(notes, unit));
			lines.push(COLUMNS.map((column) => csvField(row[column])).join(','));
		}
	}
	return {
		filename: `operator-pass-leveranser-${month}.csv`,
		csv: `${lines.join('\r\n')}\r\n`,
		rows: lines.length - 1,
		purchases: delivered.length,
	};
}
