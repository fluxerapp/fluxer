// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes} from 'node:crypto';
import {Db} from '@app/api/database/CassandraTypes';
import type {OperatorPassCertificateRow, OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {Logger} from '@app/api/Logger';
import {
	isAcceptableOperatorPassCertificateName,
	normalizeOperatorPassCertificateName,
} from '@app/api/operator_pass/OperatorPassCertificateNames';
import {
	ENDED_PURCHASE_STATUSES,
	getOperatorPassCertificateUrl,
	getOperatorPassPassId,
	getOperatorPassUnitIndex,
	isOperatorPassCertificatePageCancelled,
} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {OperatorPassCertificateNameRejectedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassCertificateNameRejectedError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {canPrintOperatorPassCertificateName} from '@pkgs/email/src/operator_pass_certificate/OperatorPassCertificateNameText';
import {
	combineOperatorPassCertificatePages,
	getOperatorPassCertificateFilename,
	renderOperatorPassCertificatePage,
} from '@pkgs/email/src/operator_pass_certificate/OperatorPassCertificatePdf';
import {OPERATOR_PASS_TERMS_VERSION} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {seconds} from 'itty-time';

const NUMBER_COUNTER_KEY = 'operator_pass:certificate:number_counter';
const CLAIMED_NUMBER_TTL_SECONDS = seconds('7 days');
const MAX_NUMBER_ATTEMPTS = 500;

export type OperatorPassCertificateReissueReason = 'name_change' | 'correction' | 'objection' | 'link';

export interface OperatorPassCertificateFile {
	filename: string;
	bytes: Uint8Array;
}

function createOperatorPassCertificateLinkToken(): string {
	return randomBytes(16).toString('base64url');
}

function claimedNumberKey(passId: string): string {
	return `operator_pass:certificate:number:${passId}`;
}

export async function validateOperatorPassCertificateName(name: string, index?: number): Promise<string> {
	const normalized = normalizeOperatorPassCertificateName(name);
	if (
		!isAcceptableOperatorPassCertificateName(normalized) ||
		!(await canPrintOperatorPassCertificateName(normalized))
	) {
		throw new OperatorPassCertificateNameRejectedError(index);
	}
	return normalized;
}

const REVERSED_PURCHASE_STATUSES: ReadonlyArray<OperatorPassPurchaseRow['status']> = [
	'dispute_lost',
	'duplicate_refunded',
];

function isDeliveredWithoutUnits(purchase: OperatorPassPurchaseRow): boolean {
	return purchase.delivered_at != null && !ENDED_PURCHASE_STATUSES.includes(purchase.status);
}

function isPaymentReversed(purchase: OperatorPassPurchaseRow): boolean {
	return REVERSED_PURCHASE_STATUSES.includes(purchase.status);
}

function resolveHolderName(purchase: OperatorPassPurchaseRow, index: number): string {
	const names = purchase.certificate_names ?? [];
	const chosen = names[index - 1] ?? names[0];
	if (!chosen) {
		throw new Error(`Operator Pass purchase ${purchase.checkout_session_id} has no certificate names`);
	}
	return normalizeOperatorPassCertificateName(chosen);
}

export class OperatorPassCertificateService {
	constructor(
		private readonly repository: OperatorPassRepository,
		private readonly cacheService: ICacheService,
		private readonly kvClient: IKVProvider,
	) {}

	async issueForPurchase(purchase: OperatorPassPurchaseRow): Promise<void> {
		const paidAt = purchase.purchased_at ?? operatorPassNow();
		const quantity = Math.max(1, purchase.quantity ?? 1);
		for (let index = 1; index <= quantity; index++) {
			const passId = getOperatorPassPassId(purchase.checkout_session_id, index);
			const [first] = await this.repository.listCertificateVersions(passId);
			const row =
				first ??
				(await this.issuePage({
					passId,
					purchase,
					holderName: resolveHolderName(purchase, index),
					paidAt,
				}));
			await this.repository.indexCertificate(row);
		}
		const pages = new Map(
			(await this.repository.listCertificatesByPurchase(purchase.checkout_session_id)).map((row) => [row.pass_id, row]),
		);
		for (let index = 1; index <= quantity; index++) {
			const page = pages.get(getOperatorPassPassId(purchase.checkout_session_id, index));
			if (!page || page.pdf.length === 0) {
				throw new Error(
					`Operator certificate page ${index} of ${purchase.checkout_session_id} is not available for download`,
				);
			}
		}
	}

	async loadPurchaseFile(purchase: OperatorPassPurchaseRow): Promise<OperatorPassCertificateFile | null> {
		if (isPaymentReversed(purchase)) {
			return null;
		}
		const purchaseId = purchase.checkout_session_id;
		const units = new Map((await this.repository.listUnitsByPurchase(purchaseId)).map((unit) => [unit.pass_id, unit]));
		const rows: Array<OperatorPassCertificateRow> = [];
		for (const row of await this.repository.listCertificatesByPurchase(purchaseId)) {
			if (row.pdf.length === 0 || row.cancelled_at != null) {
				continue;
			}
			const unit = units.get(row.pass_id);
			if (!unit) {
				if (isDeliveredWithoutUnits(purchase)) {
					rows.push(row);
				}
				continue;
			}
			if (isOperatorPassCertificatePageCancelled(unit)) {
				await this.repository.markCertificateCancelled(unit.pass_id, unit.ended_at ?? operatorPassNow());
				continue;
			}
			rows.push(row);
		}
		const [first] = rows;
		if (!first) {
			return null;
		}
		const issuedAt = rows.reduce(
			(latest, row) => (row.issued_at.getTime() > latest.getTime() ? row.issued_at : latest),
			first.issued_at,
		);
		return {
			filename: getOperatorPassCertificateFilename(first.number),
			bytes: await combineOperatorPassCertificatePages(
				rows.map((row) => new Uint8Array(row.pdf)),
				first.number,
				issuedAt,
			),
		};
	}

	async cancelReversedPages(
		purchase: OperatorPassPurchaseRow,
		reason: 'refund' | 'dispute',
		count: number = Number.POSITIVE_INFINITY,
	): Promise<void> {
		const units = await this.repository.listUnitsByPurchase(purchase.checkout_session_id);
		let missing = count - units.filter(isOperatorPassCertificatePageCancelled).length;
		const endedOtherwise = units
			.filter((unit) => unit.status !== 'held' && !isOperatorPassCertificatePageCancelled(unit))
			.sort((left, right) => getOperatorPassUnitIndex(right.pass_id) - getOperatorPassUnitIndex(left.pass_id));
		const now = operatorPassNow();
		for (const unit of endedOtherwise) {
			if (missing <= 0) {
				break;
			}
			if (!(await this.repository.updateUnit(unit, {end_reason: Db.set(reason), ended_at: Db.set(now)}))) {
				throw new Error(`Operator Pass ${unit.pass_id} changed while its certificate page was being cancelled`);
			}
			missing--;
		}
		await this.syncCancellations(purchase);
	}

	async syncCancellations(purchase: OperatorPassPurchaseRow): Promise<void> {
		const units = await this.repository.listUnitsByPurchase(purchase.checkout_session_id);
		for (const unit of units) {
			if (!isOperatorPassCertificatePageCancelled(unit)) {
				continue;
			}
			await this.repository.markCertificateCancelled(unit.pass_id, unit.ended_at ?? operatorPassNow());
		}
	}

	async reissuePage(params: {
		passId: string;
		holderName: string;
		reason: OperatorPassCertificateReissueReason;
	}): Promise<OperatorPassCertificateRow> {
		const latest = await this.repository.findLatestCertificate(params.passId);
		if (!latest) {
			throw InputValidationError.create('pass_id', 'This pass has no certificate page');
		}
		if (latest.cancelled_at) {
			throw InputValidationError.create('pass_id', 'This certificate page is cancelled');
		}
		if (latest.pdf.length === 0) {
			throw InputValidationError.create('pass_id', 'This certificate page was removed');
		}
		const holderName =
			params.reason === 'link' ? latest.holder_name : await validateOperatorPassCertificateName(params.holderName);
		const linkToken =
			params.reason !== 'link' && latest.link_token ? latest.link_token : createOperatorPassCertificateLinkToken();
		const issuedAt = operatorPassNow();
		const pdf = await renderOperatorPassCertificatePage({
			number: latest.number,
			holderName,
			paidAt: latest.paid_at,
			earlyBird: latest.phase === 'early_bird',
			termsVersion: latest.terms_version,
			issuedAt,
			copy: holderName === latest.holder_name ? 'same_name' : 'new_name',
			source: latest.source,
			verificationUrl: getOperatorPassCertificateUrl(linkToken),
		});
		const row: OperatorPassCertificateRow = {
			...latest,
			version: latest.version + 1,
			holder_name: holderName,
			issued_at: issuedAt,
			sha256: createHash('sha256').update(pdf).digest('hex'),
			pdf: Buffer.from(pdf),
			cancelled_at: null,
			reissue_reason: params.reason,
			link_token: linkToken,
		};
		if (!(await this.repository.createCertificate(row))) {
			throw new ConflictError({
				code: APIErrorCodes.CONFLICT,
				message: 'Another copy of this certificate page was issued at the same time',
			});
		}
		if (params.reason === 'objection') {
			await this.repository.clearCertificatePersonalData(row.pass_id, row.version);
		}
		if (latest.link_token && latest.link_token !== linkToken) {
			await this.repository.deleteCertificateLink(latest.link_token);
		}
		Logger.info(
			{passId: row.pass_id, number: row.number, version: row.version, reason: params.reason},
			'Issued a new copy of an Operator certificate page',
		);
		return row;
	}

	private async issuePage(params: {
		passId: string;
		purchase: OperatorPassPurchaseRow;
		holderName: string;
		paidAt: Date;
	}): Promise<OperatorPassCertificateRow> {
		const {passId, purchase, holderName, paidAt} = params;
		const number = await this.allocateNumber(passId);
		const issuedAt = operatorPassNow();
		const termsVersion = purchase.operator_pass_terms_version ?? OPERATOR_PASS_TERMS_VERSION;
		const linkToken = createOperatorPassCertificateLinkToken();
		const pdf = await renderOperatorPassCertificatePage({
			number,
			holderName,
			paidAt,
			earlyBird: purchase.phase === 'early_bird',
			termsVersion,
			issuedAt,
			copy: 'original',
			source: 'purchase',
			verificationUrl: getOperatorPassCertificateUrl(linkToken),
		});
		return this.writeFirstPage({
			pass_id: passId,
			version: 1,
			number,
			purchase_id: purchase.checkout_session_id,
			holder_name: holderName,
			issued_at: issuedAt,
			paid_at: paidAt,
			phase: purchase.phase,
			terms_version: termsVersion,
			sha256: createHash('sha256').update(pdf).digest('hex'),
			pdf: Buffer.from(pdf),
			source: 'purchase',
			cancelled_at: null,
			reissue_reason: null,
			link_token: linkToken,
		});
	}

	async issueVisionaryPage(params: {
		passId: string;
		holderName: string;
		visionarySince: Date;
		termsVersion: string;
	}): Promise<OperatorPassCertificateRow> {
		const existing = await this.repository.findLatestCertificate(params.passId);
		if (existing) {
			return existing;
		}
		const holderName = await validateOperatorPassCertificateName(params.holderName);
		const number = await this.allocateNumber(params.passId);
		const issuedAt = operatorPassNow();
		const linkToken = createOperatorPassCertificateLinkToken();
		const pdf = await renderOperatorPassCertificatePage({
			number,
			holderName,
			paidAt: params.visionarySince,
			earlyBird: false,
			termsVersion: params.termsVersion,
			issuedAt,
			copy: 'original',
			source: 'visionary',
			verificationUrl: getOperatorPassCertificateUrl(linkToken),
		});
		return this.writeFirstPage({
			pass_id: params.passId,
			version: 1,
			number,
			purchase_id: null,
			holder_name: holderName,
			issued_at: issuedAt,
			paid_at: params.visionarySince,
			phase: 'visionary',
			terms_version: params.termsVersion,
			sha256: createHash('sha256').update(pdf).digest('hex'),
			pdf: Buffer.from(pdf),
			source: 'visionary',
			cancelled_at: null,
			reissue_reason: null,
			link_token: linkToken,
		});
	}

	async listPurchaseCertificateUrls(purchase: OperatorPassPurchaseRow): Promise<Map<string, string>> {
		const urls = new Map<string, string>();
		if (purchase.delivered_at == null || isPaymentReversed(purchase)) {
			return urls;
		}
		for (const page of await this.repository.listCertificateLinksByPurchase(purchase.checkout_session_id)) {
			if (page.link_token && page.cancelled_at == null) {
				urls.set(page.pass_id, getOperatorPassCertificateUrl(page.link_token));
			}
		}
		return urls;
	}

	async loadLinkedPage(token: string): Promise<{page: OperatorPassCertificateRow; cancelled: boolean} | null> {
		const link = await this.repository.findCertificateLink(token);
		if (!link) {
			return null;
		}
		const page = await this.repository.findLatestCertificate(link.pass_id);
		if (!page || page.link_token !== token || page.pdf.length === 0) {
			return null;
		}
		if (page.cancelled_at != null) {
			return {page, cancelled: true};
		}
		if (page.source !== 'purchase') {
			return {page, cancelled: false};
		}
		const purchase = page.purchase_id ? await this.repository.findPurchase(page.purchase_id) : null;
		if (!purchase) {
			return null;
		}
		if (isPaymentReversed(purchase)) {
			return {page, cancelled: true};
		}
		const unit = await this.repository.findUnit(page.pass_id);
		if (!unit) {
			return {page, cancelled: ENDED_PURCHASE_STATUSES.includes(purchase.status)};
		}
		if (isOperatorPassCertificatePageCancelled(unit)) {
			await this.repository.markCertificateCancelled(unit.pass_id, unit.ended_at ?? operatorPassNow());
			return {page, cancelled: true};
		}
		return {page, cancelled: false};
	}

	async loadVisionaryFile(passId: string): Promise<OperatorPassCertificateFile | null> {
		const page = await this.repository.findLatestCertificate(passId);
		if (page?.source !== 'visionary' || page.pdf.length === 0 || page.cancelled_at != null) {
			return null;
		}
		return {
			filename: getOperatorPassCertificateFilename(page.number),
			bytes: await combineOperatorPassCertificatePages([new Uint8Array(page.pdf)], page.number, page.issued_at),
		};
	}

	private async writeFirstPage(row: OperatorPassCertificateRow): Promise<OperatorPassCertificateRow> {
		if (await this.repository.createCertificate(row)) {
			Logger.info({passId: row.pass_id, number: row.number}, 'Issued an Operator certificate page');
			return row;
		}
		const existing = await this.repository.findLatestCertificate(row.pass_id);
		if (!existing) {
			throw new Error(`Operator certificate for ${row.pass_id} was not written`);
		}
		return existing;
	}

	private async allocateNumber(passId: string): Promise<number> {
		const claimed = await this.cacheService.get<number>(claimedNumberKey(passId));
		if (claimed) {
			const owner = await this.repository.findCertificateNumber(claimed);
			if (owner?.pass_id === passId) {
				return claimed;
			}
		}
		for (let attempt = 0; attempt < MAX_NUMBER_ATTEMPTS; attempt++) {
			const candidate = await this.nextCandidateNumber();
			if (await this.repository.claimCertificateNumber(candidate, passId)) {
				await this.cacheService.set(claimedNumberKey(passId), candidate, CLAIMED_NUMBER_TTL_SECONDS);
				return candidate;
			}
			const owner = await this.repository.findCertificateNumber(candidate);
			if (owner?.pass_id === passId) {
				return candidate;
			}
		}
		throw new Error('Could not allocate an Operator certificate number');
	}

	private async nextCandidateNumber(): Promise<number> {
		const next = await this.kvClient.incr(NUMBER_COUNTER_KEY);
		if (next !== 1) {
			return next;
		}
		const highest = await this.repository.findHighestCertificateNumber();
		if (highest === 0) {
			return 1;
		}
		await this.kvClient.set(NUMBER_COUNTER_KEY, String(highest));
		Logger.warn({highest}, 'Seeded the Operator certificate number counter from the highest issued number');
		return this.kvClient.incr(NUMBER_COUNTER_KEY);
	}
}
