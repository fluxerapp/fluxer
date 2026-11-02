// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {OperatorPassPurchaseRow, OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import {Logger} from '@app/api/Logger';
import {
	forgetOperatorPassCertificateNumberClaim,
	resetOperatorPassCertificateNumberCounter,
} from '@app/api/operator_pass/OperatorPassCertificateService';
import {
	type OperatorPassFulfilmentService,
	operatorPassPurchaseCacheKeys,
} from '@app/api/operator_pass/OperatorPassFulfilmentService';
import type {OperatorPassRepository, OperatorPassScanPage} from '@app/api/operator_pass/OperatorPassRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

const SCAN_PAGE_SIZE = 500;
const RESET_LOCK_KEY = 'operator_pass:preview_reset_lock';
const RESET_LOCK_TTL_SECONDS = 600;

type OperatorPassResetRefusal =
	| 'non_preview_purchases'
	| 'other_holders'
	| 'visionary_activations'
	| 'other_certificates'
	| 'billing_documents';

export interface OperatorPassResetPlan {
	allowed: boolean;
	refused_reason: OperatorPassResetRefusal | null;
	preview_purchases: number;
	non_preview_purchases: number;
	passes: number;
	certificate_numbers: number;
	users: number;
}

interface CollectedResetData {
	plan: OperatorPassResetPlan;
	purchases: Array<OperatorPassPurchaseRow>;
	units: Array<OperatorPassUnitRow>;
	passIds: Set<string>;
	users: Map<string, UserID>;
	numbers: Array<{number: number; pass_id: string}>;
}

type ResetRepository = Pick<
	OperatorPassRepository,
	| 'scanPurchases'
	| 'scanOperatorPasses'
	| 'scanVisionaryActivations'
	| 'scanOffers'
	| 'scanCertificateNumberOwners'
	| 'listUnitsByPurchase'
>;

async function scanAll<Row>(
	scan: (pageSize: number, pageState: string | null) => Promise<OperatorPassScanPage<Row>>,
): Promise<Array<Row>> {
	const rows: Array<Row> = [];
	let pageState: string | null = null;
	do {
		const page = await scan(SCAN_PAGE_SIZE, pageState);
		rows.push(...page.rows);
		pageState = page.pageState;
	} while (pageState);
	return rows;
}

function resolveRefusal(params: {
	nonPreviewPurchases: number;
	otherHolders: number;
	visionaryActivations: number;
	otherCertificates: number;
	billingDocuments: number;
}): OperatorPassResetRefusal | null {
	if (params.nonPreviewPurchases > 0) return 'non_preview_purchases';
	if (params.otherHolders > 0) return 'other_holders';
	if (params.visionaryActivations > 0) return 'visionary_activations';
	if (params.otherCertificates > 0) return 'other_certificates';
	if (params.billingDocuments > 0) return 'billing_documents';
	return null;
}

async function collectResetData(repository: ResetRepository): Promise<CollectedResetData> {
	const allPurchases = await scanAll((size, state) => repository.scanPurchases(size, state));
	const purchases = allPurchases.filter((purchase) => purchase.preview_label != null);
	const units: Array<OperatorPassUnitRow> = [];
	for (const purchase of purchases) {
		units.push(...(await repository.listUnitsByPurchase(purchase.checkout_session_id)));
	}
	const passIds = new Set(units.map((unit) => unit.pass_id));
	const users = new Map<string, UserID>();
	for (const purchase of purchases) users.set(purchase.purchaser_id.toString(), purchase.purchaser_id);
	for (const unit of units) {
		users.set(unit.holder_id.toString(), unit.holder_id);
		if (unit.buyer_id != null) users.set(unit.buyer_id.toString(), unit.buyer_id);
	}
	const passes = await scanAll((size, state) => repository.scanOperatorPasses(size, state));
	const activations = await scanAll((size, state) => repository.scanVisionaryActivations(size, state));
	const numbers = await repository.scanCertificateNumberOwners();
	const refusal = resolveRefusal({
		nonPreviewPurchases: allPurchases.length - purchases.length,
		otherHolders: passes.filter((pass) => !users.has(pass.user_id.toString())).length,
		visionaryActivations: activations.filter((activation) => activation.activated_at != null).length,
		otherCertificates: numbers.filter((owner) => !passIds.has(owner.pass_id)).length,
		billingDocuments: purchases.filter(
			(purchase) => purchase.invoice_document_id != null || purchase.invoice_number != null,
		).length,
	});
	return {
		plan: {
			allowed: refusal === null,
			refused_reason: refusal,
			preview_purchases: purchases.length,
			non_preview_purchases: allPurchases.length - purchases.length,
			passes: units.length,
			certificate_numbers: numbers.filter((owner) => passIds.has(owner.pass_id)).length,
			users: users.size,
		},
		purchases,
		units,
		passIds,
		users,
		numbers,
	};
}

export async function planOperatorPassReset(repository: ResetRepository): Promise<OperatorPassResetPlan> {
	return (await collectResetData(repository)).plan;
}

function resetRefusedError(reason: OperatorPassResetRefusal): ConflictError {
	return new ConflictError({
		code: APIErrorCodes.CONFLICT,
		message: `The Operator Pass data can not be reset: ${reason}`,
	});
}

interface OperatorPassResetDependencies {
	fulfilment: OperatorPassFulfilmentService;
	cacheService: ICacheService;
	kvClient: IKVProvider;
}

export async function resetOperatorPassPreviewData({
	fulfilment,
	cacheService,
	kvClient,
}: OperatorPassResetDependencies): Promise<{removed: OperatorPassResetPlan; remaining: OperatorPassResetPlan}> {
	const token = await cacheService.acquireLock(RESET_LOCK_KEY, RESET_LOCK_TTL_SECONDS);
	if (!token) {
		throw new ConflictError({code: APIErrorCodes.CONFLICT, message: 'A reset is already running'});
	}
	try {
		const {repository, holdingService, wallService} = fulfilment;
		const collected = await collectResetData(repository);
		if (collected.plan.refused_reason) {
			throw resetRefusedError(collected.plan.refused_reason);
		}
		for (const purchase of collected.purchases) {
			await fulfilment.cancelPreviewPurchase(purchase.checkout_session_id);
		}
		for (const offer of await scanAll((size, state) => repository.scanOffers(size, state))) {
			if (collected.passIds.has(offer.pass_id)) {
				await repository.deleteOffer(offer);
			}
		}
		for (const unit of collected.units) {
			await repository.deleteCertificateRecords(unit.pass_id);
			await forgetOperatorPassCertificateNumberClaim(cacheService, unit.pass_id);
			const current = await repository.findUnit(unit.pass_id);
			if (current) {
				await repository.deleteUnitRecords(current);
			}
		}
		for (const owner of collected.numbers) {
			if (collected.passIds.has(owner.pass_id)) {
				await repository.deleteCertificateNumberClaim(owner.number, owner.pass_id);
			}
		}
		const earliestPreview = Math.min(...collected.purchases.map((purchase) => purchase.created_at.getTime()));
		for (const purchase of collected.purchases) {
			await repository.deleteCertificateIndexForPurchase(purchase.checkout_session_id);
			await repository.deletePurchase(purchase);
			for (const key of operatorPassPurchaseCacheKeys(purchase.checkout_session_id)) {
				await cacheService.delete(key);
			}
		}
		for (const userId of collected.users.values()) {
			await holdingService.syncHolding(userId);
			await wallService.forgetForDeletion(userId);
			await repository.deleteWallPartitions(userId);
			await repository.setWallPending(userId, false);
			const member = await repository.findLabsMember(userId);
			if (member?.added_at && member.added_at.getTime() >= earliestPreview) {
				await repository.deleteLabsMember(userId);
			} else {
				await repository.removeLabsGreeting(userId);
			}
		}
		if ((await repository.scanCertificateNumberOwners()).length === 0) {
			await resetOperatorPassCertificateNumberCounter(kvClient);
		}
		await wallService.publication.republish();
		const remaining = await planOperatorPassReset(repository);
		Logger.warn({removed: collected.plan, remaining}, 'Reset the Operator Pass preview data');
		return {removed: collected.plan, remaining};
	} finally {
		await cacheService.releaseLock(RESET_LOCK_KEY, token);
	}
}
