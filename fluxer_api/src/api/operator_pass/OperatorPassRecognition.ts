// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {OperatorPassRow, OperatorPassUnitRow} from '@app/api/database/types/OperatorPassTypes';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';

export interface OperatorPassRecognition {
	count: number;
	since: Date;
}

export function calculateOperatorPassRecognition(
	userId: UserID,
	units: ReadonlyArray<OperatorPassUnitRow>,
): OperatorPassRecognition | null {
	const latest = new Map<string, OperatorPassUnitRow>();
	for (const unit of units) {
		const previous = latest.get(unit.pass_id);
		if (!previous || unit.version >= previous.version) latest.set(unit.pass_id, unit);
	}
	const credited = new Map<string, Date>();
	let holdsActivePass = false;
	for (const unit of latest.values()) {
		if (unit.status !== 'held' || unit.paused_at != null) continue;
		const holds = unit.holder_id === userId;
		const bought = unit.buyer_id === userId;
		if (!holds && !bought) continue;
		holdsActivePass ||= holds;
		credited.set(unit.pass_id, bought ? unit.first_delivered_at : unit.delivered_at);
	}
	if (!holdsActivePass || credited.size === 0) return null;
	return {
		count: credited.size,
		since: new Date(
			Array.from(credited.values()).reduce(
				(earliest, date) => Math.min(earliest, date.getTime()),
				Number.POSITIVE_INFINITY,
			),
		),
	};
}

export async function loadOperatorPassRecognition(
	repository: Pick<OperatorPassRepository, 'listUnitsByHolder' | 'listPurchasesByPurchaser' | 'listUnitsByPurchase'>,
	userId: UserID,
	pass: OperatorPassRow,
): Promise<OperatorPassRecognition | null> {
	const [held, purchases] = await Promise.all([
		repository.listUnitsByHolder(userId),
		repository.listPurchasesByPurchaser(userId),
	]);
	const units = [...held];
	for (let index = 0; index < purchases.length; index += 8) {
		const batches = await Promise.all(
			purchases.slice(index, index + 8).map((purchase) => repository.listUnitsByPurchase(purchase.checkout_session_id)),
		);
		for (const batch of batches) units.push(...batch);
	}
	if (units.length === 0) {
		return {count: Math.max(1, pass.pass_count ?? 1), since: pass.granted_at};
	}
	return calculateOperatorPassRecognition(userId, units);
}
