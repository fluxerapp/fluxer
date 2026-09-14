// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {VisionarySlotRow} from '@app/api/database/types/PaymentTypes';

export class VisionarySlot {
	readonly slotIndex: number;
	readonly userId: UserID | null;

	constructor(row: VisionarySlotRow) {
		this.slotIndex = row.slot_index;
		this.userId = row.user_id;
	}

	toRow(): VisionarySlotRow {
		return {
			slot_index: this.slotIndex,
			user_id: this.userId,
		};
	}

	isReserved(): boolean {
		return this.userId !== null;
	}
}
