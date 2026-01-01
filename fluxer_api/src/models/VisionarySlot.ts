/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {VisionarySlotRow} from '~/database/CassandraTypes';
import type {UserID} from '../BrandedTypes';

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
