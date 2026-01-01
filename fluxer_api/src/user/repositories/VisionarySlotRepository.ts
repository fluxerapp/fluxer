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

import type {UserID} from '~/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {VisionarySlotRow} from '~/database/CassandraTypes';
import {CannotShrinkReservedSlotsError} from '~/Errors';
import {VisionarySlot} from '~/Models';
import {VisionarySlots} from '~/Tables';

const FETCH_ALL_VISIONARY_SLOTS_QUERY = VisionarySlots.selectCql();

const FETCH_VISIONARY_SLOT_QUERY = VisionarySlots.selectCql({
	where: VisionarySlots.where.eq('slot_index'),
	limit: 1,
});

export class VisionarySlotRepository {
	async listVisionarySlots(): Promise<Array<VisionarySlot>> {
		const slots = await fetchMany<VisionarySlotRow>(FETCH_ALL_VISIONARY_SLOTS_QUERY, {});
		return slots.map((slot) => new VisionarySlot(slot));
	}

	async expandVisionarySlots(byCount: number): Promise<void> {
		const existingSlots = await this.listVisionarySlots();
		const maxSlotIndex = existingSlots.length > 0 ? Math.max(...existingSlots.map((s) => s.slotIndex)) : -1;

		const batch = new BatchBuilder();
		for (let i = 1; i <= byCount; i++) {
			const newSlotIndex = maxSlotIndex + i;
			batch.addPrepared(
				VisionarySlots.upsertAll({
					slot_index: newSlotIndex,
					user_id: null,
				}),
			);
		}
		await batch.execute();
	}

	async shrinkVisionarySlots(toCount: number): Promise<void> {
		const existingSlots = await this.listVisionarySlots();
		if (existingSlots.length <= toCount) return;

		const sortedSlots = existingSlots.sort((a, b) => b.slotIndex - a.slotIndex);
		const slotsToRemove = sortedSlots.slice(0, existingSlots.length - toCount);

		const reservedSlots = slotsToRemove.filter((slot) => slot.userId !== null);
		if (reservedSlots.length > 0) {
			throw new CannotShrinkReservedSlotsError(reservedSlots.map((s) => s.slotIndex));
		}

		const batch = new BatchBuilder();
		for (const slot of slotsToRemove) {
			batch.addPrepared(VisionarySlots.deleteByPk({slot_index: slot.slotIndex}));
		}
		await batch.execute();
	}

	async reserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void> {
		const existingSlot = await fetchOne<VisionarySlotRow>(FETCH_VISIONARY_SLOT_QUERY, {
			slot_index: slotIndex,
		});

		if (!existingSlot) {
			await upsertOne(
				VisionarySlots.upsertAll({
					slot_index: slotIndex,
					user_id: userId,
				}),
			);
		} else {
			await upsertOne(
				VisionarySlots.upsertAll({
					slot_index: slotIndex,
					user_id: userId,
				}),
			);
		}
	}

	async unreserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void> {
		const existingSlot = await fetchOne<VisionarySlotRow>(FETCH_VISIONARY_SLOT_QUERY, {
			slot_index: slotIndex,
		});

		if (!existingSlot || existingSlot.user_id !== userId) {
			return;
		}

		await upsertOne(
			VisionarySlots.upsertAll({
				slot_index: slotIndex,
				user_id: null,
			}),
		);
	}
}
