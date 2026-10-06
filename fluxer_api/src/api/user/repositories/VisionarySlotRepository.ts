// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {BatchBuilder, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {VisionarySlotRow} from '@app/api/database/types/PaymentTypes';
import {VisionarySlot} from '@app/api/models/VisionarySlot';
import {VisionarySlots} from '@app/api/Tables';
import {CannotShrinkReservedSlotsError} from '@fluxer/errors/src/domains/core/CannotShrinkReservedSlotsError';

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

	async getVisionarySlot(slotIndex: number): Promise<VisionarySlot | null> {
		const slot = await fetchOne<VisionarySlotRow>(FETCH_VISIONARY_SLOT_QUERY, {
			slot_index: slotIndex,
		});
		return slot ? new VisionarySlot(slot) : null;
	}

	async expandVisionarySlots(byCount: number): Promise<void> {
		const existingSlots = await this.listVisionarySlots();
		const maxSlotIndex = existingSlots.length > 0 ? Math.max(...existingSlots.map((s) => s.slotIndex)) : 0;
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

	async swapVisionarySlotReservations(
		slotIndexA: number,
		slotIndexB: number,
	): Promise<{
		userIdA: UserID | null;
		userIdB: UserID | null;
	}> {
		const [slotA, slotB] = await Promise.all([
			fetchOne<VisionarySlotRow>(FETCH_VISIONARY_SLOT_QUERY, {slot_index: slotIndexA}),
			fetchOne<VisionarySlotRow>(FETCH_VISIONARY_SLOT_QUERY, {slot_index: slotIndexB}),
		]);
		const userIdA = slotA?.user_id ?? null;
		const userIdB = slotB?.user_id ?? null;
		const batch = new BatchBuilder();
		batch.addPrepared(
			VisionarySlots.upsertAll({
				slot_index: slotIndexA,
				user_id: userIdB,
			}),
		);
		batch.addPrepared(
			VisionarySlots.upsertAll({
				slot_index: slotIndexB,
				user_id: userIdA,
			}),
		);
		await batch.execute();
		return {userIdA, userIdB};
	}

	async unreserveVisionarySlot(slotIndex: number, userId: UserID): Promise<void> {
		const existingSlot = await fetchOne<VisionarySlotRow>(FETCH_VISIONARY_SLOT_QUERY, {
			slot_index: slotIndex,
		});
		if (!existingSlot) {
			return;
		}
		const sentinelUserId = createUserID(BigInt(-1));
		if (userId !== sentinelUserId && existingSlot.user_id !== userId) {
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
