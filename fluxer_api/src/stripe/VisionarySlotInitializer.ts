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

import {Config} from '~/Config';
import {Logger} from '~/Logger';
import {VisionarySlotRepository} from '~/user/repositories/VisionarySlotRepository';

const DEFAULT_SLOT_COUNT = 100;

export class VisionarySlotInitializer {
	async initialize(): Promise<void> {
		if (!Config.dev.testModeEnabled || !Config.stripe.enabled) {
			return;
		}

		try {
			const repository = new VisionarySlotRepository();
			const existingSlots = await repository.listVisionarySlots();

			if (existingSlots.length === 0) {
				Logger.info(`[VisionarySlotInitializer] Creating ${DEFAULT_SLOT_COUNT} test visionary slots...`);
				await repository.expandVisionarySlots(DEFAULT_SLOT_COUNT);
				Logger.info(`[VisionarySlotInitializer] Successfully created ${DEFAULT_SLOT_COUNT} visionary slots`);
			} else {
				Logger.info(`[VisionarySlotInitializer] Found ${existingSlots.length} existing slots, skipping initialization`);
			}
		} catch (error) {
			Logger.error({error}, '[VisionarySlotInitializer] Failed to create visionary slots');
		}
	}
}
