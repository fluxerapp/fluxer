// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {getVisionarySlotRepository} from '@app/api/middleware/ServiceSingletons';

const DEFAULT_SLOT_COUNT = 100;

export class VisionarySlotInitializer {
	async initialize(): Promise<void> {
		if (!Config.dev.testModeEnabled || !Config.stripe.enabled) {
			return;
		}
		try {
			const repository = getVisionarySlotRepository();
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
			throw error;
		}
	}
}
