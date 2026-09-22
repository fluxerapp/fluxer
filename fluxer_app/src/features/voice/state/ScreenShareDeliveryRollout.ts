// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {readScreenShareDeliveryAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';

const logger = new Logger('ScreenShareDeliveryRollout');

class ScreenShareDeliveryRolloutSelector {
	get enabled(): boolean {
		try {
			return readScreenShareDeliveryAssignment(ExperimentAssignments.response).enabled;
		} catch (err) {
			logger.warn('Failed to resolve screen share delivery assignment:', err);
			return false;
		}
	}
}

export const ScreenShareDeliveryRollout = new ScreenShareDeliveryRolloutSelector();

export default ScreenShareDeliveryRollout;
