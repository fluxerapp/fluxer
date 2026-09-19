// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {readScreenShareDeliveryAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import type {ScreenShareDeliveryAssignmentResponse} from '@fluxer/schema/src/domains/experiment/ScreenShareDeliverySchemas';

class ScreenShareDeliveryRolloutSelector {
	get assignment(): ScreenShareDeliveryAssignmentResponse {
		return readScreenShareDeliveryAssignment(ExperimentAssignments.response);
	}

	get enabled(): boolean {
		const assignment = this.assignment;
		return assignment.enabled && assignment.user_targeted;
	}
}

export const ScreenShareDeliveryRollout = new ScreenShareDeliveryRolloutSelector();

export default ScreenShareDeliveryRollout;
