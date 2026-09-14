// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {readMessageHoverTrackingAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import type {MessageHoverTrackingAssignmentResponse} from '@fluxer/schema/src/domains/experiment/MessageHoverTrackingSchemas';

export const MESSAGE_HOVER_TRACKING_EXPERIMENT_CLASS = 'experiment-message-hover-tracking';

class MessageHoverTrackingRolloutSelector {
	get assignment(): MessageHoverTrackingAssignmentResponse {
		return readMessageHoverTrackingAssignment(ExperimentAssignments.response);
	}

	get enabled(): boolean {
		const assignment = this.assignment;
		return assignment.enabled && assignment.user_targeted;
	}
}

export const MessageHoverTrackingRollout = new MessageHoverTrackingRolloutSelector();

export default MessageHoverTrackingRollout;
