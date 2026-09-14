// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import type {BlockedMessageGroupsAssignmentResponse} from '@fluxer/schema/src/domains/experiment/BlockedMessageGroupsSchemas';
import {readBlockedMessageGroupsAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';

export const BLOCKED_MESSAGE_GROUPS_EXPERIMENT_CLASS = 'experiment-blocked-message-groups';

class BlockedMessageGroupsRolloutSelector {
	get assignment(): BlockedMessageGroupsAssignmentResponse {
		return readBlockedMessageGroupsAssignment(ExperimentAssignments.response);
	}

	get enabled(): boolean {
		const assignment = this.assignment;
		return assignment.enabled && assignment.user_targeted;
	}
}

export const BlockedMessageGroupsRollout = new BlockedMessageGroupsRolloutSelector();

export default BlockedMessageGroupsRollout;
