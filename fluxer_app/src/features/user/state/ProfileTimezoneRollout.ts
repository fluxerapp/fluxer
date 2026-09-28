// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {readProfileTimezoneAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';

const logger = new Logger('ProfileTimezoneRollout');

class ProfileTimezoneRolloutSelector {
	get enabled(): boolean {
		try {
			return readProfileTimezoneAssignment(ExperimentAssignments.response).enabled;
		} catch (err) {
			logger.warn('Failed to resolve profile timezone assignment:', err);
			return false;
		}
	}
}

export const ProfileTimezoneRollout = new ProfileTimezoneRolloutSelector();

export default ProfileTimezoneRollout;
