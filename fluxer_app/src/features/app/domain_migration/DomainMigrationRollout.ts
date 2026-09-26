// SPDX-License-Identifier: AGPL-3.0-or-later

import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {readDomainMigrationAssignment} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';

const logger = new Logger('DomainMigrationRollout');

class DomainMigrationRolloutSelector {
	get enabled(): boolean {
		try {
			return readDomainMigrationAssignment(ExperimentAssignments.response).enabled;
		} catch (err) {
			logger.warn('Failed to resolve domain migration assignment:', err);
			return false;
		}
	}
}

export const DomainMigrationRollout = new DomainMigrationRolloutSelector();

export default DomainMigrationRollout;
