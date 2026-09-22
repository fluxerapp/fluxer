// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApplicationID, UserID} from '@app/api/BrandedTypes';
import type {ApplicationRow} from '@app/api/database/types/OAuth2Types';
import type {Application} from '@app/api/models/Application';

export interface IApplicationRepository {
	getApplication(applicationId: ApplicationID): Promise<Application | null>;
	listApplicationsByOwner(ownerUserId: UserID): Promise<Array<Application>>;
	upsertApplication(data: ApplicationRow, oldData?: ApplicationRow | null): Promise<Application>;
	deleteApplication(applicationId: ApplicationID): Promise<void>;
}
