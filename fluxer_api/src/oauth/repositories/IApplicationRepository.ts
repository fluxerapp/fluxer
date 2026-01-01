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

import type {ApplicationID, UserID} from '~/BrandedTypes';
import type {ApplicationRow} from '~/database/types/OAuth2Types';
import type {Application} from '~/models/Application';

export interface IApplicationRepository {
	getApplication(applicationId: ApplicationID): Promise<Application | null>;
	listApplicationsByOwner(ownerUserId: UserID): Promise<Array<Application>>;
	upsertApplication(data: ApplicationRow, oldData?: ApplicationRow | null): Promise<Application>;
	deleteApplication(applicationId: ApplicationID): Promise<void>;
}
