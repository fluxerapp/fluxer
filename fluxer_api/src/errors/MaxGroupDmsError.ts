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

import {APIErrorCodes, MAX_GROUP_DMS_PER_USER} from '~/Constants';
import {BadRequestError} from './BadRequestError';

export class MaxGroupDmsError extends BadRequestError {
	constructor() {
		super({
			code: APIErrorCodes.MAX_GROUP_DMS,
			message: `You can be a member of at most ${MAX_GROUP_DMS_PER_USER} group DMs.`,
			data: {
				max_group_dms: MAX_GROUP_DMS_PER_USER,
			},
		});
	}
}
