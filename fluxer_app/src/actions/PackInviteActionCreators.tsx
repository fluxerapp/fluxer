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

import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {PackInviteMetadata} from '~/types/PackTypes';

const logger = new Logger('PackInvites');

export interface CreatePackInviteParams {
	packId: string;
	maxUses?: number;
	maxAge?: number;
	unique?: boolean;
}

export const createInvite = async (params: CreatePackInviteParams): Promise<PackInviteMetadata> => {
	try {
		logger.debug(`Creating invite for pack ${params.packId}`);
		const response = await http.post<PackInviteMetadata>({
			url: Endpoints.PACK_INVITES(params.packId),
			body: {
				max_uses: params.maxUses ?? 0,
				max_age: params.maxAge ?? 0,
				unique: params.unique ?? false,
			},
		});
		return response.body;
	} catch (error) {
		logger.error(`Failed to create invite for pack ${params.packId}:`, error);
		throw error;
	}
};
