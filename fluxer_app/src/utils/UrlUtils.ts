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

import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import {buildMediaProxyURL} from '~/utils/MediaProxyUtils';

export function mediaUrl(path: string): string {
	return buildMediaProxyURL(`${RuntimeConfigStore.mediaEndpoint}/${path}`);
}

export function cdnUrl(path: string): string {
	return buildMediaProxyURL(`${RuntimeConfigStore.cdnEndpoint}/${path}`);
}

export function webhookUrl(webhookId: string, token: string): string {
	return `${RuntimeConfigStore.apiPublicEndpoint}/webhooks/${webhookId}/${token}`;
}

export function marketingUrl(path: string): string {
	return `${RuntimeConfigStore.marketingEndpoint}/${path}`;
}

export function adminUrl(path: string): string {
	return `${RuntimeConfigStore.adminEndpoint}/${path}`;
}
