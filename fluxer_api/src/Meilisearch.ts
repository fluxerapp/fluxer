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

import {MeiliSearch} from 'meilisearch';
import {Config} from './Config';
import {Logger} from './Logger';
import {AuditLogSearchService} from './search/AuditLogSearchService';
import {GuildSearchService} from './search/GuildSearchService';
import {MessageSearchService} from './search/MessageSearchService';
import {ReportSearchService} from './search/ReportSearchService';
import {UserSearchService} from './search/UserSearchService';

let meilisearchClient: MeiliSearch | null = null;
let messageSearchService: MessageSearchService | null = null;
let guildSearchService: GuildSearchService | null = null;
let userSearchService: UserSearchService | null = null;
let reportSearchService: ReportSearchService | null = null;
let auditLogSearchService: AuditLogSearchService | null = null;

function getMeilisearchClient(): MeiliSearch | null {
	if (!Config.search.enabled) {
		return null;
	}

	if (!meilisearchClient) {
		if (!Config.search.url || !Config.search.apiKey) {
			throw new Error('Meilisearch URL and API key are required when search is enabled');
		}

		meilisearchClient = new MeiliSearch({
			host: Config.search.url,
			apiKey: Config.search.apiKey,
		});
	}

	return meilisearchClient;
}

export function getMessageSearchService(): MessageSearchService | null {
	if (!Config.search.enabled) {
		return null;
	}

	const client = getMeilisearchClient();
	if (!client) {
		return null;
	}

	if (!messageSearchService) {
		messageSearchService = new MessageSearchService(client);
	}

	return messageSearchService;
}

export function getGuildSearchService(): GuildSearchService | null {
	if (!Config.search.enabled) {
		return null;
	}

	const client = getMeilisearchClient();
	if (!client) {
		return null;
	}

	if (!guildSearchService) {
		guildSearchService = new GuildSearchService(client);
	}

	return guildSearchService;
}

export function getUserSearchService(): UserSearchService | null {
	if (!Config.search.enabled) {
		return null;
	}

	const client = getMeilisearchClient();
	if (!client) {
		return null;
	}

	if (!userSearchService) {
		userSearchService = new UserSearchService(client);
	}

	return userSearchService;
}

export function getReportSearchService(): ReportSearchService | null {
	if (!Config.search.enabled) {
		return null;
	}

	const client = getMeilisearchClient();
	if (!client) {
		return null;
	}

	if (!reportSearchService) {
		reportSearchService = new ReportSearchService(client);
	}

	return reportSearchService;
}

export function getAuditLogSearchService(): AuditLogSearchService | null {
	if (!Config.search.enabled) {
		return null;
	}

	const client = getMeilisearchClient();
	if (!client) {
		return null;
	}

	if (!auditLogSearchService) {
		auditLogSearchService = new AuditLogSearchService(client);
	}

	return auditLogSearchService;
}

export async function initializeMeilisearch(): Promise<void> {
	if (!Config.search.enabled) {
		Logger.info('Search is disabled, skipping Meilisearch initialization');
		return;
	}

	const messageSearch = getMessageSearchService();
	const guildSearch = getGuildSearchService();
	const userSearch = getUserSearchService();
	const reportSearch = getReportSearchService();
	const auditLogSearch = getAuditLogSearchService();

	await Promise.all([
		messageSearch?.initialize(),
		guildSearch?.initialize(),
		userSearch?.initialize(),
		reportSearch?.initialize(),
		auditLogSearch?.initialize(),
	]);

	Logger.info('Meilisearch initialized successfully');
}
