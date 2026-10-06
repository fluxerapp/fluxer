// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, ReportID} from '@app/api/BrandedTypes';
import type {IGuildDataRepository} from '@app/api/guild/repositories/IGuildDataRepository';
import type {ILogger} from '@app/api/ILogger';
import type {IReportRepository} from '@app/api/report/IReportRepository';
import {
	getAuditLogSearchService,
	getGuildSearchService,
	getReportSearchService,
	getUserSearchService,
} from '@app/api/SearchFactory';
import type {IGuildSearchService} from '@app/api/search/IGuildSearchService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';
import type {IUserRepository} from '@app/api/user/IUserRepository';

const BATCH_SIZE = 100;

interface SearchWarmupDeps {
	userRepository: IUserRepository;
	guildRepository: IGuildDataRepository;
	reportRepository: IReportRepository;
	logger: ILogger;
}

export async function warmupAdminSearchIndexes(deps: SearchWarmupDeps): Promise<void> {
	const {userRepository, guildRepository, reportRepository, logger} = deps;
	const guildSearchService = getGuildSearchService();
	const userSearchService = getUserSearchService();
	const reportSearchService = getReportSearchService();
	const auditLogSearchService = getAuditLogSearchService();
	if (!guildSearchService && !userSearchService && !reportSearchService && !auditLogSearchService) {
		logger.info('No search services available, skipping warmup');
		return;
	}
	logger.info('Starting admin search index warmup');
	if (guildSearchService) {
		const testResult = await guildSearchService.searchGuilds('', {}, {limit: 1});
		if (testResult.total === 0) {
			logger.info('Guild index is empty, populating from database');
			await warmupGuilds(guildRepository, guildSearchService, logger);
		} else {
			logger.info({total: testResult.total}, 'Guild index already populated, skipping warmup');
		}
	}
	if (userSearchService) {
		const testResult = await userSearchService.searchUsers('', {}, {limit: 1});
		if (testResult.total === 0) {
			logger.info('User index is empty, populating from database');
			await warmupUsers(userRepository, userSearchService, logger);
		} else {
			logger.info({total: testResult.total}, 'User index already populated, skipping warmup');
		}
	}
	if (reportSearchService) {
		const testResult = await reportSearchService.search('', {}, {limit: 1});
		if (testResult.total === 0) {
			logger.info('Report index is empty, populating from database');
			await warmupReports(reportRepository, reportSearchService, logger);
		} else {
			logger.info({total: testResult.total}, 'Report index already populated, skipping warmup');
		}
	}
	logger.info('Admin search index warmup complete');
}

async function warmupGuilds(
	guildRepository: IGuildDataRepository,
	guildSearchService: IGuildSearchService,
	logger: ILogger,
): Promise<void> {
	let lastGuildId: GuildID | undefined;
	let hasMore = true;
	let totalIndexed = 0;
	while (hasMore) {
		const guilds = await guildRepository.listAllGuildsPaginated(BATCH_SIZE, lastGuildId);
		if (guilds.length > 0) {
			await guildSearchService.indexGuilds(guilds);
			totalIndexed += guilds.length;
			lastGuildId = guilds[guilds.length - 1]!.id;
			logger.debug({count: guilds.length, total: totalIndexed}, 'Indexed guild batch');
		}
		hasMore = guilds.length === BATCH_SIZE;
	}
	logger.info({total: totalIndexed}, 'Guild warmup complete');
}

async function warmupUsers(
	userRepository: IUserRepository,
	userSearchService: IUserSearchService,
	logger: ILogger,
): Promise<void> {
	let pageState: string | null = null;
	let totalIndexed = 0;
	do {
		const page = await userRepository.scanAllUsersPage(BATCH_SIZE, pageState);
		pageState = page.pageState;
		const users = page.users;
		if (users.length === 0) continue;
		await userSearchService.indexUsers(users);
		totalIndexed += users.length;
		logger.debug({count: users.length, total: totalIndexed}, 'Indexed user batch');
	} while (pageState);
	logger.info({total: totalIndexed}, 'User warmup complete');
}

async function warmupReports(
	reportRepository: IReportRepository,
	reportSearchService: IReportSearchService,
	logger: ILogger,
): Promise<void> {
	let lastReportId: ReportID | undefined;
	let hasMore = true;
	let totalIndexed = 0;
	while (hasMore) {
		const reports = await reportRepository.listAllReportsPaginated(BATCH_SIZE, lastReportId);
		if (reports.length > 0) {
			await reportSearchService.indexReports(reports);
			totalIndexed += reports.length;
			lastReportId = reports[reports.length - 1]!.reportId;
			logger.debug({count: reports.length, total: totalIndexed}, 'Indexed report batch');
		}
		hasMore = reports.length === BATCH_SIZE;
	}
	logger.info({total: totalIndexed}, 'Report warmup complete');
}
