// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildDiscoveryRepository} from '@app/api/guild/repositories/GuildDiscoveryRepository';
import type {SearchServices} from '@app/api/infrastructure/SearchProvider';
import {
	AuditLogSearchService,
	GuildMemberSearchService,
	GuildSearchService,
	MessageSearchService,
	ReportSearchService,
	UserSearchService,
} from '@app/api/search/SearchServices';
import {ThreadSearchIndex} from '@app/api/search/thread/ThreadSearchIndex';
import {ElasticsearchAuditLogAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchAuditLogAdapter';
import {ElasticsearchGuildAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchGuildAdapter';
import {ElasticsearchGuildMemberAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchGuildMemberAdapter';
import type {ElasticsearchDistributedLock} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchIndexAdapter';
import {ElasticsearchMessageAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchMessageAdapter';
import {ElasticsearchReportAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchReportAdapter';
import {ElasticsearchThreadAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchThreadAdapter';
import {ElasticsearchUserAdapter} from '@pkgs/elasticsearch_search/src/adapters/ElasticsearchUserAdapter';
import {
	createElasticsearchClient,
	type ElasticsearchClientConfig,
} from '@pkgs/elasticsearch_search/src/ElasticsearchClient';

export function createElasticsearchSearchServices(
	config: ElasticsearchClientConfig,
	lock?: ElasticsearchDistributedLock,
): SearchServices {
	const client = createElasticsearchClient(config);
	const message = new MessageSearchService(new ElasticsearchMessageAdapter({client, lock}));
	const guild = new GuildSearchService(new ElasticsearchGuildAdapter({client, lock}), new GuildDiscoveryRepository());
	const user = new UserSearchService(new ElasticsearchUserAdapter({client, lock}));
	const report = new ReportSearchService(new ElasticsearchReportAdapter({client, lock}));
	const auditLog = new AuditLogSearchService(new ElasticsearchAuditLogAdapter({client, lock}));
	const guildMember = new GuildMemberSearchService(new ElasticsearchGuildMemberAdapter({client, lock}));
	const thread = new ThreadSearchIndex(new ElasticsearchThreadAdapter({client, lock}));
	return {message, guild, user, report, auditLog, guildMember, thread};
}
