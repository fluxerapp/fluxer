// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IAuditLogSearchService} from '@app/api/search/IAuditLogSearchService';
import type {IGuildMemberSearchService} from '@app/api/search/IGuildMemberSearchService';
import type {IGuildSearchService} from '@app/api/search/IGuildSearchService';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import type {IReportSearchService} from '@app/api/search/IReportSearchService';
import type {ISearchProvider} from '@app/api/search/ISearchProvider';
import type {IThreadSearchService} from '@app/api/search/IThreadSearchService';
import type {IUserSearchService} from '@app/api/search/IUserSearchService';

interface SearchServiceLifecycle {
	initialize(): Promise<void>;
	shutdown(): Promise<void>;
}

export interface SearchServices {
	message: IMessageSearchService & SearchServiceLifecycle;
	guild: IGuildSearchService & SearchServiceLifecycle;
	user: IUserSearchService & SearchServiceLifecycle;
	report: IReportSearchService & SearchServiceLifecycle;
	auditLog: IAuditLogSearchService & SearchServiceLifecycle;
	guildMember: IGuildMemberSearchService & SearchServiceLifecycle;
	thread: IThreadSearchService & Pick<SearchServiceLifecycle, 'shutdown'>;
}

export class SearchProvider implements ISearchProvider {
	private services: SearchServices | null = null;

	constructor(
		private readonly init: (() => Promise<SearchServices>) | null,
		private readonly onInitialized?: () => void,
	) {}

	async initialize(): Promise<void> {
		if (!this.init) return;
		const services = await this.init();
		this.services = services;
		await Promise.all([
			services.message.initialize(),
			services.guild.initialize(),
			services.user.initialize(),
			services.report.initialize(),
			services.auditLog.initialize(),
			services.guildMember.initialize(),
		]);
		this.onInitialized?.();
	}

	async shutdown(): Promise<void> {
		if (!this.services) return;
		await Promise.all(Object.values(this.services).map((service) => service.shutdown()));
		this.services = null;
	}

	getMessageSearchService(): IMessageSearchService | null {
		return this.services?.message ?? null;
	}

	getGuildSearchService(): IGuildSearchService | null {
		return this.services?.guild ?? null;
	}

	getUserSearchService(): IUserSearchService | null {
		return this.services?.user ?? null;
	}

	getReportSearchService(): IReportSearchService | null {
		return this.services?.report ?? null;
	}

	getAuditLogSearchService(): IAuditLogSearchService | null {
		return this.services?.auditLog ?? null;
	}

	getGuildMemberSearchService(): IGuildMemberSearchService | null {
		return this.services?.guildMember ?? null;
	}

	getThreadSearchService(): IThreadSearchService | null {
		return this.services?.thread ?? null;
	}
}
