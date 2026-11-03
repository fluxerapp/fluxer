// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID, createMessageID, type MessageID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {MessageSendService} from '@app/api/channel/services/message/MessageSendService';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import type {OperatorPassLabsGreetingRow} from '@app/api/database/types/OperatorPassTypes';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {Channel} from '@app/api/models/Channel';
import {
	isOperatorPassEntitled,
	loadOperatorPassEntitlementState,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
	getLabsOperatorsChannelId,
	hidesOperatorsChannel,
	isBlockedFromLabsAutoJoin,
} from '@app/api/operator_pass/OperatorPassLabsRole';
import {type OperatorPassLockLease, withOperatorPassLock} from '@app/api/operator_pass/OperatorPassLocks';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {compareSnowflakes} from '@fluxer/snowflake/src/SnowflakeComparison';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const BATCH_SIZE = 50;

const MESSAGES_PER_RUN = 20;

const LOCK_KEY = 'operator_pass:labs_greeting_lock';

export class OperatorPassLabsGreetingBusyError extends Error {
	constructor() {
		super('Another worker is sending the #operators greetings');
	}
}

export interface OperatorPassLabsGreetingDeps {
	repository: OperatorPassRepository;
	userRepository: Pick<UserRepository, 'findUnique'>;
	guildRepository: Pick<GuildRepository, 'getMember'>;
	channelRepository: Pick<ChannelRepository, 'channelData' | 'messages'>;
	messages: Pick<MessageSendService, 'sendSystemGuildMessage'>;
	snowflakeService: Pick<ISnowflakeService, 'generateForChannel'>;
	cacheService: ICacheService;
}

export interface OperatorPassLabsGreetingSummary {
	greeted: number;
	messages: number;
	dropped: number;
}

function renderGreeting(userIds: ReadonlyArray<UserID>): string {
	const mentions = userIds.map((userId) => `<@${userId}>`);
	return getContentMessage('operator_pass.labs_greeting', null, {
		size: mentions.length === 1 ? 'single' : 'many',
		others: mentions.slice(0, -1).join(', '),
		last: mentions.at(-1) ?? '',
	});
}

function sortUserIds(userIds: ReadonlyArray<UserID>): Array<UserID> {
	return [...userIds].sort(compareSnowflakes);
}

export async function runOperatorPassLabsGreeting(
	deps: OperatorPassLabsGreetingDeps,
): Promise<OperatorPassLabsGreetingSummary | null> {
	const channelId = getLabsOperatorsChannelId();
	const guildId = Config.operatorPass.labsGuildId;
	if (channelId === null || guildId === null) {
		return null;
	}
	const channel = await deps.channelRepository.channelData.findUnique(createChannelID(channelId));
	if (!channel || channel.guildId !== createGuildID(guildId)) {
		throw new Error('FLUXER_LABS_OPERATORS_CHANNEL_ID is not a channel in Fluxer Labs');
	}
	return withOperatorPassLock(
		deps.cacheService,
		LOCK_KEY,
		(lease) => new LabsGreetingRun(deps, channel, lease).run(),
		() => new OperatorPassLabsGreetingBusyError(),
	);
}

class LabsGreetingRun {
	private readonly summary: OperatorPassLabsGreetingSummary = {greeted: 0, messages: 0, dropped: 0};

	constructor(
		private readonly deps: OperatorPassLabsGreetingDeps,
		private readonly channel: Channel,
		private readonly lease: OperatorPassLockLease,
	) {}

	async run(): Promise<OperatorPassLabsGreetingSummary> {
		const rows = await this.deps.repository.listLabsGreetings();
		const claimed = new Map<bigint, Array<UserID>>();
		const waiting: Array<OperatorPassLabsGreetingRow> = [];
		for (const row of rows) {
			if (row.message_id == null) {
				waiting.push(row);
				continue;
			}
			claimed.set(row.message_id, [...(claimed.get(row.message_id) ?? []), row.user_id]);
		}
		for (const [messageId, userIds] of claimed) {
			await this.send(createMessageID(messageId), userIds);
		}
		waiting.sort(
			(left, right) =>
				(left.queued_at?.getTime() ?? 0) - (right.queued_at?.getTime() ?? 0) ||
				compareSnowflakes(left.user_id, right.user_id),
		);
		let greetable: Array<UserID> = [];
		for (const row of waiting) {
			if (this.summary.messages >= MESSAGES_PER_RUN) {
				break;
			}
			if (await this.isGreetable(row.user_id)) {
				greetable.push(row.user_id);
			} else {
				await this.deps.repository.removeLabsGreeting(row.user_id);
				this.summary.dropped++;
			}
			if (greetable.length === BATCH_SIZE) {
				await this.claimAndSend(greetable);
				greetable = [];
			}
		}
		if (greetable.length > 0) {
			await this.claimAndSend(greetable);
		}
		return this.summary;
	}

	private async claimAndSend(userIds: ReadonlyArray<UserID>): Promise<void> {
		await this.lease.assertHeld();
		const messageId = createMessageID(await this.deps.snowflakeService.generateForChannel(this.channel.id));
		for (const userId of userIds) {
			await this.deps.repository.claimLabsGreeting(userId, messageId);
		}
		await this.send(messageId, userIds);
	}

	private async isGreetable(userId: UserID): Promise<boolean> {
		const record = await this.deps.repository.findLabsMember(userId);
		if (record?.greeting === false || record?.greeted_at != null) {
			return false;
		}
		const user = await this.deps.userRepository.findUnique(userId);
		if (!user || hidesOperatorsChannel(user) || isBlockedFromLabsAutoJoin(user, new Date())) {
			return false;
		}
		if (!this.channel.guildId || !(await this.deps.guildRepository.getMember(this.channel.guildId, userId))) {
			return false;
		}
		return isOperatorPassEntitled(await loadOperatorPassEntitlementState(this.deps.repository, userId));
	}

	private async send(messageId: MessageID, userIds: ReadonlyArray<UserID>): Promise<void> {
		await this.lease.assertHeld();
		const ordered = sortUserIds(userIds);
		if (!(await this.deps.channelRepository.messages.getMessage(this.channel.id, messageId))) {
			await this.deps.messages.sendSystemGuildMessage({
				channel: this.channel,
				messageId,
				content: renderGreeting(ordered),
				mentionUserIds: ordered,
			});
			this.summary.messages++;
		}
		const greetedAt = new Date();
		for (const userId of ordered) {
			await this.deps.repository.recordLabsGreeted(userId, greetedAt, messageId);
			await this.deps.repository.removeLabsGreeting(userId);
			this.summary.greeted++;
		}
	}
}
