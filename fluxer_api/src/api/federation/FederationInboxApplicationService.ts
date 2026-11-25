// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createMessageID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {FederationConfig} from '@app/api/federation/FederationConfig';
import type {SignedFederationEvent} from '@app/api/federation/FederationEvent';
import type {IFederationInboxRepository} from '@app/api/federation/FederationInboxRepository';
import {federationAppliedTotal} from '@app/api/federation/FederationMetrics';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {generateSnowflake} from '@fluxer/snowflake/src/Snowflake';

export interface FederationApplyLogger {
	warn(obj: unknown, message?: string): void;
}

const NOOP_LOGGER: FederationApplyLogger = {warn() {}};

interface MessageCreatePayload {
	guild?: {origin?: unknown; id?: unknown};
	channel?: {origin?: unknown; id?: unknown};
	message?: {
		origin?: unknown;
		id?: unknown;
		author?: {handle?: unknown; display_name?: unknown};
		content?: unknown;
		created_at?: unknown;
	};
}

export class FederationInboxApplicationService {
	constructor(
		private readonly config: FederationConfig,
		private readonly repository: IFederationInboxRepository,
		private readonly channelRepository: ChannelRepository,
		private readonly gatewayService: IGatewayService,
		private readonly logger: FederationApplyLogger = NOOP_LOGGER,
	) {}

	async apply(event: SignedFederationEvent): Promise<void> {
		try {
			const existing = await this.repository.get(event.origin, event.event_id);
			if (!existing || existing.status !== 'received') {
				return;
			}
			if (event.type !== 'message.create') {
				await this.repository.markRejected(event.origin, event.event_id, 'unsupported_event_type');
				federationAppliedTotal.inc('result="rejected"');
				return;
			}
			const payload = event.payload as unknown as MessageCreatePayload;
			const guildId = typeof payload.guild?.id === 'string' ? payload.guild.id : null;
			const content = typeof payload.message?.content === 'string' ? payload.message.content.trim() : '';
			if (
				guildId === null ||
				!this.config.guildIds.has(guildId) ||
				content.length === 0 ||
				content.length > MAX_MESSAGE_LENGTH_PREMIUM
			) {
				await this.repository.markRejected(event.origin, event.event_id, 'invalid_payload');
				federationAppliedTotal.inc('result="rejected"');
				return;
			}
			if (this.config.testChannelId === null) {
				await this.repository.markApplied(event.origin, event.event_id, new Date());
				federationAppliedTotal.inc('result="applied"');
				return;
			}
			const channelId = createChannelID(BigInt(this.config.testChannelId));
			const channel = await this.channelRepository.channelData.findUnique(channelId);
			if (!channel || !channel.guildId) {
				await this.repository.markRejected(event.origin, event.event_id, 'test_channel_unavailable');
				federationAppliedTotal.inc('result="rejected"');
				return;
			}
			const displayName =
				typeof payload.message?.author?.display_name === 'string' ? payload.message.author.display_name : null;
			const handle = typeof payload.message?.author?.handle === 'string' ? payload.message.author.handle : null;
			const messageId = createMessageID(generateSnowflake());
			const timestamp =
				typeof payload.message?.created_at === 'string' ? payload.message.created_at : event.occurred_at;
			await this.gatewayService.dispatchGuild({
				guildId: channel.guildId,
				event: 'MESSAGE_CREATE',
				data: {
					id: messageId.toString(),
					channel_id: channelId.toString(),
					author: {
						id: SYSTEM_USER_ID.toString(),
						username: handle ?? `federation_${event.origin}`,
						discriminator: '0000',
						global_name: displayName,
						avatar: null,
						avatar_color: null,
						flags: 0,
						system: true,
					},
					type: MessageTypes.DEFAULT,
					flags: 0,
					content: `[${event.origin}] ${content}`,
					timestamp,
					edited_timestamp: null,
					pinned: false,
					mention_everyone: false,
					tts: false,
					mentions: [],
					mention_roles: [],
					embeds: null,
					attachments: null,
					stickers: null,
					reactions: null,
					message_reference: null,
					nonce: null,
				},
			});
			await this.repository.markApplied(event.origin, event.event_id, new Date());
			federationAppliedTotal.inc('result="applied"');
		} catch (error) {
			this.logger.warn(
				{err: error, eventId: event.event_id, origin: event.origin},
				'Failed to apply federation inbox event',
			);
			await this.repository.markRejected(event.origin, event.event_id, 'apply_failed');
			federationAppliedTotal.inc('result="failed"');
		}
	}
}