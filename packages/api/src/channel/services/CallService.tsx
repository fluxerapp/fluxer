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

import {type ChannelID, createMessageID, type MessageID, type UserID} from '@fluxer/api/src/BrandedTypes';
import {mapChannelToResponse} from '@fluxer/api/src/channel/ChannelMappers';
import type {IChannelRepository} from '@fluxer/api/src/channel/IChannelRepository';
import {mapMessageToResponse} from '@fluxer/api/src/channel/MessageMappers';
import {DMPermissionValidator} from '@fluxer/api/src/channel/services/DMPermissionValidator';
import {incrementDmMentionCounts} from '@fluxer/api/src/channel/services/message/ReadStateHelpers';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import type {CallData, IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {IMediaService} from '@fluxer/api/src/infrastructure/IMediaService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {ReadStateService} from '@fluxer/api/src/read_state/ReadStateService';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import type {VoiceAccessContext, VoiceAvailabilityService} from '@fluxer/api/src/voice/VoiceAvailabilityService';
import {resolveVoiceRegionPreference, selectVoiceRegionId} from '@fluxer/api/src/voice/VoiceRegionSelection';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {IncomingCallFlags, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {CallAlreadyExistsError} from '@fluxer/errors/src/domains/channel/CallAlreadyExistsError';
import {InvalidChannelTypeForCallError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeForCallError';
import {NoActiveCallError} from '@fluxer/errors/src/domains/channel/NoActiveCallError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import * as BucketUtils from '@fluxer/snowflake/src/SnowflakeBuckets';

const FALLBACK_VOICE_REGION = 'us-east';

export class CallService {
	private dmPermissionValidator: DMPermissionValidator;

	constructor(
		private channelRepository: IChannelRepository,
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private gatewayService: IGatewayService,
		private userCacheService: UserCacheService,
		private mediaService: IMediaService,
		private snowflakeService: SnowflakeService,
		private readStateService: ReadStateService,
		private voiceAvailabilityService?: VoiceAvailabilityService,
	) {
		this.dmPermissionValidator = new DMPermissionValidator({
			userRepository: this.userRepository,
			guildRepository: this.guildRepository,
		});
	}

	async checkCallEligibility({
		userId,
		channelId,
	}: {
		userId: UserID;
		channelId: ChannelID;
	}): Promise<{ringable: boolean; silent?: boolean}> {
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeForCallError();
		}

		const caller = await this.userRepository.findUnique(userId);
		const isUnclaimedCaller = caller?.isUnclaimedAccount() ?? false;
		if (isUnclaimedCaller && channel.type === ChannelTypes.DM) {
			return {ringable: false};
		}

		const call = await this.gatewayService.getCall(channelId);
		const alreadyInCall = call ? call.voice_states.some((vs) => vs.user_id === userId.toString()) : false;
		if (alreadyInCall) {
			return {ringable: false};
		}

		if (channel.type === ChannelTypes.DM) {
			const recipientId = Array.from(channel.recipientIds).find((id) => id !== userId);
			if (!recipientId) {
				return {ringable: true};
			}

			const recipientSettings = await this.userRepository.findSettings(recipientId);
			if (!recipientSettings) {
				return {ringable: true};
			}

			const incomingCallFlags = recipientSettings.incomingCallFlags;

			if ((incomingCallFlags & IncomingCallFlags.NOBODY) === IncomingCallFlags.NOBODY) {
				return {ringable: false};
			}

			const friendship = await this.userRepository.getRelationship(userId, recipientId, RelationshipTypes.FRIEND);
			const areFriends = friendship !== null;

			if ((incomingCallFlags & IncomingCallFlags.FRIENDS_ONLY) === IncomingCallFlags.FRIENDS_ONLY) {
				return {ringable: areFriends};
			}

			if (areFriends) {
				return {ringable: true};
			}

			const shouldBeSilent =
				(incomingCallFlags & IncomingCallFlags.SILENT_EVERYONE) === IncomingCallFlags.SILENT_EVERYONE;

			if ((incomingCallFlags & IncomingCallFlags.FRIENDS_OF_FRIENDS) === IncomingCallFlags.FRIENDS_OF_FRIENDS) {
				const callerRelationships = await this.userRepository.listRelationships(userId);
				const recipientRelationships = await this.userRepository.listRelationships(recipientId);

				const callerFriendIds = new Set(
					callerRelationships.filter((r) => r.type === RelationshipTypes.FRIEND).map((r) => r.targetUserId.toString()),
				);
				const hasMutualFriends = recipientRelationships
					.filter((r) => r.type === RelationshipTypes.FRIEND)
					.some((r) => callerFriendIds.has(r.targetUserId.toString()));

				if (hasMutualFriends) {
					return {ringable: true, silent: shouldBeSilent};
				}
			}

			if ((incomingCallFlags & IncomingCallFlags.GUILD_MEMBERS) === IncomingCallFlags.GUILD_MEMBERS) {
				const callerGuildIds = new Set(
					(await this.userRepository.getUserGuildIds(userId)).map((guildId) => guildId.toString()),
				);
				const recipientGuildIds = await this.userRepository.getUserGuildIds(recipientId);
				const hasMutualGuilds = recipientGuildIds.some((guildId) => callerGuildIds.has(guildId.toString()));

				if (hasMutualGuilds) {
					return {ringable: true, silent: shouldBeSilent};
				}
			}

			if ((incomingCallFlags & IncomingCallFlags.EVERYONE) === IncomingCallFlags.EVERYONE) {
				return {ringable: true, silent: shouldBeSilent};
			}

			return {ringable: false};
		}

		return {ringable: true};
	}

	async createOrGetCall({
		userId,
		channelId,
		region,
		ringing,
		requestCache,
		latitude,
		longitude,
	}: {
		userId: UserID;
		channelId: ChannelID;
		region?: string;
		ringing: Array<UserID>;
		requestCache: RequestCache;
		latitude?: string;
		longitude?: string;
	}): Promise<CallData> {
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeForCallError();
		}

		this.ensureRequesterIsGroupDmRecipient({
			channelType: channel.type,
			channelRecipientIds: channel.recipientIds,
			userId,
		});

		this.validateExplicitRingRecipients({
			channelRecipientIds: channel.recipientIds,
			userId,
			recipients: ringing,
		});

		const recipientIds = Array.from(channel.recipientIds);
		const channelRecipients = recipientIds.length > 0 ? await this.userRepository.listUsers(recipientIds) : [];

		if (channel.type === ChannelTypes.DM) {
			await this.dmPermissionValidator.validate({recipients: channelRecipients, userId});
		}

		const existingCall = await this.gatewayService.getCall(channelId);
		if (existingCall) {
			throw new CallAlreadyExistsError();
		}

		if (region) {
			this.ensureCallRegionAccessible(userId, region);
		}

		const selectedRegion = region || this.selectOptimalRegionForCall(userId, latitude, longitude);
		const allRecipients = Array.from(new Set([userId, ...Array.from(channel.recipientIds)]));
		const messageId = createMessageID(await this.snowflakeService.generate());

		await this.channelRepository.upsertMessage({
			channel_id: channelId,
			bucket: BucketUtils.makeBucket(messageId),
			message_id: messageId,
			author_id: userId,
			type: MessageTypes.CALL,
			webhook_id: null,
			webhook_name: null,
			webhook_avatar_hash: null,
			content: null,
			edited_timestamp: null,
			pinned_timestamp: null,
			flags: 0,
			mention_everyone: false,
			mention_users: new Set(),
			mention_roles: null,
			mention_channels: null,
			attachments: null,
			embeds: null,
			sticker_items: null,
			message_reference: null,
			message_snapshots: null,
			call: {
				participant_ids: new Set(allRecipients),
				ended_timestamp: null,
			},
			has_reaction: false,
			version: 1,
		});

		const call = await this.gatewayService.createCall(
			channelId,
			messageId.toString(),
			selectedRegion,
			ringing.map((id) => id.toString()),
			allRecipients.map((id) => id.toString()),
		);

		const author = await this.userRepository.findUnique(userId);
		await incrementDmMentionCounts({
			readStateService: this.readStateService,
			user: author,
			recipients: channelRecipients,
			channelId,
		});

		const message = await this.channelRepository.getMessage(channelId, messageId);
		if (message) {
			const messageResponse = await mapMessageToResponse({
				message,
				userCacheService: this.userCacheService,
				mediaService: this.mediaService,
				requestCache,
				getReferencedMessage: (channelId, messageId) => this.channelRepository.getMessage(channelId, messageId),
			});

			for (const recipientId of allRecipients) {
				await this.gatewayService.dispatchPresence({
					userId: recipientId,
					event: 'MESSAGE_CREATE',
					data: messageResponse,
				});
			}
		}

		return call;
	}

	async updateCall({
		userId,
		channelId,
		region,
	}: {
		userId: UserID;
		channelId: ChannelID;
		region?: string | null;
	}): Promise<void> {
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeForCallError();
		}

		const call = await this.gatewayService.getCall(channelId);
		if (!call) {
			throw new NoActiveCallError();
		}

		if (region !== undefined) {
			if (region !== null) {
				this.ensureCallRegionAccessible(userId, region);
			}
			await this.gatewayService.updateCallRegion(channelId, region);
		}
	}

	async ringCallRecipients({
		userId,
		channelId,
		recipients,
		requestCache,
		latitude,
		longitude,
	}: {
		userId: UserID;
		channelId: ChannelID;
		recipients?: Array<UserID>;
		requestCache: RequestCache;
		latitude?: string;
		longitude?: string;
	}): Promise<void> {
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeForCallError();
		}

		this.ensureRequesterIsGroupDmRecipient({
			channelType: channel.type,
			channelRecipientIds: channel.recipientIds,
			userId,
		});

		if (recipients !== undefined) {
			this.validateExplicitRingRecipients({
				channelRecipientIds: channel.recipientIds,
				userId,
				recipients,
			});
		}

		if (channel.type === ChannelTypes.DM) {
			const channelRecipients = await this.userRepository.listUsers(Array.from(channel.recipientIds));
			await this.dmPermissionValidator.validate({recipients: channelRecipients, userId});
		}

		const callerRequestedNoRing = recipients !== undefined && recipients.length === 0;
		const recipientsToNotify = (
			callerRequestedNoRing ? Array.from(channel.recipientIds) : recipients || Array.from(channel.recipientIds)
		).filter((id) => id !== userId);

		const recipientsToRing: Array<UserID> = [];

		if (channel.type === ChannelTypes.DM) {
			if (!callerRequestedNoRing) {
				const recipientId = Array.from(channel.recipientIds).find((id) => id !== userId);
				if (recipientId) {
					const recipientSettings = await this.userRepository.findSettings(recipientId);
					if (recipientSettings) {
						const incomingCallFlags = recipientSettings.incomingCallFlags;
						const friendship = await this.userRepository.getRelationship(userId, recipientId, RelationshipTypes.FRIEND);
						const areFriends = friendship !== null;

						const shouldBeSilent =
							!areFriends &&
							(incomingCallFlags & IncomingCallFlags.SILENT_EVERYONE) === IncomingCallFlags.SILENT_EVERYONE;

						if (!shouldBeSilent) {
							recipientsToRing.push(recipientId);
						}
					} else {
						recipientsToRing.push(recipientId);
					}
				}
			}
		} else {
			if (!callerRequestedNoRing) {
				for (const recipientId of recipientsToNotify) {
					const recipientSettings = await this.userRepository.findSettings(recipientId);
					if (recipientSettings) {
						const incomingCallFlags = recipientSettings.incomingCallFlags;
						const friendship = await this.userRepository.getRelationship(userId, recipientId, RelationshipTypes.FRIEND);
						const areFriends = friendship !== null;

						const shouldBeSilent =
							!areFriends &&
							(incomingCallFlags & IncomingCallFlags.SILENT_EVERYONE) === IncomingCallFlags.SILENT_EVERYONE;

						if (!shouldBeSilent) {
							recipientsToRing.push(recipientId);
						}
					} else {
						recipientsToRing.push(recipientId);
					}
				}
			}
		}

		if (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) {
			const isGroupDm = channel.type === ChannelTypes.GROUP_DM;

			const callerHasChannelOpen = await this.userRepository.isDmChannelOpen(userId, channelId);
			if (!callerHasChannelOpen) {
				await this.userRepository.openDmForUser(userId, channelId, isGroupDm);

				const channelResponse = await mapChannelToResponse({
					channel,
					currentUserId: userId,
					userCacheService: this.userCacheService,
					requestCache,
				});

				await this.gatewayService.dispatchPresence({
					userId,
					event: 'CHANNEL_CREATE',
					data: channelResponse,
				});
			}

			for (const recipientId of recipientsToNotify) {
				const isOpen = await this.userRepository.isDmChannelOpen(recipientId, channelId);
				if (!isOpen) {
					await this.userRepository.openDmForUser(recipientId, channelId, isGroupDm);

					const channelResponse = await mapChannelToResponse({
						channel,
						currentUserId: recipientId,
						userCacheService: this.userCacheService,
						requestCache,
					});

					await this.gatewayService.dispatchPresence({
						userId: recipientId,
						event: 'CHANNEL_CREATE',
						data: channelResponse,
					});
				}
			}
		}

		const existingCall = await this.gatewayService.getCall(channelId);
		if (!existingCall) {
			await this.createOrGetCall({
				userId,
				channelId,
				ringing: recipientsToRing,
				requestCache,
				latitude,
				longitude,
			});
		} else {
			await this.gatewayService.ringCallRecipients(
				channelId,
				recipientsToRing.map((id) => id.toString()),
			);
		}
	}

	async stopRingingCallRecipients({
		userId,
		channelId,
		recipients,
	}: {
		userId: UserID;
		channelId: ChannelID;
		recipients?: Array<UserID>;
	}): Promise<void> {
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) throw new UnknownChannelError();

		if (channel.type !== ChannelTypes.DM && channel.type !== ChannelTypes.GROUP_DM) {
			throw new InvalidChannelTypeForCallError();
		}

		const call = await this.gatewayService.getCall(channelId);
		if (!call) {
			throw new NoActiveCallError();
		}

		const toStop = recipients || [userId];
		await this.gatewayService.stopRingingCallRecipients(
			channelId,
			toStop.map((id) => id.toString()),
		);
	}

	private selectOptimalRegionForCall(userId: UserID, latitude?: string, longitude?: string): string {
		if (!this.voiceAvailabilityService) {
			return FALLBACK_VOICE_REGION;
		}

		const context: VoiceAccessContext = {
			requestingUserId: userId,
		};

		const availableRegions = this.voiceAvailabilityService.getAvailableRegions(context);
		const accessibleRegions = availableRegions.filter((r) => r.isAccessible);

		if (accessibleRegions.length === 0) {
			throw new Error('No accessible voice regions available');
		}

		const preference = resolveVoiceRegionPreference({
			preferredRegionId: null,
			accessibleRegions,
			availableRegions,
			defaultRegionId: null,
		});

		const selectedRegion = selectVoiceRegionId({
			preferredRegionId: preference.regionId,
			mode: preference.mode,
			accessibleRegions,
			availableRegions,
			latitude,
			longitude,
		});

		if (!selectedRegion) {
			throw new Error('No accessible voice regions available');
		}

		return selectedRegion;
	}

	private ensureCallRegionAccessible(userId: UserID, region: string): void {
		if (!this.voiceAvailabilityService) {
			return;
		}

		const context: VoiceAccessContext = {
			requestingUserId: userId,
		};

		const availableRegions = this.voiceAvailabilityService.getAvailableRegions(context);
		const isAllowed = availableRegions.some(
			(availableRegion) => availableRegion.id === region && availableRegion.isAccessible,
		);

		if (!isAllowed) {
			throw InputValidationError.fromCode('region', ValidationErrorCodes.INVALID_OR_RESTRICTED_RTC_REGION, {
				region,
			});
		}
	}

	private ensureRequesterIsGroupDmRecipient({
		channelType,
		channelRecipientIds,
		userId,
	}: {
		channelType: number;
		channelRecipientIds: Set<UserID>;
		userId: UserID;
	}): void {
		if (channelType !== ChannelTypes.GROUP_DM) {
			return;
		}

		if (!channelRecipientIds.has(userId)) {
			throw new UnknownChannelError();
		}
	}

	private validateExplicitRingRecipients({
		channelRecipientIds,
		userId,
		recipients,
	}: {
		channelRecipientIds: Set<UserID>;
		userId: UserID;
		recipients: Array<UserID>;
	}): void {
		const allowedRecipientIds = new Set(Array.from(channelRecipientIds).filter((id) => id !== userId));

		for (const recipientId of recipients) {
			if (!allowedRecipientIds.has(recipientId)) {
				throw InputValidationError.fromCode('recipients', ValidationErrorCodes.USER_NOT_IN_CHANNEL);
			}
		}
	}

	async updateCallMessageEnded({
		channelId,
		messageId,
		participants,
		endedTimestamp,
	}: {
		channelId: ChannelID;
		messageId: MessageID;
		participants: Array<UserID>;
		endedTimestamp: Date;
	}): Promise<void> {
		const message = await this.channelRepository.getMessage(channelId, messageId);
		if (!message) {
			return;
		}

		if (message.type !== MessageTypes.CALL) {
			return;
		}

		const messageRow = message.toRow();
		await this.channelRepository.upsertMessage({
			...messageRow,
			call: {
				participant_ids: new Set(participants),
				ended_timestamp: endedTimestamp,
			},
		});
	}
}
