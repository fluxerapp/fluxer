// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {FeatureTemporarilyDisabledModal} from '@app/features/app/components/alerts/FeatureTemporarilyDisabledModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import Authentication from '@app/features/auth/state/Authentication';
import Channels from '@app/features/channel/state/Channels';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import {DELETE_MESSAGE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {
	type MessageFetchCacheHit,
	resolveMessageFetchExecutionDecision,
	resolveMessageFetchPreflightDecision,
} from '@app/features/messaging/commands/MessageFetchStateMachine';
import {resolveMessagePageState} from '@app/features/messaging/commands/MessagePageStateMachine';
import {MessageDeleteFailedModal} from '@app/features/messaging/components/alerts/MessageDeleteFailedModal';
import {MessageDeleteTooQuickModal} from '@app/features/messaging/components/alerts/MessageDeleteTooQuickModal';
import type {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import type {JumpOptions} from '@app/features/messaging/state/ChannelMessages';
import MessageEdit from '@app/features/messaging/state/MessageEdit';
import MessageEditMobile from '@app/features/messaging/state/MessageEditMobile';
import MessageQueue from '@app/features/messaging/state/MessageQueue';
import MessageReferences from '@app/features/messaging/state/MessageReferences';
import MessageReply from '@app/features/messaging/state/MessageReply';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {
	collectMessageModelGuildMemberUserIds,
	collectWireMessageGuildMemberUserIds,
} from '@app/features/messaging/utils/MessageMemberLoadUtils';
import type {
	ApiAttachmentMetadata,
	ApiMessageEditAttachmentMetadata,
} from '@app/features/messaging/utils/MessageRequestUtils';
import * as IARCommands from '@app/features/moderation/commands/IARCommands';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Permission from '@app/features/permissions/state/Permission';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import * as SlowmodeCommands from '@app/features/slowmode/commands/SlowmodeCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MessageFlags, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {JumpType} from '@fluxer/constants/src/JumpConstants';
import {MAX_MESSAGES_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import type {MessageId} from '@fluxer/schema/src/branded/WireIds';
import type {
	AllowedMentions,
	MessageReference,
	MessageStickerItem,
	Message as WireMessage,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: "Delete this message? Can't be undone.",
	comment: 'Error message in the messaging commands. Keep the tone plain and specific.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'Button or menu action label in the messaging commands. Keep it concise. Keep the tone plain and specific.',
});
const ALSO_REPORT_TO_SAFETY_TEAM_DESCRIPTOR = msg({
	message: 'Also report this message to the {productName} Safety Team',
	comment:
		'Toggle-switch label in the moderator delete-message confirmation dialog. When enabled, the message is reported (category: other) before being deleted. {productName} is the product name (e.g., Fluxer).',
});
const logger = new Logger('MessageCommands');
const pendingDeletePromises = new Map<string, Promise<void>>();
const pendingFetchPromises = new Map<string, Promise<Array<WireMessage>>>();

export interface ForwardMediaSelection {
	attachmentIds?: ReadonlyArray<string>;
	embedIndices?: ReadonlyArray<number>;
}

interface ForwardMessageReference {
	message_id: string;
	channel_id: string;
	guild_id?: string | null;
	attachment_ids?: ReadonlyArray<string>;
	embed_indices?: ReadonlyArray<number>;
}

export interface JumpToMessageOptions {
	channelId: string;
	messageId: string;
	flash?: boolean;
	offset?: number;
	returnTargetId?: string | null;
	returnChannelId?: string | null;
	returnGuildId?: string | null;
	jumpType?: JumpType;
}

interface FetchMessagesOptions {
	throwOnError?: boolean;
}

interface MessagePageState {
	isBefore: boolean;
	isAfter: boolean;
	hasMoreBefore: boolean;
	hasMoreAfter: boolean;
}

function shouldBlockMessageFetch(channelId: string): boolean {
	const channel = Channels.getChannel(channelId);
	if (!channel || channel.isPrivate()) {
		return false;
	}
	return GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null});
}

function makeFetchKey(
	channelId: string,
	before: string | null,
	after: string | null,
	limit: number,
	jump?: JumpOptions,
	options?: FetchMessagesOptions,
): string {
	const SEP = '\x1f';
	const throwOnError = options?.throwOnError ? '1' : '0';
	if (!jump) {
		return `${channelId}${SEP}${before ?? ''}${SEP}${after ?? ''}${SEP}${limit}${SEP}${throwOnError}`;
	}
	return (
		`${channelId}${SEP}${before ?? ''}${SEP}${after ?? ''}${SEP}${limit}${SEP}${throwOnError}${SEP}` +
		`${jump.present ? '1' : '0'}${SEP}${jump.messageId ?? ''}${SEP}${jump.offset ?? 0}${SEP}` +
		`${jump.flash ? '1' : '0'}${SEP}${jump.returnMessageId ?? ''}${SEP}` +
		`${jump.returnChannelId ?? ''}${SEP}${jump.returnGuildId ?? ''}${SEP}${jump.jumpType ?? ''}`
	);
}

async function requestMissingGuildMembers(channelId: string, messages: Array<WireMessage>): Promise<void> {
	const channel = Channels.getChannel(channelId);
	if (!channel?.guildId) {
		return;
	}
	const guildId = channel.guildId;
	const userIds = collectWireMessageGuildMemberUserIds(messages, Authentication.currentUserId);
	if (userIds.length === 0) {
		return;
	}
	await GuildMembers.ensureMembersLoadedForMessages(guildId, userIds);
}

async function applyDeveloperFetchDelay(): Promise<void> {
	if (!DeveloperOptions.slowMessageLoad) {
		return;
	}
	logger.debug('Slow message load enabled, delaying by 3 seconds');
	await new Promise((resolve) => setTimeout(resolve, 3000));
}

function handleForcedMessageLoadFailure(channelId: string, jump?: JumpOptions): Array<WireMessage> {
	logger.debug('Force fail message loads enabled, simulating failure');
	Messages.handleLoadMessages({channelId, jump});
	Messages.handleLoadMessagesFailure({channelId});
	return [];
}

async function requestChannelMessages(
	channelId: string,
	before: string | null,
	after: string | null,
	limit: number,
	jump?: JumpOptions,
): Promise<Array<WireMessage>> {
	const around = jump?.messageId;
	const response = await http.get<Array<WireMessage>>(Endpoints.CHANNEL_MESSAGES(channelId), {
		query: {before, after, limit, around: around ?? null},
		retries: 2,
	});
	return response.body ?? [];
}

function calculateMessagePageState(
	channelId: string,
	before: string | null,
	after: string | null,
	limit: number,
	messages: Array<WireMessage>,
	jump?: JumpOptions,
): MessagePageState {
	const around = jump?.messageId;
	const targetIndex = around == null ? -1 : messages.findIndex((msg: WireMessage) => msg.id === around);
	const knownLatestMessageId =
		around == null
			? null
			: (ReadStates.lastMessageId(channelId) ?? Channels.getChannel(channelId)?.lastMessageId ?? null);
	const newestFetchedMessageId = messages[0]?.id ?? null;
	const pageState = resolveMessagePageState({
		before,
		after,
		limit,
		messageCount: messages.length,
		aroundMessageId: around ?? null,
		aroundTargetIndex: targetIndex,
		newestFetchedMessageId,
		knownLatestMessageId,
	});
	if (around) {
		if (pageState.shouldWarnMissingAroundTarget) {
			logger.warn(`Target message ${around} not found in response!`);
		} else if (pageState.aroundDebug != null) {
			const debug = pageState.aroundDebug;
			logger.debug(
				`Jump to message ${around}: targetIndex=${targetIndex}, messagesNewer=${debug.messagesNewerThanTarget}, messagesOlder=${debug.messagesOlderThanTarget}, expectedNewer=${debug.expectedNewer}, expectedOlder=${debug.expectedOlder}, pageFilled=${debug.pageFilled}, hasMoreBefore=${pageState.hasMoreBefore}, hasMoreAfter=${pageState.hasMoreAfter}, limit=${limit}, knownLatestMessageId=${knownLatestMessageId}, newestFetched=${newestFetchedMessageId}`,
			);
		}
	}
	return {
		isBefore: pageState.isBefore,
		isAfter: pageState.isAfter,
		hasMoreBefore: pageState.hasMoreBefore,
		hasMoreAfter: pageState.hasMoreAfter,
	};
}

function handleMessageFetchSuccess(
	channelId: string,
	messages: Array<WireMessage>,
	pageState: MessagePageState,
	jump?: JumpOptions,
): void {
	Messages.handleLoadMessagesSuccess({
		channelId,
		messages,
		isBefore: pageState.isBefore,
		isAfter: pageState.isAfter,
		hasMoreBefore: pageState.hasMoreBefore,
		hasMoreAfter: pageState.hasMoreAfter,
		cached: false,
		jump,
	});
	ReadStates.handleLoadMessages({
		channelId,
		isAfter: pageState.isAfter,
		messages,
	});
	MessageReferences.handleMessagesFetchSuccess(channelId, messages);
	void requestMissingGuildMembers(channelId, messages);
}

export async function ensureMembersForMessages(messages: ReadonlyArray<MessageModel>): Promise<void> {
	const currentUserId = Authentication.currentUserId;
	const byGuild = new Map<string, Set<string>>();
	for (const msg of messages) {
		if (!msg.guildId) continue;
		const userIds = collectMessageModelGuildMemberUserIds([msg], currentUserId);
		if (userIds.length === 0) continue;
		let set = byGuild.get(msg.guildId);
		if (!set) {
			set = new Set();
			byGuild.set(msg.guildId, set);
		}
		for (const userId of userIds) {
			set.add(userId);
		}
	}
	if (byGuild.size === 0) return;
	await Promise.all(
		Array.from(byGuild, ([guildId, ids]) => GuildMembers.ensureMembersLoadedForMessages(guildId, Array.from(ids))),
	);
}

interface SendMessageParams {
	content: string;
	nonce: string;
	hasAttachments?: boolean;
	allowedMentions?: AllowedMentions;
	messageReference?: MessageReference;
	flags?: number;
	favoriteMemeId?: string;
	stickers?: Array<MessageStickerItem>;
	tts?: boolean;
}

export function jumpToPresent(channelId: string, limit = MAX_MESSAGES_PER_CHANNEL): void {
	NavigationCommands.clearMessageIdForChannel(channelId);
	logger.debug(`Jumping to present in channel ${channelId}`);
	ReadStateCommands.clearStickyUnread(channelId);
	const jump: JumpOptions = {
		present: true,
	};
	if (Messages.hasPresent(channelId)) {
		Messages.handleLoadMessagesSuccessCached({channelId, jump, limit});
	} else {
		fetchMessages(channelId, null, null, limit, jump);
	}
}

export function jumpToMessage({
	channelId,
	messageId,
	flash = true,
	offset,
	returnTargetId,
	returnChannelId,
	returnGuildId,
	jumpType,
}: JumpToMessageOptions): void {
	logger.debug(`Jumping to message ${messageId} in channel ${channelId}`);
	fetchMessages(channelId, null, null, MAX_MESSAGES_PER_CHANNEL, {
		messageId: messageId as MessageId,
		flash,
		offset,
		returnMessageId: returnTargetId as MessageId | null | undefined,
		returnChannelId,
		returnGuildId,
		jumpType,
	});
}

function getMessageFetchCacheHit(
	channelId: string,
	before: string | null,
	after: string | null,
	jump?: JumpOptions,
): MessageFetchCacheHit | null {
	const messages = Messages.getMessages(channelId);
	if (jump?.messageId && messages.has(jump.messageId, false)) {
		return 'jump';
	}
	if (before && messages.hasBeforeCached(before)) {
		return 'before';
	}
	if (after && messages.hasAfterCached(after)) {
		return 'after';
	}
	return null;
}

function applyMessageFetchCacheHit(
	channelId: string,
	cacheHit: MessageFetchCacheHit,
	before: string | null,
	after: string | null,
	limit: number,
	jump?: JumpOptions,
): void {
	switch (cacheHit) {
		case 'jump':
			Messages.handleLoadMessagesSuccessCached({channelId, jump, limit});
			return;
		case 'before':
			Messages.handleLoadMessagesSuccessCached({channelId, before: before ?? undefined, limit});
			return;
		case 'after':
			Messages.handleLoadMessagesSuccessCached({channelId, after: after ?? undefined, limit});
			return;
	}
}

export async function fetchMessages(
	channelId: string,
	before: string | null,
	after: string | null,
	limit: number,
	jump?: JumpOptions,
	options?: FetchMessagesOptions,
): Promise<Array<WireMessage>> {
	const key = makeFetchKey(channelId, before, after, limit, jump, options);
	const inFlight = pendingFetchPromises.get(key);
	const preflightDecision = resolveMessageFetchPreflightDecision({
		hasInFlightRequest: inFlight != null,
		shouldBlockForGate: shouldBlockMessageFetch(channelId),
		cacheHit: getMessageFetchCacheHit(channelId, before, after, jump),
	});
	switch (preflightDecision.type) {
		case 'useInFlightRequest':
			logger.debug(`Using in-flight fetchMessages for channel ${channelId} (deduped)`);
			return inFlight as Promise<Array<WireMessage>>;
		case 'blockForGate':
			logger.debug(`Skipping message fetch for gated channel ${channelId}`);
			Messages.handleLoadMessagesBlocked({channelId});
			return [];
		case 'useCache':
			applyMessageFetchCacheHit(channelId, preflightDecision.cacheHit, before, after, limit, jump);
			return [];
		case 'startFetch':
			break;
	}
	const promise = (async () => {
		await applyDeveloperFetchDelay();
		const executionDecision = resolveMessageFetchExecutionDecision({
			forceFailure: DeveloperOptions.forceFailMessageLoads,
		});
		if (executionDecision.type === 'simulateFailure') {
			return handleForcedMessageLoadFailure(channelId, jump);
		}
		Messages.handleLoadMessages({channelId, jump});
		try {
			const timeStart = Date.now();
			logger.debug(`Fetching messages for channel ${channelId}`);
			const messages = await requestChannelMessages(channelId, before, after, limit, jump);
			const pageState = calculateMessagePageState(channelId, before, after, limit, messages, jump);
			logger.info(`Fetched ${messages.length} messages for channel ${channelId}, took ${Date.now() - timeStart}ms`);
			handleMessageFetchSuccess(channelId, messages, pageState, jump);
			return messages;
		} catch (error) {
			logger.error(`Failed to fetch messages for channel ${channelId}:`, error);
			Messages.handleLoadMessagesFailure({channelId});
			if (options?.throwOnError) {
				throw error;
			}
			return [];
		}
	})();
	pendingFetchPromises.set(key, promise);
	promise.finally(() => pendingFetchPromises.delete(key));
	return promise;
}

interface SequentialSendEntry {
	task: () => Promise<RestResponse<WireMessage> | undefined>;
	resolve: (value: RestResponse<WireMessage> | undefined) => void;
}

interface ChannelSendOrderState {
	nextOrder: number;
	nextExpected: number;
	pending: Map<number, SequentialSendEntry>;
	processing: boolean;
	channelId: string;
}

const channelSendOrders = new Map<string, ChannelSendOrderState>();

function getOrCreateChannelState(channelId: string): ChannelSendOrderState {
	let state = channelSendOrders.get(channelId);
	if (!state) {
		state = {nextOrder: 0, nextExpected: 0, pending: new Map(), processing: false, channelId};
		channelSendOrders.set(channelId, state);
	}
	return state;
}

function orderedSendImmediately(
	channelId: string,
	order: number,
	task: () => Promise<RestResponse<WireMessage> | undefined>,
): Promise<RestResponse<WireMessage> | undefined> {
	return new Promise<RestResponse<WireMessage> | undefined>((resolve) => {
		const state = getOrCreateChannelState(channelId);
		state.pending.set(order, {task, resolve});
		void processSequentialQueue(state);
	});
}

async function processSequentialQueue(state: ChannelSendOrderState): Promise<void> {
	if (state.processing) return;
	state.processing = true;
	try {
		while (state.pending.has(state.nextExpected)) {
			const entry = state.pending.get(state.nextExpected)!;
			state.pending.delete(state.nextExpected);
			state.nextExpected++;
			const result = await entry.task();
			entry.resolve(result);
		}
	} finally {
		state.processing = false;
		if (state.pending.size === 0 && state.nextExpected === state.nextOrder) {
			channelSendOrders.delete(state.channelId);
		}
	}
}

async function prepareSendAttachments(
	channelId: string,
	params: SendMessageParams,
): Promise<{attachments?: Array<ApiAttachmentMetadata>; files?: Array<File>} | null> {
	if (!params.hasAttachments) {
		return {};
	}
	logger.debug(`Preparing attachments for channel ${channelId}`);
	const prepared = await MessageQueue.prepareAttachmentsForSend({
		channelId,
		nonce: params.nonce,
		favoriteMemeId: params.favoriteMemeId,
	});
	if (!prepared) {
		return null;
	}
	return {attachments: prepared.attachments, files: prepared.files};
}

function nextChannelOrder(channelId: string): number {
	return getOrCreateChannelState(channelId).nextOrder++;
}

export async function send(channelId: string, params: SendMessageParams): Promise<WireMessage | null> {
	if (!MessageQueue.consumeLocalSendReservation(channelId, params.nonce)) {
		MessageQueue.rejectLocalRateLimitedSend(channelId, params.nonce, params.hasAttachments);
		return null;
	}
	const sendOrder = Accessibility.sequentialFileSend && params.hasAttachments ? nextChannelOrder(channelId) : -1;
	const prepared = await prepareSendAttachments(channelId, params);
	if (!prepared) {
		if (Accessibility.sequentialFileSend) {
			orderedSendImmediately(channelId, sendOrder, () => Promise.resolve(undefined));
		}
		return null;
	}
	const payload = {
		type: 'send' as const,
		channelId,
		nonce: params.nonce,
		content: params.content,
		hasAttachments: params.hasAttachments,
		preparedAttachments: prepared.attachments,
		preparedFiles: prepared.files,
		allowedMentions: params.allowedMentions,
		messageReference: params.messageReference,
		flags: params.flags,
		favoriteMemeId: params.favoriteMemeId,
		stickers: params.stickers,
		tts: params.tts,
	};
	if (params.hasAttachments) {
		logger.debug(`Sending attachment message immediately for channel ${channelId}`);
		const result = Accessibility.sequentialFileSend
			? await orderedSendImmediately(channelId, sendOrder, () => MessageQueue.sendImmediately(payload))
			: await MessageQueue.sendImmediately(payload);
		if (result?.body) {
			logger.debug(`Attachment message sent successfully in channel ${channelId}`);
			Messages.handleIncomingMessage({channelId, message: result.body});
			return result.body;
		}
		return null;
	}
	return new Promise<WireMessage | null>((resolve) => {
		logger.debug(`Enqueueing message for channel ${channelId}`);
		MessageQueue.enqueue(payload, (result, error) => {
			if (result?.body) {
				logger.debug(`Message sent successfully in channel ${channelId}`);
				Messages.handleIncomingMessage({channelId, message: result.body});
				resolve(result.body);
			} else {
				if (error) {
					logger.debug(`Message send failed in channel ${channelId}`, error);
				}
				resolve(null);
			}
		});
	});
}

export function reserveSend(channelId: string, nonce: string): boolean {
	return MessageQueue.reserveLocalSend(channelId, nonce);
}

function showDeleteFailureModal(error: unknown, messageId: string): void {
	if (error instanceof HttpError) {
		const {status} = error;
		const errorCode = failureCode(error);
		if (status === 429) {
			ModalCommands.push(
				modal(() => (
					<MessageDeleteTooQuickModal data-flx="messaging.message-commands.show-delete-failure-modal.message-delete-too-quick-modal" />
				)),
			);
		} else if (status === 403 && errorCode === APIErrorCodes.FEATURE_TEMPORARILY_DISABLED) {
			ModalCommands.push(
				modal(() => (
					<FeatureTemporarilyDisabledModal data-flx="messaging.message-commands.show-delete-failure-modal.feature-temporarily-disabled-modal" />
				)),
			);
		} else if (status === 404) {
			logger.debug(`Message ${messageId} was already deleted (404 response)`);
		} else {
			ModalCommands.push(
				modal(() => (
					<MessageDeleteFailedModal data-flx="messaging.message-commands.show-delete-failure-modal.message-delete-failed-modal" />
				)),
			);
		}
		return;
	}
	ModalCommands.push(
		modal(() => (
			<MessageDeleteFailedModal data-flx="messaging.message-commands.show-delete-failure-modal.message-delete-failed-modal--2" />
		)),
	);
}

export function edit(
	channelId: string,
	messageId: string,
	content?: string,
	flags?: number,
	allowedMentions?: AllowedMentions,
	attachments?: Array<ApiMessageEditAttachmentMetadata>,
): Promise<WireMessage | null> {
	return new Promise<WireMessage | null>((resolve) => {
		logger.debug(`Enqueueing edit for message ${messageId} in channel ${channelId}`);
		MessageQueue.enqueue(
			{
				type: 'edit',
				channelId,
				messageId,
				content,
				allowedMentions,
				flags,
				attachments,
			},
			(result, error) => {
				if (result?.body) {
					logger.debug(`Message edited successfully: ${messageId} in channel ${channelId}`);
					resolve(result.body);
				} else {
					if (error) {
						logger.debug(`Message edit failed: ${messageId} in channel ${channelId}`, error);
					}
					resolve(null);
				}
			},
		);
	});
}

export async function remove(channelId: string, messageId: string): Promise<void> {
	const pendingPromise = pendingDeletePromises.get(messageId);
	if (pendingPromise) {
		logger.debug(`Using in-flight delete request for message ${messageId}`);
		return pendingPromise;
	}
	const deletePromise = (async () => {
		try {
			logger.debug(`Deleting message ${messageId} in channel ${channelId}`);
			await http.delete(Endpoints.CHANNEL_MESSAGE(channelId, messageId));
			logger.debug(`Successfully deleted message ${messageId} in channel ${channelId}`);
		} catch (error) {
			logger.error(`Failed to delete message ${messageId} in channel ${channelId}:`, error);
			showDeleteFailureModal(error, messageId);
			throw error;
		} finally {
			pendingDeletePromises.delete(messageId);
		}
	})();
	pendingDeletePromises.set(messageId, deletePromise);
	return deletePromise;
}

interface ShowDeleteConfirmationOptions {
	message: MessageModel;
	onDelete?: () => void;
	showShiftBypassConfirmationTip?: boolean;
	suppressSafetyTeamReportToggle?: boolean;
}

export function showDeleteConfirmation(
	i18n: I18n,
	{
		message,
		onDelete,
		showShiftBypassConfirmationTip = false,
		suppressSafetyTeamReportToggle = false,
	}: ShowDeleteConfirmationOptions,
): void {
	const showSafetyTeamReportToggle =
		!suppressSafetyTeamReportToggle &&
		message.author.id !== Authentication.currentUserId &&
		Permission.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId});
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(DELETE_MESSAGE_DESCRIPTOR)}
				description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR)}
				message={message}
				primaryText={i18n._(DELETE_DESCRIPTOR)}
				primaryVariant="danger"
				toggleSwitchContent={
					showSafetyTeamReportToggle ? (
						<Switch
							value={false}
							onChange={() => {}}
							label={i18n._(ALSO_REPORT_TO_SAFETY_TEAM_DESCRIPTOR, {productName: PRODUCT_NAME})}
							compact
							data-flx="messaging.message-commands.show-delete-confirmation.switch"
						/>
					) : undefined
				}
				onPrimary={async (alsoReportToSafetyTeam) => {
					if (alsoReportToSafetyTeam) {
						try {
							await IARCommands.reportMessage(message.channelId, message.id, 'other');
						} catch (error) {
							logger.error('Failed to also-report message before deletion:', error);
						}
					}
					remove(message.channelId, message.id);
					onDelete?.();
				}}
				showShiftBypassConfirmationTip={showShiftBypassConfirmationTip}
				data-flx="messaging.message-commands.show-delete-confirmation.confirm-modal"
			/>
		)),
	);
}

export function deleteLocal(channelId: string, messageId: string): void {
	logger.debug(`Deleting message ${messageId} locally in channel ${channelId}`);
	Messages.handleMessageDelete({id: messageId, channelId});
}

export function revealMessage(channelId: string, messageId: string | null): void {
	logger.debug(`Revealing message ${messageId} in channel ${channelId}`);
	Messages.handleMessageReveal({channelId, messageId});
}

export function startReply(channelId: string, messageId: string, mentioning: boolean): void {
	logger.debug(`Starting reply to message ${messageId} in channel ${channelId}, mentioning=${mentioning}`);
	MessageReply.startReply(channelId, messageId, mentioning);
	ComponentDispatch.dispatch('FOCUS_TEXTAREA', {channelId});
	window.requestAnimationFrame(() => {
		ComponentDispatch.dispatch('FOCUS_TEXTAREA', {channelId});
	});
	window.setTimeout(() => {
		ComponentDispatch.dispatch('FOCUS_TEXTAREA', {channelId});
	}, 300);
}

export function stopReply(channelId: string): void {
	logger.debug(`Stopping reply in channel ${channelId}`);
	MessageReply.stopReply(channelId);
}

export function setReplyMentioning(channelId: string, mentioning: boolean): void {
	logger.debug(`Setting reply mentioning in channel ${channelId}: ${mentioning}`);
	MessageReply.setMentioning(channelId, mentioning);
}

export function startEdit(channelId: string, messageId: string, initialContent: string): void {
	logger.debug(`Starting edit for message ${messageId} in channel ${channelId}`);
	const draftContent = Accessibility.preserveEditDraft ? MessageEdit.getDraftContent(messageId) : null;
	const contentToUse = draftContent ?? initialContent;
	MessageEdit.startEditing(channelId, messageId, contentToUse);
}

export function stopEdit(channelId: string): void {
	logger.debug(`Stopping edit in channel ${channelId}`);
	MessageEdit.stopEditing(channelId);
}

export function startEditMobile(channelId: string, messageId: string): void {
	logger.debug(`Starting mobile edit for message ${messageId} in channel ${channelId}`);
	MessageEditMobile.startEditingMobile(channelId, messageId);
}

export function stopEditMobile(channelId: string): void {
	logger.debug(`Stopping mobile edit in channel ${channelId}`);
	MessageEditMobile.stopEditingMobile(channelId);
}

export function createOptimistic(channelId: string, message: WireMessage): void {
	logger.debug(`Creating optimistic message in channel ${channelId}`);
	Messages.handleIncomingMessage({channelId, message});
}

export function deleteOptimistic(channelId: string, messageId: string): void {
	logger.debug(`Deleting optimistic message ${messageId} in channel ${channelId}`);
	Messages.handleMessageDelete({channelId, id: messageId});
}

export function sendError(channelId: string, nonce: string): void {
	logger.debug(`Message send error for nonce ${nonce} in channel ${channelId}`);
	Messages.handleSendFailed({channelId, nonce});
}

export function retryLocal(channelId: string, messageId: string): void {
	logger.debug(`Retrying optimistic message ${messageId} in channel ${channelId}`);
	Messages.handleSendRetry({channelId, messageId});
}

export function editOptimistic(
	channelId: string,
	messageId: string,
	content: string,
): {originalContent: string; originalEditedTimestamp: string | null} | null {
	logger.debug(`Applying optimistic edit for message ${messageId} in channel ${channelId}`);
	return Messages.handleOptimisticEdit({channelId, messageId, content});
}

export function editRollback(
	channelId: string,
	messageId: string,
	originalContent: string,
	originalEditedTimestamp: string | null,
): void {
	logger.debug(`Rolling back edit for message ${messageId} in channel ${channelId}`);
	Messages.handleEditRollback({channelId, messageId, originalContent, originalEditedTimestamp});
}

export async function forward(
	channelIds: Array<string>,
	messageReference: ForwardMessageReference,
	optionalMessage?: string,
): Promise<boolean> {
	logger.debug(`Forwarding message ${messageReference.message_id} to ${channelIds.length} channels`);
	try {
		for (const channelId of channelIds) {
			const nonce = SnowflakeUtils.fromTimestamp(Date.now());
			const forwardedMessage = await send(channelId, {
				content: '',
				nonce,
				messageReference: {
					message_id: messageReference.message_id,
					channel_id: messageReference.channel_id,
					guild_id: messageReference.guild_id || undefined,
					attachment_ids: messageReference.attachment_ids,
					embed_indices: messageReference.embed_indices,
					type: 1,
				},
				flags: 1,
			});
			if (!forwardedMessage) {
				logger.warn(`Forward send failed in channel ${channelId}`);
				return false;
			}
			SlowmodeCommands.recordMessageSend(channelId);
			if (optionalMessage) {
				const commentNonce = SnowflakeUtils.fromTimestamp(Date.now() + 1);
				const commentMessage = await send(channelId, {
					content: optionalMessage,
					nonce: commentNonce,
				});
				if (!commentMessage) {
					logger.warn(`Forward comment send failed in channel ${channelId}`);
					return false;
				}
				SlowmodeCommands.recordMessageSend(channelId);
			}
		}
		logger.debug('Successfully forwarded message to all channels');
		return true;
	} catch (error) {
		logger.error('Failed to forward message:', error);
		throw error;
	}
}

function toggledSuppressEmbedsFlags(currentFlags: number): {isSuppressed: boolean; newFlags: number} {
	const isSuppressed = (currentFlags & MessageFlags.SUPPRESS_EMBEDS) === MessageFlags.SUPPRESS_EMBEDS;
	const newFlags = isSuppressed
		? currentFlags & ~MessageFlags.SUPPRESS_EMBEDS
		: currentFlags | MessageFlags.SUPPRESS_EMBEDS;
	return {isSuppressed, newFlags};
}

async function requestMessageFlagsPatch(channelId: string, messageId: string, flags: number): Promise<void> {
	await http.patch<WireMessage>(Endpoints.CHANNEL_MESSAGE(channelId, messageId), {
		body: {flags},
	});
}

export async function toggleSuppressEmbeds(channelId: string, messageId: string, currentFlags: number): Promise<void> {
	try {
		const {isSuppressed, newFlags} = toggledSuppressEmbedsFlags(currentFlags);
		logger.debug(`${isSuppressed ? 'Unsuppressing' : 'Suppressing'} embeds for message ${messageId}`);
		await requestMessageFlagsPatch(channelId, messageId, newFlags);
		logger.debug(`Successfully ${isSuppressed ? 'unsuppressed' : 'suppressed'} embeds for message ${messageId}`);
	} catch (error) {
		logger.error('Failed to toggle suppress embeds:', error);
		throw error;
	}
}

async function requestPersonalNotesPurge(channelId: string): Promise<number> {
	const response = await http.post<{deleted_count: number}>(Endpoints.CHANNEL_MESSAGES_PURGE(channelId), {
		body: {},
	});
	return response.body.deleted_count;
}

export async function purgePersonalNotes(channelId: string): Promise<{deletedCount: number}> {
	try {
		logger.debug(`Purging personal notes channel ${channelId}`);
		const deletedCount = await requestPersonalNotesPurge(channelId);
		logger.info(`Purged ${deletedCount} messages from personal notes ${channelId}`);
		return {deletedCount};
	} catch (error) {
		logger.error(`Failed to purge personal notes channel ${channelId}:`, error);
		throw error;
	}
}

async function requestAttachmentDelete(channelId: string, messageId: string, attachmentId: string): Promise<void> {
	await http.delete(Endpoints.CHANNEL_MESSAGE_ATTACHMENT(channelId, messageId, attachmentId));
}

export async function deleteAttachment(channelId: string, messageId: string, attachmentId: string): Promise<void> {
	try {
		logger.debug(`Deleting attachment ${attachmentId} from message ${messageId}`);
		await requestAttachmentDelete(channelId, messageId, attachmentId);
		logger.debug(`Successfully deleted attachment ${attachmentId} from message ${messageId}`);
	} catch (error) {
		logger.error('Failed to delete attachment:', error);
		throw error;
	}
}
